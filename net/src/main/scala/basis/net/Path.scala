//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.collections._
import basis.text._
import basis.util._

sealed abstract class Path private[net] extends Equals with Family[Path] with LinearSeq[String] {
  override def isEmpty: Boolean

  override def head: String

  override def tail: Path

  def :: (segment: String): Path

  def / (segment: String): Path = (Path.Builder ++= this += segment).state

  def / : Path = (Path.Builder ++= this += "/").state

  def writeUriString(builder: StringBuilder): Unit = {
    var path = this
    while (!path.isEmpty) {
      if (path.isInstanceOf[Path.Slash]) builder.append('/')
      else {
        val segment = path.head
        var i = 0
        val n = segment.length
        while (i < n) {
          val c = segment.codePointAt(i)
          if (Uri.isUnreservedChar(c) || c == ':' || c == '@') builder.append(c)
          else Uri.writeEncoded(c)
          i = segment.offsetByCodePoints(i, 1)
        }
      }
      path = path.tail
    }
  }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Path]

  override def toString: String = {
    val s = String.Builder~"Path"~'('~'"'
    writeUriString(s)
    (s~'"'~')').state
  }
}

object Path {
  def empty: Path = Empty

  def apply(path: String): Path = {
    val result = Uri.Parser.PathParser.run(new UString(path).iterator)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  val slash: Path = Slash

  def / : Path = Slash

  def / (path: Path): Path = new Slash(path)

  def / (segment: String): Path = new Slash(new Segment(segment, Empty))

  implicit def Builder: Builder[String] with State[Path] = new PathBuilder()

  override def toString: String = "Path"


  final class Segment private[net] (
      override val head: String,
      private[this] var rest: SlashOrEmpty)
    extends Path {

    override def isEmpty: Boolean = false

    override def tail: SlashOrEmpty = rest

    private[net] def tail_=(tail: SlashOrEmpty): Unit = rest = tail

    override def :: (segment: String): Path = new Segment(segment, new Slash(this))

    override def equals(other: Any): Boolean = other match {
      case that: Segment => head.equals(that.head) && tail.equals(that.tail)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Segment], head.hashCode), tail.hashCode))
    }
  }

  sealed abstract class SlashOrEmpty private[net] extends Path {
    override def :: (segment: String): Path = new Segment(segment, this)
  }

  sealed class Slash private[net] (private[this] var rest: Path) extends SlashOrEmpty {
    override def isEmpty: Boolean = false

    override def head: String = "/"

    override def tail: Path = rest

    private[net] def tail_=(tail: Path): Unit = rest = tail

    override def equals(other: Any): Boolean = other match {
      case that: Slash => tail.equals(that.tail)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Slash], tail.hashCode))
    }
  }

  object Slash extends Slash(Empty) {
    private[net] override def tail_=(tail: Path): Unit = throw new AssertionError()

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef])

    override def hashCode: Int = MurmurHash3.seed[Slash.type]
  }

  object Empty extends SlashOrEmpty {
    override def isEmpty: Boolean = true

    override def head: String = throw new NoSuchElementException("head of empty path")

    override def tail: Path = throw new UnsupportedOperationException("tail of empty path")

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef])

    override def hashCode: Int = MurmurHash3.seed[Empty.type]

    override def toString: String = (String.Builder~"Path"~'.'~"empty").state
  }
}

private[net] final class PathBuilder(
    private[this] var first: Path,
    private[this] var last: Path,
    private[this] var size: Int,
    private[this] var aliased: Int)
  extends Builder[String] with State[Path] {

  def this() = this(Path.Empty, null, 0, 0)

  override def append(segment: String): Unit = {
    if (segment == "/") {
      val xn = new Path.Slash(Path.Empty)
      if (size == 0) first = xn
      else {
        val next = dealias(size - 1)
        if (next.isInstanceOf[Path.Slash]) next.asInstanceOf[Path.Slash].tail = xn
        else next.asInstanceOf[Path.Segment].tail = xn
      }
      last = xn
    }
    else {
      val xn = new Path.Segment(segment, Path.Empty)
      if (size == 0) first = xn
      else {
        val next = dealias(size - 1)
        if (next.isInstanceOf[Path.Slash]) next.asInstanceOf[Path.Slash].tail = xn
        else next.asInstanceOf[Path.Segment].tail = new Path.Slash(xn)
      }
      last = xn
    }
    size += 1
    aliased += 1
  }

  override def appendAll(elems: Traverser[String]): Unit = {
    if (elems.isInstanceOf[Path.Empty.type]) ()
    else if (elems.isInstanceOf[Path]) {
      var xs = elems.asInstanceOf[Path]
      if (size == 0) first = xs
      else {
        val next = dealias(size - 1)
        if (next.isInstanceOf[Path.Slash]) next.asInstanceOf[Path.Slash].tail = xs
        else next.asInstanceOf[Path.Segment].tail = new Path.Slash(xs)
      }
      size += 1
      while (!xs.tail.isEmpty) {
        xs = xs.tail
        size += 1
      }
      last = xs
    }
    else super.appendAll(elems)
  }

  override def clear(): Unit = {
    first = Path.Empty
    last = null
    size = 0
    aliased = 0
  }

  override def state: Path = {
    aliased = 0
    first
  }

  private[this] def dealias(n: Int): Path = {
    var i = 0
    var xi = null: Path
    var xs = first
    if (aliased <= n) {
      while (i < aliased) {
        xi = xs
        xs = xs.tail
        i += 1
      }
      while (i <= n) {
        if (xs.isInstanceOf[Path.Slash]) {
          val xn = new Path.Slash(xs.tail)
          if (i == 0) first = xn
          else if (xi.isInstanceOf[Path.Slash]) xi.asInstanceOf[Path.Slash].tail = xn
          else xi.asInstanceOf[Path.Segment].tail = xn
          xi = xn
        }
        else {
          val xn = new Path.Segment(xs.head, xs.asInstanceOf[Path.Segment].tail)
          if (i == 0) first = xn
          else xi.asInstanceOf[Path.Slash].tail = xn
          xi = xn
        }
        xs = xs.tail
        i += 1
      }
      if (i == size) last = xi
      aliased = i
    }
    else if (n == 0) xi = first
    else if (n == size - 1) xi = last
    else while (i <= n) {
      xi = xs
      xs = xs.tail
      i += 1
    }
    xi
  }

  override def toString: String = (String.Builder~"Path"~'.'~"Builder").state
}
