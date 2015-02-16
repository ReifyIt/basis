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
import scala.annotation._

sealed abstract class Path private[net]
  extends Equals
  with Family[Path]
  with LinearSeq[String]
  with UriPart {

  def isDefined: Boolean

  override def isEmpty: Boolean

  override def head: String

  override def tail: Path

  def :: (segment: String): Path

  def / (segment: String): Path = (new PathBuilder() ++= this += segment).state

  def / : Path = (new PathBuilder() ++= this += "/").state

  def removeDotSegments: Path = {
    var path = this
    val buffer = new PathBuilder()
    while (!path.isEmpty) {
      if (path.head.equals(".") || path.head.equals("..")) {
        path = path.tail
        if (!path.isEmpty) path = path.tail
      }
      else if (path.head.equals("/")) {
        val rest = path.tail
        if (!rest.isEmpty) {
          if (rest.head.equals(".")) {
            path = rest.tail
            if (path.isEmpty) path = Path.Slash
          }
          else if (rest.head.equals("..")) {
            path = rest.tail
            if (path.isEmpty) path = Path.Slash
            if (!buffer.isEmpty && !buffer.dropFoot().equals("/")) {
              if (!buffer.isEmpty) buffer.dropFoot()
            }
          }
          else {
            buffer.append(path.head)
            buffer.append(rest.head)
            path = rest.tail
          }
        }
        else {
          buffer.append(path.head)
          path = path.tail
        }
      }
      else {
        buffer.append(path.head)
        path = path.tail
      }
    }
    buffer.state
  }

  def writeUriString(builder: StringBuilder): Unit = writeUriString(this)(builder)
  @tailrec private[this] def writeUriString(path: Path)(builder: StringBuilder): Unit =
    if (!path.isEmpty) {
      if (path.isInstanceOf[Path.Slash]) builder.append('/')
      else Uri.writePathSegment(path.head)(builder)
      writeUriString(path.tail)(builder)
    }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Path]

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[Path] && equals(this, other.asInstanceOf[Path])
  @tailrec private[this] def equals(these: Path, those: Path): Boolean =
    if (these.isEmpty) those.isEmpty
    else if (those.isEmpty) these.isEmpty
    else these.head.equals(those.head) && equals(these.tail, those.tail)

  override def hashCode: Int = hash(MurmurHash3.seed[Path], this)
  @tailrec private[this] def hash(code: Int, path: Path): Int =
    if (path.isEmpty) MurmurHash3.mash(code)
    else hash(MurmurHash3.mix(code, path.head.hashCode), path.tail)

  override def toString: String = {
    val s = String.Builder~"Path"~'('~'"'
    writeUriString(s)
    (s~'"'~')').state
  }
}

object Path extends Uri.PathFactory {
  def empty: Path = Empty

  val slash: Path = Slash

  def / : Path = Slash

  def / (path: Path): Path = new Slash(path)

  def / (segment: String): Path = new Slash(new Segment(segment, Empty))

  override def SegmentSlash: String = "/"

  override def Segment(segment: String): String = segment

  implicit def apply(path: String): Path = {
    val input = new UString(path).iterator
    var result = Uri.PathParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid path character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Path] = part match {
    case path: Path => Bind(path)
    case _ => Trap
  }

  implicit override def Builder: Uri.PathBuilder = new PathBuilder()

  override def toString: String = "Path"


  final class Segment private[net] (
      override val head: String,
      private[this] var rest: SlashOrEmpty)
    extends Path {

    override def isDefined: Boolean = true

    override def isEmpty: Boolean = false

    override def tail: SlashOrEmpty = rest

    private[net] def tail_=(tail: SlashOrEmpty): Unit = rest = tail

    override def :: (segment: String): Path =
      if (segment == "/") new Slash(this)
      else new Segment(segment, new Slash(this))
  }

  sealed abstract class SlashOrEmpty private[net] extends Path {
    override def :: (segment: String): Path = new Segment(segment, this)
  }

  sealed class Slash private[net] (private[this] var rest: Path) extends SlashOrEmpty {
    override def isDefined: Boolean = true

    override def isEmpty: Boolean = false

    override def head: String = "/"

    override def tail: Path = rest

    private[net] def tail_=(tail: Path): Unit = rest = tail
  }

  object Slash extends Slash(Empty) {
    private[net] override def tail_=(tail: Path): Unit = throw new UnsupportedOperationException()
  }

  object Empty extends SlashOrEmpty {
    override def isDefined: Boolean = false

    override def isEmpty: Boolean = true

    override def head: String = throw new NoSuchElementException("head of empty path")

    override def tail: Path = throw new UnsupportedOperationException("tail of empty path")

    override def toString: String = (String.Builder~"Path"~'.'~"empty").state
  }
}

private[net] final class PathBuilder(
    private[this] var first: Path,
    private[this] var last: Path,
    private[this] var size: Int,
    private[this] var aliased: Int)
  extends Uri.PathBuilder {

  def this() = this(Path.Empty, null, 0, 0)

  def isEmpty: Boolean = size == 0

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

  override def appendAll(segments: Traverser[String]): Unit = {
    if (segments.isInstanceOf[Path.Empty.type]) ()
    else if (segments.isInstanceOf[Path]) {
      var xs = segments.asInstanceOf[Path]
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
    else super.appendAll(segments)
  }

  override def appendPath(path: Path) = appendAll(path)

  def dropFoot(): String = {
    if (size == 0) throw new UnsupportedOperationException()
    if (size == 1) {
      val x0 = first
      val x = x0.head
      first = x0.tail
      if (x0.tail.isEmpty) last = null
      size -= 1
      if (aliased > 0) aliased -= 1
      x
    }
    else {
      val xi = dealias(size - 2)
      val x = xi.tail.head
      if (xi.isInstanceOf[Path.Slash]) xi.asInstanceOf[Path.Slash].tail = Path.Empty
      else xi.asInstanceOf[Path.Segment].tail = Path.Empty
      last = xi
      size -= 1
      aliased -= 1
      x
    }
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
