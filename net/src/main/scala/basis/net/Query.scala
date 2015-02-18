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

sealed abstract class Query private[net]
  extends Equals
  with Family[Query]
  with LinearSeq[(String, String)]
  with UriPart {

  def isDefined: Boolean

  override def isEmpty: Boolean

  def key: String

  def value: String

  override def head: (String, String)

  override def tail: Query

  def +: (key: String, value: String): Query =
    new Query.Param(key, value, this)

  def :+ (key: String, value: String): Query =
    (new QueryBuilder() ++= this += (key, value)).state

  def ++ (that: Query): Query =
    (new QueryBuilder() ++= this ++= that).state

  def part: String

  def writeUriString(builder: StringBuilder): Unit

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Query]

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[Query] && equals(this, other.asInstanceOf[Query])
  @tailrec private[this] def equals(these: Query, those: Query): Boolean =
    if (these.isEmpty) those.isEmpty
    else if (those.isEmpty) these.isEmpty
    else these.key.equals(those.key) && these.value.equals(those.value) && equals(these.tail, those.tail)

  override def hashCode: Int = hash(MurmurHash3.seed[Query], this)
  @tailrec private[this] def hash(code: Int, query: Query): Int =
    if (query.isEmpty) MurmurHash3.mash(code)
    else hash(MurmurHash3.mix(MurmurHash3.mix(code, query.key.hashCode), query.value.hashCode), query.tail)

  override def toString: String = {
    val s = String.Builder~"Query"~'('~'"'
    writeUriString(s)
    (s~'"'~')').state
  }
}

object Query extends Uri.QueryFactory {
  override val Undefined: Query = new Undefined()

  override def Part(query: String): Query = new Part(query)

  override def Param(key: String, value: String): (String, String) = (key, value)

  implicit def apply(query: String): Query = {
    val input = new UString(query).iterator
    var result = Uri.QueryParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid query character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Query] = part match {
    case query: Query => Bind(query)
    case _ => Trap
  }

  implicit override def Builder: Builder[(String, String)] with State[Query] = new QueryBuilder()

  def +: (key: String, value: String): Query =
    new Query.Param(key, value, Undefined)

  def :+ (key: String, value: String): Query =
    new Query.Param(key, value, Undefined)

  override def toString: String = "Query"


  private[net] final class Param(
      override val key: String,
      override val value: String,
      private[this] var rest: Query)
    extends Query {

    override def isDefined: Boolean = true

    override def isEmpty: Boolean = false

    override def head: (String, String) = (key, value)

    override def tail: Query = rest

    private[net] def tail_=(tail: Query): Unit = rest = tail

    override def part: String = {
      val builder = String.Builder
      builder.append(key)
      builder.append('=')
      builder.append(value)
      writePart(tail)(builder)
      builder.state
    }
    @tailrec private[this] def writePart(query: Query)(builder: StringBuilder): Unit =
      if (!query.isEmpty) {
        builder.append('&')
        builder.append(key)
        builder.append('=')
        builder.append(value)
        writePart(query.tail)(builder)
      }

    override def writeUriString(builder: StringBuilder): Unit = {
      Uri.writeParam(key)(builder)
      builder.append('=')
      Uri.writeParam(value)(builder)
      writeUriString(tail)(builder)
    }
    @tailrec private[this] def writeUriString(query: Query)(builder: StringBuilder): Unit =
      if (!query.isEmpty) {
        builder.append('&')
        Uri.writeParam(query.key)(builder)
        builder.append('=')
        Uri.writeParam(query.value)(builder)
        writeUriString(query.tail)(builder)
      }
  }

  private[net] final class Part(override val part: String) extends Query {
    override def isDefined: Boolean = true

    override def isEmpty: Boolean = false

    override def key: String = ""

    override def value: String = part

    override def head: (String, String) = (key, part)

    override def tail: Query = Undefined

    override def writeUriString(builder: StringBuilder): Unit =  Uri.writeQuery(part)(builder)
  }

  private[net] final class Undefined extends Query {
    override def isDefined: Boolean = false

    override def isEmpty: Boolean = true

    override def key: String = throw new NoSuchElementException("key of undefined query")

    override def value: String = throw new NoSuchElementException("value of undefined query")

    override def head: (String, String) = throw new NoSuchElementException("head of undefined query")

    override def tail: Query = throw new NoSuchElementException("tail of undefined query")

    override def part: String = ""

    override def writeUriString(builder: StringBuilder): Unit = ()

    override def toUriString: String = ""

    override def toString: String = (String.Builder~"Query"~'.'~"Undefined").state
  }
}

private[net] final class QueryBuilder(
    private[this] var first: Query,
    private[this] var last: Query.Param,
    private[this] var size: Int,
    private[this] var aliased: Int)
  extends Builder[(String, String)] with State[Query] {

  def this() = this(Query.Undefined, null, 0, 0)

  def += (key: String, value: String): this.type = {
    append(key, value)
    this
  }

  def append(key: String, value: String): Unit = {
    val xn = new Query.Param(key, value, Query.Undefined)
    if (size == 0) first = xn
    else dealias(size - 1).tail = xn
    last = xn
    size += 1
    aliased += 1
  }

  override def append(param: (String, String)): Unit = {
    val xn = new Query.Param(param._1, param._2, Query.Undefined)
    if (size == 0) first = xn
    else dealias(size - 1).tail = xn
    last = xn
    size += 1
    aliased += 1
  }

  override def appendAll(params: Traverser[(String, String)]): Unit = {
    if (params.isInstanceOf[Query.Undefined]) ()
    else if (params.isInstanceOf[Query.Param]) {
      var xs = params.asInstanceOf[Query.Param]
      if (size == 0) first = xs
      else dealias(size - 1).tail = xs
      size += 1
      while (!xs.tail.isEmpty) {
        xs = xs.tail.asInstanceOf[Query.Param]
        size += 1
      }
      last = xs
    }
    else super.appendAll(params)
  }

  override def clear(): Unit = {
    first = Query.Undefined
    last = null
    size = 0
    aliased = 0
  }

  override def state: Query = {
    aliased = 0
    first
  }

  private[this] def dealias(n: Int): Query.Param = {
    var i = 0
    var xi = null: Query.Param
    var xs = first
    if (aliased <= n) {
      while (i < aliased) {
        xi = xs.asInstanceOf[Query.Param]
        xs = xs.tail
        i += 1
      }
      while (i <= n) {
        val xn = new Query.Param(xs.key, xs.value, xs.tail)
        if (i == 0) first = xn
        else xi.tail = xn
        xi = xn
        xs = xs.tail
        i += 1
      }
      if (i == size) last = xi
      aliased = i
    }
    else if (n == 0) xi = first.asInstanceOf[Query.Param]
    else if (n == size - 1) xi = last
    else while (i <= n) {
      xi = xs.asInstanceOf[Query.Param]
      xs = xs.tail
      i += 1
    }
    xi
  }

  override def toString: String = (String.Builder~"Query"~'.'~"Builder").state
}
