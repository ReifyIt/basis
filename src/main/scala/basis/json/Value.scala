/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import basis.collection._
import basis.encoding._

sealed abstract class Value

final class Object(
    names: scala.Array[utf8.String],
    values: scala.Array[Value])
  extends Value with Map[utf8.String, Value] {
  
  override type Kind = Object
  
  override def iterator: Iterator[(utf8.String, Value)] = ??? // new ForwardIterator(0)
  
  override def get(name: utf8.String): Option[Value] = {
    var i = 0
    while (i < names.length && !name.equals(names(i))) i += 1
    if (i < names.length) Some(values(i)) else None
  }
  /*
  private final class ForwardIterator(private[this] var index: Int) extends Iterator[(utf8.String, Value)] {
    override def hasNext: scala.Boolean = index < names.length
    override def next: (utf8.String, Value) = {
      if (!hasNext) Iterator.Empty.next()
      val name = names(index)
      val value = values(index)
      index += 1
      (name, value)
    }
  }
  */
}

object Object {
  
}


final class Array(values: scala.Array[Value]) extends Value with IndexedSeq[Value] {
  override type Kind = Array
  
  override def length: Int = values.length
  
  override def apply(index: Int): Value = values(index)
}

object Array {
  
}


final class String(val value: utf8.String) extends Value

object String {
  
}


sealed abstract class Number extends Value


final class Integer(value: Long) extends Number

object Integer {
  
}


final class Decimal(value: Double) extends Number

object Decimal {
  
}


sealed abstract class Boolean private[json] (val value: scala.Boolean) extends Value

object Boolean {
  
}

object True extends Boolean(true)

object False extends Boolean(false)


object Null extends Value
