//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import basis.collections._
import basis.text._

trait PartialOrder[T] {
  def compare(x: T, y: T): PartialComparison
}

trait Order[T] extends PartialOrder[T] {
  override def compare(x: T, y: T): Comparison
}

object Order {
  def apply[T](implicit order: Order[T]): order.type = order

  def by[A, B](f: A => B)(implicit order: Order[B]): Order[A] = new OrderBy[A, B](f)

  implicit lazy val Byte: Order[Byte]       = new ByteOrder
  implicit lazy val Short: Order[Short]     = new ShortOrder
  implicit lazy val Int: Order[Int]         = new IntOrder
  implicit lazy val Long: Order[Long]       = new LongOrder
  implicit lazy val Float: Order[Float]     = new FloatOrder
  implicit lazy val Double: Order[Double]   = new DoubleOrder
  implicit lazy val Char: Order[Char]       = new CharOrder
  implicit lazy val Boolean: Order[Boolean] = new BooleanOrder
  implicit lazy val Unit: Order[Unit]       = new UnitOrder
  implicit lazy val String: Order[String]   = new StringOrder

  implicit def Maybe[T : Order]: Order[Maybe[T]] = new MaybeOrder[T]
  implicit def Container[T : Order]: Order[Container[T]] = new ContainerOrder[T]

  implicit def Tuple2[T1 : Order, T2 : Order]: Order[(T1, T2)] = new Tuple2Order[T1, T2]
  implicit def Tuple3[T1 : Order, T2 : Order, T3 : Order]: Order[(T1, T2, T3)] = new Tuple3Order[T1, T2, T3]
  implicit def Tuple4[T1 : Order, T2 : Order, T3 : Order, T4 : Order]: Order[(T1, T2, T3, T4)] = new Tuple4Order[T1, T2, T3, T4]

  implicit def comparator[T](implicit order: Order[T]): java.util.Comparator[T] = new OrderComparator[T]
}

private[basis] final class OrderBy[A, B](f: A => B)(implicit order: Order[B]) extends Order[A] {
  override def compare(x: A, y: A): Comparison = order.compare(f(x), f(y))

  override def toString: String = (String.Builder~"Order"~'.'~"by"~'('~>f~')'~'('~>order~')').state
}

private[basis] final class ByteOrder extends Order[Byte] {
  override def compare(x: Byte, y: Byte): Comparison = Comparison(x.toInt - y.toInt)

  override def toString: String = (String.Builder~"Order"~'.'~"Byte").state
}

private[basis] final class ShortOrder extends Order[Short] {
  override def compare(x: Short, y: Short): Comparison = Comparison(x.toInt - y.toInt)

  override def toString: String = (String.Builder~"Order"~'.'~"Short").state
}

private[basis] final class IntOrder extends Order[Int] {
  override def compare(x: Int, y: Int): Comparison = {
    if (x < y) Precedes
    else if (x > y) Succeeds
    else Equivalent
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Int").state
}

private[basis] final class LongOrder extends Order[Long] {
  override def compare(x: Long, y: Long): Comparison = {
    if (x < y) Precedes
    else if (x > y) Succeeds
    else Equivalent
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Long").state
}

private[basis] final class FloatOrder extends Order[Float] {
  override def compare(x: Float, y: Float): Comparison = Comparison(java.lang.Float.compare(x, y))

  override def toString: String = (String.Builder~"Order"~'.'~"Float").state
}

private[basis] final class DoubleOrder extends Order[Double] {
  override def compare(x: Double, y: Double): Comparison = Comparison(java.lang.Double.compare(x, y))

  override def toString: String = (String.Builder~"Order"~'.'~"Double").state
}

private[basis] final class CharOrder extends Order[Char] {
  override def compare(x: Char, y: Char): Comparison = Comparison(x.toInt - y.toInt)

  override def toString: String = (String.Builder~"Order"~'.'~"Char").state
}

private[basis] final class BooleanOrder extends Order[Boolean] {
  override def compare(x: Boolean, y: Boolean): Comparison = {
    if (!x && y) Precedes
    else if (x && !y) Succeeds
    else Equivalent
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Boolean").state
}

private[basis] final class UnitOrder extends Order[Unit] {
  override def compare(x: Unit, y: Unit): Comparison = Equivalent

  override def toString: String = (String.Builder~"Order"~'.'~"Unit").state
}

private[basis] final class StringOrder extends Order[String] {
  override def compare(x: String, y: String): Comparison = Comparison(x compareTo y)

  override def toString: String = (String.Builder~"Order"~'.'~"String").state
}

private[basis] final class MaybeOrder[T](implicit T: Order[T]) extends Order[Maybe[T]] {
  override def compare(x: Maybe[T], y: Maybe[T]): Comparison = {
    if (x.canBind && y.canBind) T.compare(x.bind, y.bind)
    else if (y.canBind) Precedes
    else if (x.canBind) Succeeds
    else Equivalent
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Maybe"~'('~>T~')').state
}

private[basis] final class ContainerOrder[T](implicit T: Order[T]) extends Order[Container[T]] {
  override def compare(these: Container[T], those: Container[T]): Comparison = {
    val xs = these.iterator
    val ys = those.iterator
    while (!xs.isEmpty && !ys.isEmpty) {
      val cmp = T.compare(xs.head, ys.head)
      if (!cmp.isEquivalent) return cmp
      xs.step()
      ys.step()
    }
    Order.Boolean.compare(!xs.isEmpty, !ys.isEmpty)
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Container"~'('~>T~')').state
}

private[basis] final class Tuple2Order[T1, T2](implicit T1: Order[T1], T2: Order[T2]) extends Order[(T1, T2)] {
  override def compare(x: (T1, T2), y: (T1, T2)): Comparison = {
    val cmp = T1.compare(x._1, y._1)
    if (!cmp.isEquivalent) return cmp
    T2.compare(x._2, y._2)
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Tuple2"~'('~>T1~", "~>T2~')').state
}

private[basis] final class Tuple3Order[T1, T2, T3](implicit T1: Order[T1], T2: Order[T2], T3: Order[T3]) extends Order[(T1, T2, T3)] {
  override def compare(x: (T1, T2, T3), y: (T1, T2, T3)): Comparison = {
    var cmp = T1.compare(x._1, y._1)
    if (!cmp.isEquivalent) return cmp
    cmp = T2.compare(x._2, y._2)
    if (!cmp.isEquivalent) return cmp
    T3.compare(x._3, y._3)
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Tuple3"~'('~>T1~", "~>T2~", "~>T3~')').state
}

private[basis] final class Tuple4Order[T1, T2, T3, T4](implicit T1: Order[T1], T2: Order[T2], T3: Order[T3], T4: Order[T4]) extends Order[(T1, T2, T3, T4)] {
  override def compare(x: (T1, T2, T3, T4), y: (T1, T2, T3, T4)): Comparison = {
    var cmp = T1.compare(x._1, y._1)
    if (!cmp.isEquivalent) return cmp
    cmp = T2.compare(x._2, y._2)
    if (!cmp.isEquivalent) return cmp
    cmp = T3.compare(x._3, y._3)
    if (!cmp.isEquivalent) return cmp
    T4.compare(x._4, y._4)
  }

  override def toString: String = (String.Builder~"Order"~'.'~"Tuple4"~'('~>T1~", "~>T2~", "~>T3~", "~>T4~')').state
}

private[basis] final class OrderComparator[T](implicit order: Order[T]) extends java.util.Comparator[T] {
  override def compare(x: T, y: T): Int = order.compare(x, y).toInt

  override def toString: String = (String.Builder~"Order"~'.'~"comparator"~'('~>order~')').state
}
