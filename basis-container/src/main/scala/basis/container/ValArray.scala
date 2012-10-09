/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.data._

final class ValArray[A](val data: Mem)(implicit typeA: ValType[A]) extends Array[A] {
  override def length: Int = (data.size / typeA.size.toLong).toInt
  
  override def apply(index: Int): A =
    typeA.load(data, typeA.size.toLong * index.toLong)
  
  def update(index: Int, value: A): Unit =
    typeA.store(data, typeA.size.toLong * index.toLong, value)
  
  def copy(length: Int = this.length): ValArray[A] =
    new ValArray[A](data.copy(typeA.size.toLong * length.toLong))
}

object ValArray {
  def apply[A](length: Int)(implicit typeA: ValType[A]): ValArray[A] =
    new ValArray[A](Mem.alloc[A](length))
}
