//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import basis._
import scala.reflect._
import scala.reflect.macros._

trait ArrayFactory[+CC[_]] {
  def empty[A](implicit A: ClassTag[A]): CC[A]           = Builder[A].state
  def apply[A](elems: A*): CC[A]                         = macro ArrayFactoryMacros.apply[CC, A]
  def fill[A](count: Int)(elem: => A): CC[A]             = macro ArrayFactoryMacros.fill[CC, A]
  def tabulate[A](count: Int)(f: Int => A): CC[A]        = macro ArrayFactoryMacros.tabulate[CC, A]
  def iterate[A](start: A, count: Int)(f: A => A): CC[A] = macro ArrayFactoryMacros.iterate[CC, A]

  def coerce[A](elems: Traverser[A]): CC[A] = {
    val builder = Builder[A](ClassTag.AnyRef.asInstanceOf[ClassTag[A]])
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from[A](elems: Traverser[A])(implicit A: ClassTag[A]): CC[A] = {
    val builder = Builder[A]
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from[A](elems: scala.collection.TraversableOnce[A])(implicit A: ClassTag[A]): CC[A] = {
    val builder = Builder[A]
    elems.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder[A](implicit A: ClassTag[A]): Builder[A] with State[CC[A]]
}

private[generic] class ArrayFactoryMacros(override val c: blackbox.Context { type PrefixType <: ArrayFactory[CC] forSome { type CC[_] } }) extends FactoryMacros(c)
