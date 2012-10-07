/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package object basis {
  type Any       = scala.Any
  type AnyRef    = scala.AnyRef
  type AnyVal    = scala.AnyVal
  type Nothing   = scala.Nothing
  type Singleton = scala.Singleton
  
  type Unit    = scala.Unit
  type Byte    = scala.Byte
  type Short   = scala.Short
  type Int     = scala.Int
  type Long    = scala.Long
  type Float   = scala.Float
  type Double  = scala.Double
  type Boolean = scala.Boolean
  
  type Function1[-T1, +R] = scala.Function1[T1, R]
  
  type PartialFunction[-T1, +R] = scala.PartialFunction[T1, R]
  
  type Option[+A] = scala.Option[A]
  val  Option     = scala.Option
  
  type Some[+A]   = scala.Some[A]
  val  Some       = scala.Some
  val  None       = scala.None
  
  type inline = scala.inline
  
  val ??? : Nothing = scala.Predef.???
  
  def traverse[A, U](xs: Enumerator[A])(f: A => U): scala.Unit =
    Enumerator.traverse[A, U](xs)(f)
}
