//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

/** Core basis types.
  *
  * @contentDiagram hideNodes "basis.Family"
  */
package object basis {
  type Maybe[+A] = A Else Any

  def Maybe[A](value: A): A Else Nothing = if (value != null) Bind(value) else Trap

  type Try[+A] = A Else Throwable

  def Try[A](expr: => A): Try[A] = macro BasisMacros.Try[A]

  type Truth = Boolean Else Any

  val True: Bind[Boolean] = new BindBoolean(true)

  val False: Bind[Boolean] = new BindBoolean(false)

  implicit def ElseToOps[A, B](self: A Else B): ElseOps[A, B]           = macro BasisMacros.ElseToOps[A, B]
  implicit def MaybeToOps[A](self: A Else Nothing): ElseOps[A, Nothing] = macro BasisMacros.ElseToOps[A, Nothing]
  implicit def TruthToOps(self: Truth): TruthOps                        = macro BasisMacros.TruthToOps
}
