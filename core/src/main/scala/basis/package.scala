//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

/** Core basis types.
  *
  * @contentDiagram hideNodes "basis.Family"
  */
package object basis {
  /** A bound value or else a trapped value.
    * @template */
  type Maybe[+A] = A Else Any

  object Maybe {
    def empty[A]: Maybe[A] = Trap
    def apply[A](value: A): A Else Nothing = macro BasisMacros.Maybe[A]
    override def toString: String = "Maybe"
  }

  /** A bound value or else a trapped throwable.
    * @template */
  type Try[+A] = A Else Throwable

  object Try {
    def empty[A]: Try[A] = Trap
    def apply[A](expr: => A): Try[A] = macro BasisMacros.Try[A]
    override def toString: String = "Try"
  }

  /** A bound boolean or else a trapped value.
    * @template */
  type Truth = Boolean Else Any

  val True: Bind[Boolean] = new BindBoolean(true)

  val False: Bind[Boolean] = new BindBoolean(false)

  implicit def ElseToOps[A, B](self: A Else B): ElseOps[A, B]           = macro BasisMacros.ElseToOps[A, B]
  implicit def MaybeToOps[A](self: A Else Nothing): ElseOps[A, Nothing] = macro BasisMacros.ElseToOps[A, Nothing]
  implicit def TruthToOps(self: Truth): TruthOps                        = macro BasisMacros.TruthToOps
}
