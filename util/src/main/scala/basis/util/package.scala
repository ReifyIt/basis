//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

/** General utility functions. */
package object util extends basis.util.Types {
  type Maybe[+A] = A Else Any

  def Maybe[A](value: A): A Else Nothing =
    if (value != null) Bind(value) else Trap

  type Try[+A] = A Else Throwable

  def Try[A](expr: => A): Try[A] =
    macro FuseMacrosStatics.Try[A]

  type Truth = Boolean Else Any

  val True: Bind[Boolean] = new BindBoolean(true)

  val False: Bind[Boolean] = new BindBoolean(false)

  /** The default breakable control-flow context. */
  val begin: Begin = new Begin

  implicit def ElseToOps[A, B](self: A Else B): ElseOps[A, B] =
    macro ElseMacrosStatics.ElseToOps[A, B]

  implicit def MaybeToOps[A](self: A Else Nothing): ElseOps[A, Nothing] =
    macro ElseMacrosStatics.ElseToOps[A, Nothing]

  implicit def TruthToOps(self: Truth): TruthOps =
    macro TruthOps.TruthToOps

  implicit def ArrowToOps[A](left: A): ArrowOps[A] = macro ArrowMacros.ArrowToOps[A]
  implicit def IntToOps(a: Int): IntOps            = macro IntMacros.IntToOps
  implicit def LongToOps(a: Long): LongOps         = macro LongMacros.LongToOps
  implicit def FloatToOps(x: Float): FloatOps      = macro FloatMacros.FloatToOps
  implicit def DoubleToOps(x: Double): DoubleOps   = macro DoubleMacros.DoubleToOps

  private[basis] implicit def StringBuilderToShower(builder: java.lang.StringBuilder): StringShower =
    macro StringShower.StringBuilderToShower
}
