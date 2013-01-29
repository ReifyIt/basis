/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Imperative control-flow and conditional bindings.
  * 
  * @groupname  Imperative    Imperative Control-Flow
  * @groupprio  Imperative    1
  * @groupname  Conditional   Conditional Bindings
  * @groupprio  Conditional   2
  */
package object control {
  /** A conditional binding with any alternative.
    * [[ElseOps]] provides standard operations available to all `Maybe` values.
    *
    * @template
    * @group Conditional
    * 
    * @groupprio  Determining   1
    * @groupprio  Binding       2
    */
  type Maybe[+A] = A Else Any
  
  /** Binds a non-`null` value, or returns the unit [[Trap$ Trap]] for `null`.
    * @group Conditional */
  def Maybe[A](value: A): Maybe[A] = if (value != null) Bind(value) else Trap
  
  /** A nondeterministic `Boolean`; either `True`, `False`, or a [[Trap]].
    * [[ElseOps]] provides standard operations available to all `Truth` values.
    * 
    * @template
    * @group Conditional
    * 
    * @groupprio  Determining   1
    * @groupprio  Binding       2
    */
  type Truth = Boolean Else Any
  
  /** The `true` value of the [[Truth]].
    * @group Conditional */
  val True: Bind[Boolean] = new BindBoolean(true)
  
  /** The `false` value of the [[Truth]].
    * @group Conditional */
  val False: Bind[Boolean] = new BindBoolean(false)
  
  /** A conditional binding with an exceptional alternative.
    * [[ElseOps]] provides standard operations available to all `Try` values.
    * 
    * @template
    * @group Conditional
    * 
    * @groupprio  Determining   1
    * @groupprio  Binding       2
    */
  type Try[+A] = A Else Throwable
  
  /** Binds an expression, or traps a non-fatal exception.
    * @group Conditional */
  def Try[A](expr: => A): Try[A] = macro FuseMacros.Try[A]
  
  /** Implicitly adds standard operations to [[Else]] values.
    * @group Conditional */
  implicit def ElseOps[A, B](self: A Else B): ElseOps[A, B] =
    macro ElseMacros.ElseOps[A, B]
  
  /** Implicitly adds standard operations to [[Maybe]] values.
    * @group Conditional */
  implicit def MaybeOps[A](self: Maybe[A]): ElseOps[A, Any] =
    macro ElseMacros.ElseOps[A, Any]
  
  /** Implicitly adds standard operations to [[Truth]] values.
    * @group Conditional */
  implicit def TruthOps(self: Truth): TruthOps =
    macro TruthMacros.TruthOps
}
