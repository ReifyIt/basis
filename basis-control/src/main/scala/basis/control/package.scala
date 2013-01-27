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
  /** Either a [[Free]] value or an arbitrary [[Trap]].
    * @group Conditional
    * @template */
  type Maybe[+A] = A Else Nothing
  
  /** A factory for [[Maybe]] bindings.
    * @group Conditional */
  object Maybe {
    /** Returns a `Free` binding with a non-`null` value,
      * or a `Trap` for a `null` value. */
    def apply[A](value: A): Maybe[A] = if (value != null) Free(value) else Trap
    
    /** Extracts a `Maybe` binding as an `Option`. */
    def unapply[A](maybe: Maybe[A]): Option[A] = MaybeToOption(maybe)
    
    /** Returns `Some` with a `Free` value, or `None` for a `Trap`. */
    implicit def MaybeToOption[A](maybe: Maybe[A]): Option[A] =
      if (maybe.canBind) Some(maybe.bind) else None
    
    /** Returns `Free` with `Some` value, or a `Trap` for `None`. */
    implicit def OptionToMaybe[A](option: Option[A]): Maybe[A] =
      if (option.isDefined) Free(option.get) else Trap
  }
  
  /** Either a [[Free]] value or a [[Trap trapped]] exception.
    * @group Conditional
    * @template */
  type Try[+A] = A Else Throwable
  
  /** A factory for [[Try]] bindings.
    * @group Conditional */
  object Try {
    /** Returns a `Free` binding for an expression,
      * or a `Trap` with a caught, non-fatal exception. */
    def apply[A](expr: => A): Try[A] = macro FuseMacros.Try[A]
  }
  
  /** A nondeterministic `Boolean` value; either `True`, `False`, or an arbitrary `Trap`.
    * @group Conditional
    * @template */
  type Truth = Boolean Else Any
  
  /** Returns the `true` [[Truth]] value.
    * @group Conditional */
  val True: Free[Boolean] = new FreeBoolean(true)
  
  /** Returns the `false` [[Truth]] value.
    * @group Conditional */
  val False: Free[Boolean] = new FreeBoolean(false)
  
  /** Implicitly adds operations to `Else` values.
    * @group Conditional */
  implicit def ElseOps[A, B](self: A Else B): ElseOps[A, B] =
    macro ElseMacros.ElseOps[A, B]
  
  /** Implicitly adds operations to `Maybe` values.
    * @group Conditional */
  implicit def MaybeOps[A](self: Maybe[A]): ElseOps[A, Nothing] =
    macro ElseMacros.ElseOps[A, Nothing]
  
  /** Implicitly adds operations to `Truth` values.
    * @group Conditional */
  implicit def TruthOps(self: Truth): TruthOps =
    macro TruthMacros.TruthOps
}
