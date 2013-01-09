/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** Standard [[Try]] result combinators.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  * @groupprio  Composing   2
  * @groupprio  Recovering  3
  */
final class TryOps[+A](result: Try[A]) {
  /** Returns the value for a `Success`, or the `default` value for a `Failure`.
    * @group Evaluating */
  def getOrElse[B >: A](default: B): B =
    macro TryMacros.getOrElse[B]
  
  /** Returns `this` for a `Success`, or `alternate` for a `Failure`.
    * @group Composing */
  def orElse[B >: A](alternate: Try[B]): Try[B] =
    macro TryMacros.orElse[B]
  
  /** Applies the given side-effecting function only to a `Success` value.
    * @group Evaluating */
  def foreach[U](f: A => U): Unit =
    macro TryMacros.foreach[A, U]
  
  /** Tries the given function applied to a `Success` value,
    * or returns `this` for a `Failure`.
    * @group Composing */
  def map[B](f: A => B): Try[B] =
    macro TryMacros.map[A, B]
  
  /** Returns the given `Try` function applied to a `Success` value,
    * or returns `this` for a `Failure`.
    * @group Composing */
  def flatMap[B](f: A => Try[B]): Try[B] =
    macro TryMacros.flatMap[A, B]
  
  /** Returns `this` for a `Success` or an undefined `Failure`, otherwise
    * tries the given function applied to a defined `Failure` cause.
    * @group Recovering */
  def recover[B >: A](q: PartialFunction[Throwable, B]): Try[B] =
    macro TryMacros.recover[B]
  
  /** Returns `this` for a `Success` or an undefined `Failure`, otherwise
    * returns the given function applied to a defined `Failure` cause.
    * @group Recovering */
  def recoverWith[B >: A](q: PartialFunction[Throwable, Try[B]]): Try[B] =
    macro TryMacros.recoverWith[B]
  
  /** Returns `this` for a `Success` whose value satisfies the given predicate,
    * otherwise returns a `Failure`.
    * @group Composing */
  def filter(p: A => Boolean): Try[A] =
    macro TryMacros.filter[A]
  
  /** Returns `this` for a `Success` whose value satisfies the given predicate,
    * otherwise returns a `Failure`; equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): Try[A] =
    macro TryMacros.filter[A]
}

private[control] object TryMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Try[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, result :: Nil) = prefix.tree
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[A] :: Nil)
    Expr(typeCheck(result, TryType))(WeakTypeTag(TryType))
  }
  
  def TryOps[A : c.WeakTypeTag](c: Context)(result: c.Expr[Try[A]]): c.Expr[TryOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val TryOpsType = appliedType(mirror.staticClass("basis.control.TryOps").toType, weakTypeOf[A] :: Nil)
    Expr(New(TryOpsType, result.tree))(WeakTypeTag(TryOpsType))
  }
  
  def apply[A : c.WeakTypeTag](c: Context)(op: c.Expr[A]): c.Expr[Try[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[A] :: Nil)
    Expr {
      Try(
        Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Success"), op.tree :: Nil),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def getOrElse[A : c.WeakTypeTag](c: Context)(default: c.Expr[A]): c.Expr[A] = {
    import c.{Expr, fresh, weakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    Expr {
      Block(
        ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(r), "isSuccess"), Select(Ident(r), "get"), default.tree))
    } (weakTypeTag[A])
  }
  
  def orElse[A : c.WeakTypeTag](c: Context)(alternate: c.Expr[Try[A]]): c.Expr[Try[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[A] :: Nil)
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) :: Nil,
          If(Select(Ident(r), "isSuccess"), Ident(r), alternate.tree)),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def foreach[A : c.WeakTypeTag, U](c: Context)(f: c.Expr[A => U]): c.Expr[Unit] = {
    import c.{Expr, fresh, TypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    Expr {
      Block(
        ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) :: Nil,
        If(Select(Ident(r), "isSuccess"), Apply(f.tree, Select(Ident(r), "get") :: Nil), EmptyTree))
    } (TypeTag.Unit)
  }
  
  def map[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(f: c.Expr[A => B]): c.Expr[Try[B]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[B] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) :: Nil,
          If(
            Select(Ident(r), "isSuccess"),
            Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Success"),
                  Apply(f.tree, Select(Ident(r), "get") :: Nil) :: Nil),
            TypeApply(Select(Ident(r), "asInstanceOf"), TypeTree(FailureType) :: Nil))),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(f: c.Expr[A => Try[B]]): c.Expr[Try[B]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[B] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) :: Nil,
          If(
            Select(Ident(r), "isSuccess"),
            Apply(f.tree, Select(Ident(r), "get") :: Nil),
            TypeApply(Select(Ident(r), "asInstanceOf"), TypeTree(FailureType) :: Nil))),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def recover[A : c.WeakTypeTag](c: Context)(q: c.Expr[PartialFunction[Throwable, A]]): c.Expr[Try[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    val f = newTermName(fresh("pf$"))
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[A] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) ::
          ValDef(NoMods, f, TypeTree(), q.tree) :: Nil,
          If(
            Apply(
              Select(Select(Ident(r), "isSuccess"), "$bar$bar"),
              Select(
                Apply(
                  Select(Ident(f), "isDefinedAt"),
                  Select(TypeApply(Select(Ident(r), "asInstanceOf"), TypeTree(FailureType) :: Nil), "cause") :: Nil),
                "unary_$bang") :: Nil),
            Ident(r),
            Apply(
              Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Success"),
              Apply(
                Select(Ident(f), "applyOrElse"),
                Select(TypeApply(Select(Ident(r), "asInstanceOf"), TypeTree(FailureType) :: Nil), "cause") ::
                Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil))),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def recoverWith[A : c.WeakTypeTag](c: Context)(q: c.Expr[PartialFunction[Throwable, Try[A]]]): c.Expr[Try[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    val f = newTermName(fresh("pf$"))
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[A] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) ::
          ValDef(NoMods, f, TypeTree(), q.tree) :: Nil,
          If(
            Apply(
              Select(Select(Ident(r), "isSuccess"), "$bar$bar"),
              Select(
                Apply(
                  Select(Ident(f), "isDefinedAt"),
                  Select(TypeApply(Select(Ident(r), "asInstanceOf"), TypeTree(FailureType) :: Nil), "cause") :: Nil),
                "unary_$bang") :: Nil),
            Ident(r),
            Apply(
              Select(Ident(f), "applyOrElse"),
              Select(TypeApply(Select(Ident(r), "asInstanceOf"), TypeTree(FailureType) :: Nil), "cause") ::
              Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil))),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def filter[A : c.WeakTypeTag](c: Context)(p: c.Expr[A => Boolean]): c.Expr[Try[A]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("result$"))
    val e = newTermName(fresh("exception$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[A] :: Nil)
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[A](c).tree) :: Nil,
          If(
            Apply(Select(Select(Ident(r), "isFailure"), "$bar$bar"), Apply(p.tree, Select(Ident(r), "get") :: Nil) :: Nil),
            Ident(r),
            Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), "undefined"))),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
}
