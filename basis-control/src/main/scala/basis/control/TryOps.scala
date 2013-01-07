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
final class TryOps[+T](result: Try[T]) {
  /** Returns the value for a `Success`, or the `default` value for a `Failure`.
    * @group Evaluating */
  def getOrElse[U >: T](default: U): U =
    macro TryMacros.getOrElse[U]
  
  /** Returns `this` for a `Success`, or the `default` for a `Failure`.
    * @group Evaluating */
  def orElse[U >: T](default: Try[U]): Try[U] =
    macro TryMacros.orElse[U]
  
  /** Applies the given side-effecting function only to a `Success` value.
    * @group Composing */
  def foreach[U](f: T => U): Unit =
    macro TryMacros.foreach[T, U]
  
  /** Tries the given function applied to a `Success` value,
    * or returns `this` for a `Failure`.
    * @group Composing */
  def map[U](f: T => U): Try[U] =
    macro TryMacros.map[T, U]
  
  /** Returns the given function applied to a `Success` value,
    * or returns `this` for a `Failure`.
    * @group Composing */
  def flatMap[U](f: T => Try[U]): Try[U] =
    macro TryMacros.flatMap[T, U]
  
  /** Returns `this` for a `Success` whose value satisfies the given predicate,
    * otherwise returns a `Failure`.
    * @group Composing */
  def filter(p: T => Boolean): Try[T] =
    macro TryMacros.filter[T]
  
  /** Returns `this` for a `Success` or an undefined `Failure`, otherwise
    * tries the given function applied to a defined `Failure` cause.
    * @group Recovering */
  def recover[U >: T](q: PartialFunction[Throwable, U]): Try[U] =
    macro TryMacros.recover[U]
  
  /** Returns `this` for a `Success` or an undefined `Failure`, otherwise
    * returns the given function applied to a defined `Failure` cause.
    * @group Recovering */
  def recoverWith[U >: T](q: PartialFunction[Throwable, Try[U]]): Try[U] =
    macro TryMacros.recoverWith[U]
}

private[control] object TryMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[T : c.WeakTypeTag](c: Context): c.Expr[Try[T]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, result :: Nil) = prefix.tree
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[T] :: Nil)
    Expr(typeCheck(result, TryType))(WeakTypeTag(TryType))
  }
  
  def TryOps[T : c.WeakTypeTag](c: Context)(result: c.Expr[Try[T]]): c.Expr[TryOps[T]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val TryOpsType = appliedType(mirror.staticClass("basis.control.TryOps").toType, weakTypeOf[T] :: Nil)
    Expr(New(TryOpsType, result.tree))(WeakTypeTag(TryOpsType))
  }
  
  def apply[T : c.WeakTypeTag](c: Context)(op: c.Expr[T]): c.Expr[Try[T]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[T] :: Nil)
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
  
  def getOrElse[T : c.WeakTypeTag](c: Context)(default: c.Expr[T]): c.Expr[T] = {
    import c.{Expr, fresh, weakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    Expr {
      Block(
        ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) :: Nil,
        If(Select(Ident(r), "isSuccess"), Select(Ident(r), "get"), default.tree))
    } (weakTypeTag[T])
  }
  
  def orElse[T : c.WeakTypeTag](c: Context)(default: c.Expr[Try[T]]): c.Expr[Try[T]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[T] :: Nil)
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) :: Nil,
          If(Select(Ident(r), "isSuccess"), Ident(r), default.tree)),
        CaseDef(
          Bind(e, Ident(nme.WILDCARD)),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "NonFatal"), Ident(e) :: Nil),
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Failure"), Ident(e) :: Nil)) :: Nil,
        EmptyTree)
    } (WeakTypeTag(TryType))
  }
  
  def foreach[T : c.WeakTypeTag, U](c: Context)(f: c.Expr[T => U]): c.Expr[Unit] = {
    import c.{Expr, fresh, TypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    Expr {
      Block(
        ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) :: Nil,
        If(Select(Ident(r), "isSuccess"), Apply(f.tree, Select(Ident(r), "get") :: Nil), EmptyTree))
    } (TypeTag.Unit)
  }
  
  def map[T : c.WeakTypeTag, U : c.WeakTypeTag](c: Context)(f: c.Expr[T => U]): c.Expr[Try[U]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[U] :: Nil)
    val SuccessType = appliedType(mirror.staticClass("basis.control.Success").toType, weakTypeOf[U] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) :: Nil,
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
  
  def flatMap[T : c.WeakTypeTag, U : c.WeakTypeTag](c: Context)(f: c.Expr[T => Try[U]]): c.Expr[Try[U]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[U] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) :: Nil,
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
  
  def filter[T : c.WeakTypeTag](c: Context)(p: c.Expr[T => Boolean]): c.Expr[Try[T]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[T] :: Nil)
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) :: Nil,
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
  
  def recover[T : c.WeakTypeTag](c: Context)(q: c.Expr[PartialFunction[Throwable, T]]): c.Expr[Try[T]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    val f = newTermName(fresh("f$"))
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[T] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) ::
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
  
  def recoverWith[T : c.WeakTypeTag](c: Context)(q: c.Expr[PartialFunction[Throwable, Try[T]]]): c.Expr[Try[T]] = {
    import c.{Expr, fresh, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val r = newTermName(fresh("r$"))
    val f = newTermName(fresh("f$"))
    val e = newTermName(fresh("e$"))
    val TryType = appliedType(mirror.staticClass("basis.control.Try").toType, weakTypeOf[T] :: Nil)
    val FailureType = mirror.staticClass("basis.control.Failure").toType
    Expr {
      Try(
        Block(
          ValDef(NoMods, r, TypeTree(), unApply[T](c).tree) ::
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
}
