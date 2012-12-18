/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._

class Strict extends General {
  implicit final def StrictEnumeratorOps[A](these: Enumerator[A]): StrictEnumeratorOps[A, these.Family] =
    macro Strict.StrictEnumeratorOps[A]
  
  implicit final def StrictIteratorOps[A](these: Iterator[A]): StrictIteratorOps[A, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictCollectionOps[A](these: Collection[A]): StrictCollectionOps[A, these.Family] =
    macro Strict.StrictCollectionOps[A]
  
  implicit final def StrictContainerOps[A](these: Container[A]): StrictContainerOps[A, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictSeqOps[A](these: Seq[A]): StrictSeqOps[A, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictIndexOps[A](these: Index[A]): StrictIndexOps[A, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictStackOps[A](these: Stack[A]): StrictStackOps[A, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictSetOps[A](these: Set[A]): StrictSetOps[A, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  implicit final def StrictMapOps[A, T](these: Map[A, T]): StrictMapOps[A, T, these.Family] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}

private[sequential] object Strict {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def StrictEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[StrictEnumeratorOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictEnumeratorOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictEnumeratorOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictEnumeratorOpsType, these.tree))(WeakTypeTag(StrictEnumeratorOpsType))
  }
  
  def StrictIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[StrictIteratorOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictIteratorOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictIteratorOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictIteratorOpsType, these.tree))(WeakTypeTag(StrictIteratorOpsType))
  }
  
  def StrictCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[StrictCollectionOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictCollectionOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictCollectionOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictCollectionOpsType, these.tree))(WeakTypeTag(StrictCollectionOpsType))
  }
  
  def StrictContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[StrictContainerOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictContainerOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictContainerOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictContainerOpsType, these.tree))(WeakTypeTag(StrictContainerOpsType))
  }
  
  def StrictSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[StrictSeqOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictSeqOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictSeqOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictSeqOpsType, these.tree))(WeakTypeTag(StrictSeqOpsType))
  }
  
  def StrictIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[StrictIndexOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictIndexOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictIndexOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictIndexOpsType, these.tree))(WeakTypeTag(StrictIndexOpsType))
  }
  
  def StrictStackOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Stack[A]])
    : c.Expr[StrictStackOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictStackOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictStackOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictStackOpsType, these.tree))(WeakTypeTag(StrictStackOpsType))
  }
  
  def StrictSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[StrictSetOps[A, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictSetOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictSetOps").toType,
        weakTypeOf[A] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictSetOpsType, these.tree))(WeakTypeTag(StrictSetOpsType))
  }
  
  def StrictMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[StrictMapOps[A, T, these.value.Family]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val StrictMapOpsType =
      appliedType(
        mirror.staticClass("basis.sequential.StrictMapOps").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: FamilyType(c)(these) :: Nil)
    Expr(New(StrictMapOpsType, these.tree))(WeakTypeTag(StrictMapOpsType))
  }
  
  private def FamilyType(c: Context)(these: c.Expr[Family[_]]): c.Type = {
    import c.universe._
    val TheseType = these.tree.symbol match {
      case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
      case _ => these.actualType
    }
    val FamilySymbol = c.mirror.staticClass("basis.collections.Family").toType.member(newTypeName("Family"))
    typeRef(TheseType, FamilySymbol, Nil)
  }
}
