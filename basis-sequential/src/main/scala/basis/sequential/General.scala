/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Implicit conversions that add general operations to collections.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    General
  * 
  * @groupname  General   General collection extensions
  * @groupprio  General   1
  */
class General {
  /** Implicitly provides general operations for arrays.
    * @group General */
  implicit def GeneralArrayOps[A](these: Array[A]): GeneralArrayOps[A] =
    macro General.GeneralArrayOps[A]
  
  /** Implicitly provides general operations for enumerators.
    * @group General */
  implicit def GeneralEnumeratorOps[A](these: Enumerator[A]): GeneralEnumeratorOps[A] =
    macro General.GeneralEnumeratorOps[A]
  
  /** Implicitly provides general operations for iterators.
    * @group General */
  implicit def GeneralIteratorOps[A](these: Iterator[A]): GeneralIteratorOps[A] =
    macro General.GeneralIteratorOps[A]
  
  /** Implicitly provides general operations for collections.
    * @group General */
  implicit def GeneralCollectionOps[A](these: Collection[A]): GeneralCollectionOps[A] =
    macro General.GeneralCollectionOps[A]
  
  /** Implicitly provides general operations for containers.
    * @group General */
  implicit def GeneralContainerOps[A](these: Container[A]): GeneralContainerOps[A] =
    macro General.GeneralContainerOps[A]
  
  /** Implicitly provides general operations for sequences.
    * @group General */
  implicit def GeneralSeqOps[A](these: Seq[A]): GeneralSeqOps[A] =
    macro General.GeneralSeqOps[A]
  
  /** Implicitly provides general operations for indexes.
    * @group General */
  implicit def GeneralIndexOps[A](these: Index[A]): GeneralIndexOps[A] =
    macro General.GeneralIndexOps[A]
  
  /** Implicitly provides general operations for stacks.
    * @group General */
  implicit def GeneralStackOps[A](these: Stack[A]): GeneralStackOps[A] =
    macro General.GeneralStackOps[A]
  
  /** Implicitly provides general operations for sets.
    * @group General */
  implicit def GeneralSetOps[A](these: Set[A]): GeneralSetOps[A] =
    macro General.GeneralSetOps[A]
  
  /** Implicitly provides general operations for maps.
    * @group General */
  implicit def GeneralMapOps[A, T](these: Map[A, T]): GeneralMapOps[A, T] =
    macro General.GeneralMapOps[A, T]
}

private[sequential] object General {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def GeneralArrayOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Array[A]])
    : c.Expr[GeneralArrayOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralArrayOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralArrayOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralArrayOpsTpe, these.tree))(WeakTypeTag(GeneralArrayOpsTpe))
  }
  
  def GeneralEnumeratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Enumerator[A]])
    : c.Expr[GeneralEnumeratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralEnumeratorOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralEnumeratorOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralEnumeratorOpsTpe, these.tree))(WeakTypeTag(GeneralEnumeratorOpsTpe))
  }
  
  def GeneralIteratorOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Iterator[A]])
    : c.Expr[GeneralIteratorOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralIteratorOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralIteratorOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralIteratorOpsTpe, these.tree))(WeakTypeTag(GeneralIteratorOpsTpe))
  }
  
  def GeneralCollectionOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Collection[A]])
    : c.Expr[GeneralCollectionOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralCollectionOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralCollectionOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralCollectionOpsTpe, these.tree))(WeakTypeTag(GeneralCollectionOpsTpe))
  }
  
  def GeneralContainerOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Container[A]])
    : c.Expr[GeneralContainerOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralContainerOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralContainerOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralContainerOpsTpe, these.tree))(WeakTypeTag(GeneralContainerOpsTpe))
  }
  
  def GeneralSeqOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Seq[A]])
    : c.Expr[GeneralSeqOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralSeqOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralSeqOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralSeqOpsTpe, these.tree))(WeakTypeTag(GeneralSeqOpsTpe))
  }
  
  def GeneralIndexOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Index[A]])
    : c.Expr[GeneralIndexOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralIndexOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralIndexOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralIndexOpsTpe, these.tree))(WeakTypeTag(GeneralIndexOpsTpe))
  }
  
  def GeneralStackOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Stack[A]])
    : c.Expr[GeneralStackOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralStackOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralStackOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralStackOpsTpe, these.tree))(WeakTypeTag(GeneralStackOpsTpe))
  }
  
  def GeneralSetOps[A : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Set[A]])
    : c.Expr[GeneralSetOps[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralSetOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralSetOps").toType,
        weakTypeOf[A] :: Nil)
    Expr(New(GeneralSetOpsTpe, these.tree))(WeakTypeTag(GeneralSetOpsTpe))
  }
  
  def GeneralMapOps[A : c.WeakTypeTag, T : c.WeakTypeTag]
      (c: Context)
      (these: c.Expr[Map[A, T]])
    : c.Expr[GeneralMapOps[A, T]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val GeneralMapOpsTpe =
      appliedType(
        mirror.staticClass("basis.sequential.GeneralMapOps").toType,
        weakTypeOf[A] :: weakTypeOf[T] :: Nil)
    Expr(New(GeneralMapOpsTpe, these.tree))(WeakTypeTag(GeneralMapOpsTpe))
  }
}
