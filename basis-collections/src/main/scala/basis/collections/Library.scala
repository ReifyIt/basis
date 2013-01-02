/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import basis.runtime._

/** A source of collections.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
trait Library extends Library1 {
  /** The default [[Enumerator]] factory. */
  val Enumerator: BuilderFactory[Enumerator]
  
  /** The default [[Collection]] factory. */
  val Collection: BuilderFactory[Collection]
  
  /** The default [[Container]] factory. */
  val Container: BuilderFactory[Container]
  
  /** The default [[Seq]] factory. */
  val Seq: SeqFactory[Seq]
  
  /** The default [[Set]] factory. */
  val Set: SetFactory[Set]
  
  /** The default [[Map]] factory. */
  val Map: MapFactory[Map]
  
  /** The default [[Index]] factory. */
  val Index: SeqFactory[Index]
  
  /** The default [[Stack]] factory. */
  val Stack: SeqFactory[Stack]
}

private[collections] trait Library1 extends Library2 { this: Library =>
  /** Implicitly provides the default [[Enumerator]] factory. */
  implicit def EnumeratorFactory: Enumerator.type = Enumerator
  
  /** Implicitly returns a new default [[Index]] builder. */
  implicit def IndexBuilder[A](implicit A: TypeHint[A])
    : Builder[Index[_], A] { type State = Index.Product[A] } =
    Index.Builder(A)
  
  /** Implicitly returns a new default [[Stack]] builder. */
  implicit def StackBuilder[A](implicit A: TypeHint[A])
    : Builder[Stack[_], A] { type State = Stack.Product[A] } =
    Stack.Builder[A]
}

private[collections] trait Library2 extends Library3 { this: Library =>
  /** Implicitly provides the default [[Collection]] factory. */
  implicit def CollectionFactory: Collection.type = Collection
  
  /** Implicitly returns a new default [[Seq]] builder. */
  implicit def SeqBuilder[A](implicit A: TypeHint[A])
    : Builder[Seq[_], A] { type State = Seq.Product[A] } =
    Seq.Builder(A)
  
  /** Implicitly returns a new default [[Set]] builder. */
  implicit def SetBuilder[A](implicit A: TypeHint[A])
    : Builder[Set[_], A] { type State = Set.Product[A] } =
    Set.Builder(A)
  
  /** Implicitly returns a new default [[Map]] builder. */
  implicit def MapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
    : Builder[Map[_, _], (A, T)] { type State = Map.Product[A, T] } =
    Map.Builder(A, T)
}

private[collections] trait Library3 extends Library4 { this: Library =>
  /** Implicitly provides the default [[Container]] factory. */
  implicit def ContainerFactory: Container.type = Container
  
  /** Implicitly returns a new default [[Container]] builder. */
  implicit def ContainerBuilder[A](implicit A: TypeHint[A])
    : Builder[Container[_], A] { type State = Container.Product[A] } =
    Container.Builder(A)
}

private[collections] trait Library4 extends Library5 { this: Library =>
  /** Implicitly provides the default [[Seq]] factory. */
  implicit def SeqFactory: Seq.type = Seq
  
  /** Implicitly provides the default [[Set]] factory. */
  implicit def SetFactory: Set.type = Set
  
  /** Implicitly provides the default [[Map]] factory. */
  implicit def MapFactory: Map.type = Map
  
  /** Implicitly returns a new default [[Collection]] builder. */
  implicit def CollectionBuilder[A](implicit A: TypeHint[A])
    : Builder[Collection[_], A] { type State = Collection.Product[A] } =
    Collection.Builder(A)
}

private[collections] trait Library5 { this: Library =>
  /** Implicitly provides the default [[Index]] factory. */
  implicit def IndexFactory: Index.type = Index
  
  /** Implicitly provides the default [[Stack]] factory. */
  implicit def StackFactory: Stack.type = Stack
  
  /** Implicitly returns a new default [[Enumerator]] builder. */
  implicit def EnumeratorBuilder[A](implicit A: TypeHint[A])
    : Builder[Enumerator[_], A] { type State = Enumerator.Product[A] } =
    Enumerator.Builder(A)
}
