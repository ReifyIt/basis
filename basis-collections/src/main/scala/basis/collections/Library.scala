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
  * @group    Factories
  * 
  * @groupprio  Collections   1
  * @groupprio  Builders      2
  */
trait Library extends Library1 {
  /** The default [[Enumerator]] factory.
    * @group Collections */
  val Enumerator: BuilderFactory[Enumerator]
  
  /** The default [[Collection]] factory.
    * @group Collections */
  val Collection: BuilderFactory[Collection]
  
  /** The default [[Container]] factory.
    * @group Collections */
  val Container: BuilderFactory[Container]
  
  /** The default [[Seq]] factory.
    * @group Collections */
  val Seq: SeqFactory[Seq]
  
  /** The default [[Set]] factory.
    * @group Collections */
  val Set: SetFactory[Set]
  
  /** The default [[Map]] factory.
    * @group Collections */
  val Map: MapFactory[Map]
  
  /** The default [[Index]] factory.
    * @group Collections */
  val Index: SeqFactory[Index]
  
  /** The default [[Stack]] factory.
    * @group Collections */
  val Stack: SeqFactory[Stack]
}

private[collections] trait Library1 extends Library2 { this: Library =>
  /** Implicitly provides the default [[Enumerator]] factory.
    * @group Collections */
  implicit def EnumeratorFactory: Enumerator.type = Enumerator
  
  /** Implicitly returns a new default [[Index]] builder.
    * @group Builders */
  implicit def IndexBuilder[A](implicit A: TypeHint[A])
    : Builder[Index[_], A] { type State = Index.Product[A] } =
    Index.Builder(A)
  
  /** Implicitly returns a new default [[Stack]] builder.
    * @group Builders */
  implicit def StackBuilder[A](implicit A: TypeHint[A])
    : Builder[Stack[_], A] { type State = Stack.Product[A] } =
    Stack.Builder[A]
}

private[collections] trait Library2 extends Library3 { this: Library =>
  /** Implicitly provides the default [[Collection]] factory.
    * @group Collections */
  implicit def CollectionFactory: Collection.type = Collection
  
  /** Implicitly returns a new default [[Seq]] builder.
    * @group Builders */
  implicit def SeqBuilder[A](implicit A: TypeHint[A])
    : Builder[Seq[_], A] { type State = Seq.Product[A] } =
    Seq.Builder(A)
  
  /** Implicitly returns a new default [[Set]] builder.
    * @group Builders */
  implicit def SetBuilder[A](implicit A: TypeHint[A])
    : Builder[Set[_], A] { type State = Set.Product[A] } =
    Set.Builder(A)
  
  /** Implicitly returns a new default [[Map]] builder.
    * @group Builders */
  implicit def MapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
    : Builder[Map[_, _], (A, T)] { type State = Map.Product[A, T] } =
    Map.Builder(A, T)
}

private[collections] trait Library3 extends Library4 { this: Library =>
  /** Implicitly provides the default [[Container]] factory.
    * @group Collections */
  implicit def ContainerFactory: Container.type = Container
  
  /** Implicitly returns a new default [[Container]] builder.
    * @group Builders */
  implicit def ContainerBuilder[A](implicit A: TypeHint[A])
    : Builder[Container[_], A] { type State = Container.Product[A] } =
    Container.Builder(A)
}

private[collections] trait Library4 extends Library5 { this: Library =>
  /** Implicitly provides the default [[Seq]] factory.
    * @group Collections */
  implicit def SeqFactory: Seq.type = Seq
  
  /** Implicitly provides the default [[Set]] factory.
    * @group Collections */
  implicit def SetFactory: Set.type = Set
  
  /** Implicitly provides the default [[Map]] factory.
    * @group Collections */
  implicit def MapFactory: Map.type = Map
  
  /** Implicitly returns a new default [[Collection]] builder.
    * @group Builders */
  implicit def CollectionBuilder[A](implicit A: TypeHint[A])
    : Builder[Collection[_], A] { type State = Collection.Product[A] } =
    Collection.Builder(A)
}

private[collections] trait Library5 { this: Library =>
  /** Implicitly provides the default [[Index]] factory.
    * @group Collections */
  implicit def IndexFactory: Index.type = Index
  
  /** Implicitly provides the default [[Stack]] factory.
    * @group Collections */
  implicit def StackFactory: Stack.type = Stack
  
  /** Implicitly returns a new default [[Enumerator]] builder.
    * @group Builders */
  implicit def EnumeratorBuilder[A](implicit A: TypeHint[A])
    : Builder[Enumerator[_], A] { type State = Enumerator.Product[A] } =
    Enumerator.Builder(A)
}
