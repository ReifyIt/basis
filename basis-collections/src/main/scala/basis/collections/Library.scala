/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.reflect.ClassTag

/** A source of collections. */
trait Library extends Library1 {
  val Enumerator: BuilderFactory[Enumerator]
  
  val Collection: BuilderFactory[Collection]
  
  val Container: BuilderFactory[Container]
  
  val Seq: SeqFactory[Seq]
  
  val Set: SetFactory[Set]
  
  val Map: MapFactory[Map]
  
  val Index: SeqFactory[Index]
  
  val Stack: SeqFactory[Stack]
}

private[collections] trait Library1 extends Library2 { this: Library =>
  implicit def EnumeratorFactory: Enumerator.type = Enumerator
  
  implicit def IndexBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Index[_], A] { type State = Index.Product[A] } =
    Index.Builder(A)
  
  implicit def StackBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Stack[_], A] { type State = Stack.Product[A] } =
    Stack.Builder[A]
}

private[collections] trait Library2 extends Library3 { this: Library =>
  implicit def CollectionFactory: Collection.type = Collection
  
  implicit def SeqBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Seq[_], A] { type State = Seq.Product[A] } =
    Seq.Builder(A)
  
  implicit def SetBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Set[_], A] { type State = Set.Product[A] } =
    Set.Builder(A)
  
  implicit def MapBuilder[A, T]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]],
                T: ClassTag[T] = ClassTag.Any.asInstanceOf[ClassTag[T]])
    : Builder[Map[_, _], (A, T)] { type State = Map.Product[A, T] } =
    Map.Builder(A, T)
}

private[collections] trait Library3 extends Library4 { this: Library =>
  implicit def ContainerFactory: Container.type = Container
  
  implicit def ContainerBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Container[_], A] { type State = Container.Product[A] } =
    Container.Builder(A)
}

private[collections] trait Library4 extends Library5 { this: Library =>
  implicit def SeqFactory: Seq.type = Seq
  
  implicit def SetFactory: Set.type = Set
  
  implicit def MapFactory: Map.type = Map
  
  implicit def CollectionBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Collection[_], A] { type State = Collection.Product[A] } =
    Collection.Builder(A)
}

private[collections] trait Library5 { this: Library =>
  implicit def IndexFactory: Index.type = Index
  
  implicit def StackFactory: Stack.type = Stack
  
  implicit def EnumeratorBuilder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Enumerator[_], A] { type State = Enumerator.Product[A] } =
    Enumerator.Builder(A)
}
