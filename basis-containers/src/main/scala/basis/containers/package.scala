/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Specialized collection implementations.
  * 
  * @groupprio  Surrogates    1
  * @groupprio  Containers    2
  * @groupprio  Collections   3
  * @groupprio  Builders      4
  */
package object containers extends basis.containers.Library

package containers {
  import basis.collections._
  import basis.runtime._
  
  private[containers] class Library extends Library1 {
    /** Implicitly returns a new [[ArraySeq]] builder.
      * @group Builders */
    implicit def ArraySeqBuilder[A](implicit A: TypeHint[A])
      : Builder[ArraySeq[_], A] { type State = ArraySeq[A] } =
      ArraySeq.Builder(A)
    
    /** Implicitly returns a new [[ArrayBuffer]] builder.
      * @group Builders */
    implicit def ArrayBufferBuilder[A](implicit A: TypeHint[A])
      : Builder[ArrayBuffer[_], A] { type State = ArrayBuffer[A] } =
      ArrayBuffer.Builder(A)
    
    /** Implicitly returns a new [[List]] builder.
      * @group Builders */
    implicit def ListBuilder[A](implicit A: TypeHint[A])
      : Builder[List[_], A] { type State = List[A] } =
      new ListBuilder
    
    /** Implicitly returns a new [[ListBuffer]] builder.
      * @group Builders */
    implicit def ListBufferBuilder[A](implicit A: TypeHint[A])
      : Builder[ListBuffer[_], A] { type State = ListBuffer[A] } =
      new ListBufferBuilder
    
    /** Implicitly returns a new [[Vector]] builder.
      * @group Builders */
    implicit def VectorBuilder[A](implicit A: TypeHint[A])
      : Builder[Vector[_], A] { type State = Vector[A] } =
      new VectorBuilder
    
    /** Implicitly returns a new [[HashSet]] builder.
      * @group Builders */
    implicit def HashSetBuilder[A](implicit A: TypeHint[A])
      : Builder[HashSet[_], A] { type State = HashSet[A] } =
      new HashSetBuilder
    
    /** Implicitly returns a new [[HashMap]] builder.
      * @group Builders */
    implicit def HashMapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
      : Builder[HashMap[_, _], (A, T)] { type State = HashMap[A, T] } =
      new HashMapBuilder
    
    /** Implicitly returns a new [[ArrayMap]] builder.
      * @group Builders */
    private[containers] implicit def ArrayMapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
      : Builder[ArrayMap[_, _], (A, T)] { type State = ArrayMap[A, T] } =
      new ArrayMapBuilder
    
    /** Implicitly returns a new [[ArraySet]] builder.
      * @group Builders */
    private[containers] implicit def ArraySetBuilder[A](implicit A: TypeHint[A])
      : Builder[ArraySet[_], A] { type State = ArraySet[A] } =
      new ArraySetBuilder
  }

  private[containers] abstract class Library1 extends Library2 { this: Library =>
    /** Implicitly provides a default [[Enumerator]] factory.
      * @group Collections */
    implicit val Enumerator: Vector.type = Vector
    
    /** Implicitly returns a new default [[Index]] builder.
      * @group Builders */
    implicit def IndexBuilder[A](implicit A: TypeHint[A])
      : Builder[Index[_], A] { type State = ArraySeq[A] } =
      Index.Builder(A)
    
    /** Implicitly returns a new default [[Stack]] builder.
      * @group Builders */
    implicit def StackBuilder[A](implicit A: TypeHint[A])
      : Builder[Stack[_], A] { type State = List[A] } =
      Stack.Builder[A]
  }

  private[containers] abstract class Library2 extends Library3 { this: Library =>
    /** Implicitly provides a default [[Collection]] factory.
      * @group Collections */
    implicit val Collection: Vector.type = Vector
    
    /** Implicitly returns a new default [[Seq]] builder.
      * @group Builders */
    implicit def SeqBuilder[A](implicit A: TypeHint[A])
      : Builder[Seq[_], A] { type State = Vector[A] } =
      Seq.Builder(A)
    
    /** Implicitly returns a new default [[Set]] builder.
      * @group Builders */
    implicit def SetBuilder[A](implicit A: TypeHint[A])
      : Builder[Set[_], A] { type State = HashSet[A] } =
      Set.Builder(A)
    
    /** Implicitly returns a new default [[Map]] builder.
      * @group Builders */
    implicit def MapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
      : Builder[Map[_, _], (A, T)] { type State = HashMap[A, T] } =
      Map.Builder(A, T)
  }

  private[containers] abstract class Library3 extends Library4 { this: Library =>
    /** Implicitly provides a default [[Container]] factory.
      * @group Collections */
    implicit val Container: Vector.type = Vector
    
    /** Implicitly returns a new default [[Container]] builder.
      * @group Builders */
    implicit def ContainerBuilder[A](implicit A: TypeHint[A])
      : Builder[Container[_], A] { type State = Vector[A] } =
      Container.Builder(A)
  }

  private[containers] abstract class Library4 extends Library5 { this: Library =>
    /** Implicitly provides a default [[Seq]] factory.
      * @group Collections */
    implicit val Seq: Vector.type = Vector
    
    /** Implicitly provides a default [[Set]] factory.
      * @group Collections */
    implicit val Set: HashSet.type = HashSet
    
    /** Implicitly provides a default [[Map]] factory.
      * @group Collections */
    implicit val Map: HashMap.type = HashMap
    
    /** Implicitly returns a new default [[Collection]] builder.
      * @group Builders */
    implicit def CollectionBuilder[A](implicit A: TypeHint[A])
      : Builder[Collection[_], A] { type State = Vector[A] } =
      Collection.Builder(A)
  }

  private[containers] abstract class Library5 { this: Library =>
    /** Implicitly provides a default [[Index]] factory.
      * @group Collections */
    implicit val Index: ArraySeq.type = ArraySeq
    
    /** Implicitly provides a default [[Stack]] factory.
      * @group Collections */
    implicit val Stack: List.type = List
    
    /** Implicitly returns a new default [[Enumerator]] builder.
      * @group Builders */
    implicit def EnumeratorBuilder[A](implicit A: TypeHint[A])
      : Builder[Enumerator[_], A] { type State = Vector[A] } =
      Enumerator.Builder(A)
  }
}
