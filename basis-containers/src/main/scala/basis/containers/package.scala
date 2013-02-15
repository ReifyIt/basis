/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

import basis.collections._
import basis.runtime._

import scala.reflect.ClassTag

/** Specialized collection implementations.
  * 
  * @groupprio  Surrogates    1
  * @groupprio  Containers    2
  * @groupprio  Collections   3
  * @groupprio  Builders      4
  */
package object containers extends basis.containers.Library1

package containers {
  private[containers] abstract class Library1 extends Library2 {
    /** The default [[Enumerator]] factory.
      * @group Collections */
    val Enumerator: BuilderFactory[Enumerator] = Vector
    
    /** Implicitly returns a new default [[Enumerator]] builder.
      * @group Builders */
    implicit def EnumeratorBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Enumerator[_]; type State = Enumerator[A] } =
      Enumerator.Builder(A)
  }

  private[containers] abstract class Library2 extends Library3 {
    /** The default [[Collection]] factory.
      * @group Collections */
    val Collection: BuilderFactory[Collection] = Vector
    
    /** Implicitly returns a new default [[Collection]] builder.
      * @group Builders */
    implicit def CollectionBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Collection[_]; type State = Collection[A] } =
      Collection.Builder(A)
  }

  private[containers] abstract class Library3 extends Library4 {
    /** The default [[Container]] factory.
      * @group Collections */
    val Container: BuilderFactory[Container] = Vector
    
    /** Implicitly returns a new default [[Container]] builder.
      * @group Builders */
    implicit def ContainerBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Container[_]; type State = Container[A] } =
      Container.Builder(A)
  }

  private[containers] abstract class Library4 extends Library5 {
    /** The default [[Seq]] factory.
      * @group Collections */
    val Seq: SeqFactory[Seq] = Vector
    
    /** The default [[Set]] factory.
      * @group Collections */
    val Set: SetFactory[Set] = HashSet
    
    /** The default [[Map]] factory.
      * @group Collections */
    val Map: MapFactory[Map] = HashMap
    
    /** Implicitly returns a new default [[Seq]] builder.
      * @group Builders */
    implicit def SeqBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Seq[_]; type State = Seq[A] } =
      Seq.Builder(A)
    
    /** Implicitly returns a new default [[Set]] builder.
      * @group Builders */
    implicit def SetBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Set[_]; type State = Set[A] } =
      Set.Builder(A)
    
    /** Implicitly returns a new default [[Map]] builder.
      * @group Builders */
    implicit def MapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
      : Builder[(A, T)] { type Scope = Map[_, _]; type State = Map[A, T] } =
      Map.Builder(A, T)
  }

  private[containers] abstract class Library5 extends Library6 {
    /** The default [[Index]] factory.
      * @group Collections */
    val Index: SeqFactory[Index] = ArraySeq
    
    /** The default [[Stack]] factory.
      * @group Collections */
    val Stack: SeqFactory[Stack] = List
    
    /** Implicitly returns a new default [[Index]] builder.
      * @group Builders */
    implicit def IndexBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Index[_]; type State = Index[A] } =
      Index.Builder(A)
    
    /** Implicitly returns a new default [[Stack]] builder.
      * @group Builders */
    implicit def StackBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Stack[_]; type State = Stack[A] } =
      Stack.Builder[A]
  }
  
  private[containers] abstract class Library6 {
    /** Implicitly returns a new `Array` builder.
      * @group Builders */
    implicit def ArrayBuilder[A](implicit A: ClassTag[A])
      : ArrayBuilder[A] { type Scope = Array[_]; type State = Array[A] } = (A match {
      case ClassTag.Byte    => new ByteArrayBuilder
      case ClassTag.Short   => new ShortArrayBuilder
      case ClassTag.Int     => new IntArrayBuilder
      case ClassTag.Long    => new LongArrayBuilder
      case ClassTag.Float   => new FloatArrayBuilder
      case ClassTag.Double  => new DoubleArrayBuilder
      case _                => new RefArrayBuilder[A]
    }).asInstanceOf[ArrayBuilder[A] { type Scope = Array[_]; type State = Array[A] }]
    
    /** Implicitly returns a new [[ArraySeq]] builder.
      * @group Builders */
    implicit def ArraySeqBuilder[A](implicit A: TypeHint[A])
      : ArrayBuilder[A] { type Scope = ArraySeq[_]; type State = ArraySeq[A] } =
      ArraySeq.Builder(A)
    
    /** Implicitly returns a new [[ArrayBuffer]] builder.
      * @group Builders */
    implicit def ArrayBufferBuilder[A](implicit A: TypeHint[A])
      : ArrayBuilder[A] { type Scope = ArrayBuffer[_]; type State = ArrayBuffer[A] } =
      ArrayBuffer.Builder(A)
    
    /** Implicitly returns a new [[List]] builder.
      * @group Builders */
    implicit def ListBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = List[_]; type State = List[A] } =
      new ListBuilder[A]
    
    /** Implicitly returns a new [[ListBuffer]] builder.
      * @group Builders */
    implicit def ListBufferBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = ListBuffer[_]; type State = ListBuffer[A] } =
      new ListBufferBuilder[A]
    
    /** Implicitly returns a new [[Vector]] builder.
      * @group Builders */
    implicit def VectorBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = Vector[_]; type State = Vector[A] } =
      new VectorBuilder[A]
    
    /** Implicitly returns a new [[HashSet]] builder.
      * @group Builders */
    implicit def HashSetBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = HashSet[_]; type State = HashSet[A] } =
      new HashSetBuilder[A]
    
    /** Implicitly returns a new [[HashMap]] builder.
      * @group Builders */
    implicit def HashMapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
      : Builder[(A, T)] { type Scope = HashMap[_, _]; type State = HashMap[A, T] } =
      new HashMapBuilder[A, T]
    
    /** Implicitly returns a new [[ArraySet]] builder.
      * @group Builders */
    private[containers] implicit def ArraySetBuilder[A](implicit A: TypeHint[A])
      : Builder[A] { type Scope = ArraySet[_]; type State = ArraySet[A] } =
      new ArraySetBuilder[A]
    
    /** Implicitly returns a new [[ArrayMap]] builder.
      * @group Builders */
    private[containers] implicit def ArrayMapBuilder[A, T](implicit A: TypeHint[A], T: TypeHint[T])
      : Builder[(A, T)] { type Scope = ArrayMap[_, _]; type State = ArrayMap[A, T] } =
      new ArrayMapBuilder[A, T]
  }
}
