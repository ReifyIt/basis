/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

import basis.collections._
import basis.runtime._

/** Specialized collection implementations. */
package object containers extends Library {
  override val Enumerator = List
  
  override val Collection = List
  
  override val Container = List
  
  override val Seq = List
  
  override val Set = HashSet
  
  override val Map = HashMap
  
  override val Index = ArraySeq
  
  override val Stack = List
  
  /** Implicitly returns a new [[ArraySeq]] builder. */
  implicit def ArraySeqBuilder[A](implicit A: TypeHint[A])
    : Builder[ArraySeq[_], A] { type State = ArraySeq[A] } =
    ArraySeq.Builder(A)
  
  /** Implicitly returns a new [[ArrayBuffer]] builder. */
  implicit def ArrayBufferBuilder[A](implicit A: TypeHint[A])
    : Builder[ArrayBuffer[_], A] { type State = ArrayBuffer[A] } =
    ArrayBuffer.Builder(A)
  
  /** Implicitly returns a new [[List]] builder. */
  implicit def ListBuilder[A]
    : Builder[List[_], A] { type State = List[A] } =
    new ListBuilder
  
  /** Implicitly returns a new [[ListBuffer]] builder. */
  implicit def ListBufferBuilder[A]
    : Builder[ListBuffer[_], A] { type State = ListBuffer[A] } =
    new ListBufferBuilder
  
  /** Implicitly returns a new [[Vector]] builder. */
  implicit def VectorBuilder[A]
    : Builder[Vector[_], A] { type State = Vector[A] } =
    new VectorBuilder
  
  /** Implicitly returns a new [[HashSet]] builder. */
  implicit def HashSetBuilder[A]
    : Builder[HashSet[_], A] { type State = HashSet[A] } =
    new HashSetBuilder
  
  /** Implicitly returns a new [[HashMap]] builder. */
  implicit def HashMapBuilder[A, T]
    : Builder[HashMap[_, _], (A, T)] { type State = HashMap[A, T] } =
    new HashMapBuilder
  
  /** Implicitly returns a new [[ArrayMap]] builder. */
  private[containers] implicit def ArrayMapBuilder[A, T]
    : Builder[ArrayMap[_, _], (A, T)] { type State = ArrayMap[A, T] } =
    new ArrayMapBuilder
  
  /** Implicitly returns a new [[ArraySet]] builder. */
  private[containers] implicit def ArraySetBuilder[A]
    : Builder[ArraySet[_], A] { type State = ArraySet[A] } =
    new ArraySetBuilder
}
