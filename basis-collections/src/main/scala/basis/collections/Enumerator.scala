/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A stateful traversable collection. `Enumerator` declares a protected
  * `foreach` method that traverses its elements. To traverse an enumerator,
  * invoke [[basis.collections#traverse basis.collections.traverse]], or add
  * a public `foreach` implementation by importing
  * [[basis.sequential.General `basis.sequential.general`]]`._`. Traversing
  * an enumerator may alter its state, causing subsequent traversals to yield
  * different results.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @groupprio  Traversing    1
  * @groupprio  Classifying   2
  * 
  * @define collection  enumerator
  * @define Extensions
  * Add operations to this interface by importing one of these use cases:
  * 
  *  - `import `[[basis.sequential.General `basis.sequential.general`]]`._`
  *   - adds reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - `import `[[basis.sequential.Strict `basis.sequential.strict`]]`._`
  *   - adds reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *   - adds eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - `import `[[basis.sequential.NonStrict `basis.sequential.nonstrict`]]`._`
  *   - adds reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *   - adds lazy transformations (`map`, `flatMap`, `filter`, etc.).
  * 
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralEnumeratorOps GeneralEnumeratorOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictEnumeratorOps StrictEnumeratorOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictEnumeratorOps NonStrictEnumeratorOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Enumerator[+A] extends Any with Family[Enumerator[A]] {
  /** Applies a function to each element of this $collection. The protected
    * status of `foreach` allows optimized static implementations to shadow
    * this virtual method. To force a virtual `foreach` call, invoke
    * [[basis.collections#traverse traverse]].
    * 
    * @group  Traversing
    */
  protected def foreach[U](f: A => U): Unit
}

private[collections] object Enumerator {
  /** Applies a function to each of an enumerator's elements by invoking the
    * enumerator's protected `foreach` method. */
  def traverse[A, U](xs: Enumerator[A])(f: A => U): Unit = xs.foreach[U](f)
}
