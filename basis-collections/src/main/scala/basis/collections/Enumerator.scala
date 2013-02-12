/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A once traversable collection. Traversing an enumerator may alter its
  * state, causing subsequent traversals to yield different results.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Collections
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
trait Enumerator[+A] extends Any with Family[Enumerator[_]] {
  /** Applies a side-effecting function to each element of this $collection.
    * @group Traversing */
  def traverse(f: A => Unit): Unit
}
