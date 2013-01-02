/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** General collection interfaces.
  * 
  * @contentDiagram hideNodes "*Factory" "*Family"
  */
package object collections {
  /** Applies a function to each element of an enumerator by invoking its
    * protected `foreach` method. */
  def traverse[A, U](xs: Enumerator[A])(f: A => U): Unit =
    Enumerator.traverse[A, U](xs)(f)
}
