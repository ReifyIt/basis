//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A once traversable collection of elements.
  *
  * @define collection  traverser
  */
trait Traverser[+A] extends Any with Family[Traverser[_]] {
  def traverse(f: A => Unit): Unit
}
