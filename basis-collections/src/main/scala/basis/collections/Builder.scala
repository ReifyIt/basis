//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

trait Builder[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) -A] extends From[Nothing] with State[Any] {
  def expect(count: Int): this.type = this

  def append(elem: A): Unit

  def appendAll(elems: Traverser[A]): Unit =
    elems.traverse(new Buffer.Append(this))

  def += (elem: A): this.type = {
    append(elem)
    this
  }

  def ++= (elems: Traverser[A]): this.type = {
    appendAll(elems)
    this
  }

  def state: State

  def clear(): Unit
}
