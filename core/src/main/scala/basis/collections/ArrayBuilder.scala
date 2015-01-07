//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

/** An array-like collection builder. */
trait ArrayBuilder[A] extends Builder[A] {
  override def expect(count: Int): this.type

  def appendArray(elems: Array[A]): Unit = {
    var i = 0
    val n = elems.length
    while (i < n) {
      append(elems(i))
      i += 1
    }
  }

  def ++= (elems: Array[A]): this.type = {
    appendArray(elems)
    this
  }
}
