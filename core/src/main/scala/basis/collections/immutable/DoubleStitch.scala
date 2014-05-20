//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import scala.annotation._

private[collections] final class DoubleStitch1(_1: Double) extends Stitch[Double] {
  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def apply(index: Int): Double = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (index == 0) new DoubleStitch1(elem.asInstanceOf[Double])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Stitch[Double] = Stitch.empty

  override def body: Stitch[Double] = Stitch.empty

  override def foot: Double = _1

  override def drop(lower: Int): Stitch[Double] = if (lower <= 0) this else Stitch.empty

  override def take(upper: Int): Stitch[Double] = if (upper <= 0) Stitch.empty else this

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch2(_1, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch2(elem.asInstanceOf[Double], _1)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = f(_1)

  private[this] def lifted: Stitch[Double] = new RefStitch1(_1)
}

private[collections] final class DoubleStitch2(_1: Double, _2: Double) extends Stitch[Double] {
  override def isEmpty: Boolean = false

  override def length: Int = 2

  override def apply(index: Int): Double = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (index == 0) new DoubleStitch2(elem.asInstanceOf[Double], _2)
      else if (index == 1) new DoubleStitch2(_1, elem.asInstanceOf[Double])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Stitch[Double] = new DoubleStitch1(_2)

  override def body: Stitch[Double] = new DoubleStitch1(_1)

  override def foot: Double = _2

  override def drop(lower: Int): Stitch[Double] = {
    if (lower <= 0) this
    else if (lower == 1) new DoubleStitch1(_2)
    else Stitch.empty
  }

  override def take(upper: Int): Stitch[Double] = {
    if (upper <= 0) Stitch.empty
    else if (upper == 1) new DoubleStitch1(_1)
    else this
  }

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch3(_1, _2, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch3(elem.asInstanceOf[Double], _1, _2)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2)
  }

  private[this] def lifted: Stitch[Double] = new RefStitch2(_1, _2)
}

private[collections] final class DoubleStitch3(_1: Double, _2: Double, _3: Double) extends Stitch[Double] {
  override def isEmpty: Boolean = false

  override def length: Int = 3

  override def apply(index: Int): Double = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleStitch3(elem.asInstanceOf[Double], _2, _3)
      case 1 => new DoubleStitch3(_1, elem.asInstanceOf[Double], _3)
      case 2 => new DoubleStitch3(_1, _2, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Stitch[Double] = new DoubleStitch2(_2, _3)

  override def body: Stitch[Double] = new DoubleStitch2(_1, _2)

  override def foot: Double = _3

  @tailrec override def drop(lower: Int): Stitch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleStitch2(_2, _3)
    case 2 => new DoubleStitch1(_3)
    case 3 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }

  @tailrec override def take(upper: Int): Stitch[Double] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new DoubleStitch1(_1)
    case 2 => new DoubleStitch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch4(_1, _2, _3, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch4(elem.asInstanceOf[Double], _1, _2, _3)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3)
  }

  private[this] def lifted: Stitch[Double] = new RefStitch3(_1, _2, _3)
}

private[collections] final class DoubleStitch4
    (_1: Double, _2: Double, _3: Double, _4: Double)
  extends Stitch[Double] {

  override def isEmpty: Boolean = false

  override def length: Int = 4

  override def apply(index: Int): Double = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleStitch4(elem.asInstanceOf[Double], _2, _3, _4)
      case 1 => new DoubleStitch4(_1, elem.asInstanceOf[Double], _3, _4)
      case 2 => new DoubleStitch4(_1, _2, elem.asInstanceOf[Double], _4)
      case 3 => new DoubleStitch4(_1, _2, _3, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Stitch[Double] = new DoubleStitch3(_2, _3, _4)

  override def body: Stitch[Double] = new DoubleStitch3(_1, _2, _3)

  override def foot: Double = _4

  @tailrec override def drop(lower: Int): Stitch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleStitch3(_2, _3, _4)
    case 2 => new DoubleStitch2(_3, _4)
    case 3 => new DoubleStitch1(_4)
    case 4 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }

  @tailrec override def take(upper: Int): Stitch[Double] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new DoubleStitch1(_1)
    case 2 => new DoubleStitch2(_1, _2)
    case 3 => new DoubleStitch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch5(_1, _2, _3, _4, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch5(elem.asInstanceOf[Double], _1, _2, _3, _4)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4)
  }

  private[this] def lifted: Stitch[Double] = new RefStitch4(_1, _2, _3, _4)
}

private[collections] final class DoubleStitch5
    (_1: Double, _2: Double, _3: Double, _4: Double, _5: Double)
  extends Stitch[Double] {

  override def isEmpty: Boolean = false

  override def length: Int = 5

  override def apply(index: Int): Double = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleStitch5(elem.asInstanceOf[Double], _2, _3, _4, _5)
      case 1 => new DoubleStitch5(_1, elem.asInstanceOf[Double], _3, _4, _5)
      case 2 => new DoubleStitch5(_1, _2, elem.asInstanceOf[Double], _4, _5)
      case 3 => new DoubleStitch5(_1, _2, _3, elem.asInstanceOf[Double], _5)
      case 4 => new DoubleStitch5(_1, _2, _3, _4, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Stitch[Double] = new DoubleStitch4(_2, _3, _4, _5)

  override def body: Stitch[Double] = new DoubleStitch4(_1, _2, _3, _4)

  override def foot: Double = _5

  @tailrec override def drop(lower: Int): Stitch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleStitch4(_2, _3, _4, _5)
    case 2 => new DoubleStitch3(_3, _4, _5)
    case 3 => new DoubleStitch2(_4, _5)
    case 4 => new DoubleStitch1(_5)
    case 5 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }

  @tailrec override def take(upper: Int): Stitch[Double] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new DoubleStitch1(_1)
    case 2 => new DoubleStitch2(_1, _2)
    case 3 => new DoubleStitch3(_1, _2, _3)
    case 4 => new DoubleStitch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleStitch6(elem.asInstanceOf[Double], _1, _2, _3, _4, _5)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }

  private[this] def lifted: Stitch[Double] = new RefStitch5(_1, _2, _3, _4, _5)
}

private[collections] final class DoubleStitch6
    (_1: Double, _2: Double, _3: Double, _4: Double, _5: Double, _6: Double)
  extends Stitch[Double] {

  override def isEmpty: Boolean = false

  override def length: Int = 6

  override def apply(index: Int): Double = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleStitch6(elem.asInstanceOf[Double], _2, _3, _4, _5, _6)
      case 1 => new DoubleStitch6(_1, elem.asInstanceOf[Double], _3, _4, _5, _6)
      case 2 => new DoubleStitch6(_1, _2, elem.asInstanceOf[Double], _4, _5, _6)
      case 3 => new DoubleStitch6(_1, _2, _3, elem.asInstanceOf[Double], _5, _6)
      case 4 => new DoubleStitch6(_1, _2, _3, _4, elem.asInstanceOf[Double], _6)
      case 5 => new DoubleStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Stitch[Double] = new DoubleStitch5(_2, _3, _4, _5, _6)

  override def body: Stitch[Double] = new DoubleStitch5(_1, _2, _3, _4, _5)

  override def foot: Double = _6

  @tailrec override def drop(lower: Int): Stitch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleStitch5(_2, _3, _4, _5, _6)
    case 2 => new DoubleStitch4(_3, _4, _5, _6)
    case 3 => new DoubleStitch3(_4, _5, _6)
    case 4 => new DoubleStitch2(_5, _6)
    case 5 => new DoubleStitch1(_6)
    case 6 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }

  @tailrec override def take(upper: Int): Stitch[Double] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new DoubleStitch1(_1)
    case 2 => new DoubleStitch2(_1, _2)
    case 3 => new DoubleStitch3(_1, _2, _3)
    case 4 => new DoubleStitch4(_1, _2, _3, _4)
    case 5 => new DoubleStitch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double])
      new DoubleStitchN(7, new DoubleStitch4(_1, _2, _3, _4), Stitch.empty, new DoubleStitch3(_5, _6, elem.asInstanceOf[Double]))
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double])
      new DoubleStitchN(7, new DoubleStitch3(elem.asInstanceOf[Double], _1, _2), Stitch.empty, new DoubleStitch4(_3, _4, _5, _6))
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }

  private[this] def lifted: Stitch[Double] = new RefStitch6(_1, _2, _3, _4, _5, _6)
}

private[collections] final class DoubleStitchN
    (override val length: Int, prefix: Stitch[Double], tree: Stitch[Stitch[Double]], suffix: Stitch[Double])
  extends Stitch[Double] {

  override def isEmpty: Boolean = false

  override def apply(index: Int): Double = {
    val n = index - prefix.length
    if (n < 0) prefix(index)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) tree(n >> 2)(n & 3)
      else suffix(k)
    }
  }

  override def update[B >: Double](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) {
      val n = index - prefix.length
      if (n < 0) new DoubleStitchN(length, prefix.update(index, elem).asInstanceOf[Stitch[Double]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new DoubleStitchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Stitch[Double]]),
            suffix)
        else new DoubleStitchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Stitch[Double]])
      }
    }
    else lifted.update(index, elem)
  }

  override def head: Double = prefix.head

  override def tail: Stitch[Double] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new DoubleStitchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new DoubleStitchN(length - 1, prefix.tail, tree, suffix)
  }

  override def body: Stitch[Double] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new DoubleStitchN(length - 1, prefix, tree.body, tree.foot)
    }
    else new DoubleStitchN(length - 1, prefix, tree, suffix.body)
  }

  override def foot: Double = suffix.foot

  override def drop(lower: Int): Stitch[Double] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new DoubleStitchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new DoubleStitchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }

  override def take(upper: Int): Stitch[Double] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new DoubleStitchN(upper, prefix, split.body, split.foot.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new DoubleStitchN(upper, prefix, tree, suffix.take(k))
    }
  }

  override def :+ [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (suffix.length == 6)
        new DoubleStitchN(
          length + 1,
          prefix,
          tree :+ new DoubleStitch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new DoubleStitch3(suffix(4), suffix(5), elem.asInstanceOf[Double]))
      else new DoubleStitchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Stitch[Double]])
    }
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (prefix.length == 6)
        new DoubleStitchN(
          length + 1,
          new DoubleStitch3(elem.asInstanceOf[Double], prefix(0), prefix(1)),
          new DoubleStitch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new DoubleStitchN(length + 1, (elem +: prefix).asInstanceOf[Stitch[Double]], tree, suffix)
    }
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    prefix.traverse(f)
    tree.flatTraverse(f)
    suffix.traverse(f)
  }

  private[this] def lifted: Stitch[Double] = new RefStitchN(length, prefix, tree, suffix)
}
