//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import scala.annotation._

private[collections] final class DoubleBatch1(_1: Double) extends Batch[Double] {
  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def apply(index: Int): Double = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (index == 0) new DoubleBatch1(elem.asInstanceOf[Double])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Batch[Double] = Batch.empty

  override def body: Batch[Double] = Batch.empty

  override def foot: Double = _1

  override def drop(lower: Int): Batch[Double] = if (lower <= 0) this else Batch.empty

  override def take(upper: Int): Batch[Double] = if (upper <= 0) Batch.empty else this

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch2(_1, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch2(elem.asInstanceOf[Double], _1)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = f(_1)

  private[this] def lifted: Batch[Double] = new RefBatch1(_1)
}

private[collections] final class DoubleBatch2(_1: Double, _2: Double) extends Batch[Double] {
  override def isEmpty: Boolean = false

  override def length: Int = 2

  override def apply(index: Int): Double = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (index == 0) new DoubleBatch2(elem.asInstanceOf[Double], _2)
      else if (index == 1) new DoubleBatch2(_1, elem.asInstanceOf[Double])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Batch[Double] = new DoubleBatch1(_2)

  override def body: Batch[Double] = new DoubleBatch1(_1)

  override def foot: Double = _2

  override def drop(lower: Int): Batch[Double] = {
    if (lower <= 0) this
    else if (lower == 1) new DoubleBatch1(_2)
    else Batch.empty
  }

  override def take(upper: Int): Batch[Double] = {
    if (upper <= 0) Batch.empty
    else if (upper == 1) new DoubleBatch1(_1)
    else this
  }

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch3(_1, _2, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch3(elem.asInstanceOf[Double], _1, _2)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2)
  }

  private[this] def lifted: Batch[Double] = new RefBatch2(_1, _2)
}

private[collections] final class DoubleBatch3(_1: Double, _2: Double, _3: Double) extends Batch[Double] {
  override def isEmpty: Boolean = false

  override def length: Int = 3

  override def apply(index: Int): Double = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleBatch3(elem.asInstanceOf[Double], _2, _3)
      case 1 => new DoubleBatch3(_1, elem.asInstanceOf[Double], _3)
      case 2 => new DoubleBatch3(_1, _2, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Batch[Double] = new DoubleBatch2(_2, _3)

  override def body: Batch[Double] = new DoubleBatch2(_1, _2)

  override def foot: Double = _3

  @tailrec override def drop(lower: Int): Batch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleBatch2(_2, _3)
    case 2 => new DoubleBatch1(_3)
    case 3 => Batch.empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }

  @tailrec override def take(upper: Int): Batch[Double] = (upper: @switch) match {
    case 0 => Batch.empty
    case 1 => new DoubleBatch1(_1)
    case 2 => new DoubleBatch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch4(_1, _2, _3, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch4(elem.asInstanceOf[Double], _1, _2, _3)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3)
  }

  private[this] def lifted: Batch[Double] = new RefBatch3(_1, _2, _3)
}

private[collections] final class DoubleBatch4
    (_1: Double, _2: Double, _3: Double, _4: Double)
  extends Batch[Double] {

  override def isEmpty: Boolean = false

  override def length: Int = 4

  override def apply(index: Int): Double = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleBatch4(elem.asInstanceOf[Double], _2, _3, _4)
      case 1 => new DoubleBatch4(_1, elem.asInstanceOf[Double], _3, _4)
      case 2 => new DoubleBatch4(_1, _2, elem.asInstanceOf[Double], _4)
      case 3 => new DoubleBatch4(_1, _2, _3, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Batch[Double] = new DoubleBatch3(_2, _3, _4)

  override def body: Batch[Double] = new DoubleBatch3(_1, _2, _3)

  override def foot: Double = _4

  @tailrec override def drop(lower: Int): Batch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleBatch3(_2, _3, _4)
    case 2 => new DoubleBatch2(_3, _4)
    case 3 => new DoubleBatch1(_4)
    case 4 => Batch.empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }

  @tailrec override def take(upper: Int): Batch[Double] = (upper: @switch) match {
    case 0 => Batch.empty
    case 1 => new DoubleBatch1(_1)
    case 2 => new DoubleBatch2(_1, _2)
    case 3 => new DoubleBatch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch5(_1, _2, _3, _4, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch5(elem.asInstanceOf[Double], _1, _2, _3, _4)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4)
  }

  private[this] def lifted: Batch[Double] = new RefBatch4(_1, _2, _3, _4)
}

private[collections] final class DoubleBatch5
    (_1: Double, _2: Double, _3: Double, _4: Double, _5: Double)
  extends Batch[Double] {

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

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleBatch5(elem.asInstanceOf[Double], _2, _3, _4, _5)
      case 1 => new DoubleBatch5(_1, elem.asInstanceOf[Double], _3, _4, _5)
      case 2 => new DoubleBatch5(_1, _2, elem.asInstanceOf[Double], _4, _5)
      case 3 => new DoubleBatch5(_1, _2, _3, elem.asInstanceOf[Double], _5)
      case 4 => new DoubleBatch5(_1, _2, _3, _4, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Batch[Double] = new DoubleBatch4(_2, _3, _4, _5)

  override def body: Batch[Double] = new DoubleBatch4(_1, _2, _3, _4)

  override def foot: Double = _5

  @tailrec override def drop(lower: Int): Batch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleBatch4(_2, _3, _4, _5)
    case 2 => new DoubleBatch3(_3, _4, _5)
    case 3 => new DoubleBatch2(_4, _5)
    case 4 => new DoubleBatch1(_5)
    case 5 => Batch.empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }

  @tailrec override def take(upper: Int): Batch[Double] = (upper: @switch) match {
    case 0 => Batch.empty
    case 1 => new DoubleBatch1(_1)
    case 2 => new DoubleBatch2(_1, _2)
    case 3 => new DoubleBatch3(_1, _2, _3)
    case 4 => new DoubleBatch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Double])
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) new DoubleBatch6(elem.asInstanceOf[Double], _1, _2, _3, _4, _5)
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }

  private[this] def lifted: Batch[Double] = new RefBatch5(_1, _2, _3, _4, _5)
}

private[collections] final class DoubleBatch6
    (_1: Double, _2: Double, _3: Double, _4: Double, _5: Double, _6: Double)
  extends Batch[Double] {

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

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) (index: @switch) match {
      case 0 => new DoubleBatch6(elem.asInstanceOf[Double], _2, _3, _4, _5, _6)
      case 1 => new DoubleBatch6(_1, elem.asInstanceOf[Double], _3, _4, _5, _6)
      case 2 => new DoubleBatch6(_1, _2, elem.asInstanceOf[Double], _4, _5, _6)
      case 3 => new DoubleBatch6(_1, _2, _3, elem.asInstanceOf[Double], _5, _6)
      case 4 => new DoubleBatch6(_1, _2, _3, _4, elem.asInstanceOf[Double], _6)
      case 5 => new DoubleBatch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Double])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Double = _1

  override def tail: Batch[Double] = new DoubleBatch5(_2, _3, _4, _5, _6)

  override def body: Batch[Double] = new DoubleBatch5(_1, _2, _3, _4, _5)

  override def foot: Double = _6

  @tailrec override def drop(lower: Int): Batch[Double] = (lower: @switch) match {
    case 0 => this
    case 1 => new DoubleBatch5(_2, _3, _4, _5, _6)
    case 2 => new DoubleBatch4(_3, _4, _5, _6)
    case 3 => new DoubleBatch3(_4, _5, _6)
    case 4 => new DoubleBatch2(_5, _6)
    case 5 => new DoubleBatch1(_6)
    case 6 => Batch.empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }

  @tailrec override def take(upper: Int): Batch[Double] = (upper: @switch) match {
    case 0 => Batch.empty
    case 1 => new DoubleBatch1(_1)
    case 2 => new DoubleBatch2(_1, _2)
    case 3 => new DoubleBatch3(_1, _2, _3)
    case 4 => new DoubleBatch4(_1, _2, _3, _4)
    case 5 => new DoubleBatch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double])
      new DoubleBatchN(7, new DoubleBatch4(_1, _2, _3, _4), Batch.empty, new DoubleBatch3(_5, _6, elem.asInstanceOf[Double]))
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double])
      new DoubleBatchN(7, new DoubleBatch3(elem.asInstanceOf[Double], _1, _2), Batch.empty, new DoubleBatch4(_3, _4, _5, _6))
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }

  private[this] def lifted: Batch[Double] = new RefBatch6(_1, _2, _3, _4, _5, _6)
}

private[collections] final class DoubleBatchN
    (override val length: Int, prefix: Batch[Double], tree: Batch[Batch[Double]], suffix: Batch[Double])
  extends Batch[Double] {

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

  override def update[B >: Double](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) {
      val n = index - prefix.length
      if (n < 0) new DoubleBatchN(length, prefix.update(index, elem).asInstanceOf[Batch[Double]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new DoubleBatchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Batch[Double]]),
            suffix)
        else new DoubleBatchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Batch[Double]])
      }
    }
    else lifted.update(index, elem)
  }

  override def head: Double = prefix.head

  override def tail: Batch[Double] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new DoubleBatchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new DoubleBatchN(length - 1, prefix.tail, tree, suffix)
  }

  override def body: Batch[Double] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new DoubleBatchN(length - 1, prefix, tree.body, tree.foot)
    }
    else new DoubleBatchN(length - 1, prefix, tree, suffix.body)
  }

  override def foot: Double = suffix.foot

  override def drop(lower: Int): Batch[Double] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new DoubleBatchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new DoubleBatchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }

  override def take(upper: Int): Batch[Double] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new DoubleBatchN(upper, prefix, split.body, split.foot.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new DoubleBatchN(upper, prefix, tree, suffix.take(k))
    }
  }

  override def :+ [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (suffix.length == 6)
        new DoubleBatchN(
          length + 1,
          prefix,
          tree :+ new DoubleBatch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new DoubleBatch3(suffix(4), suffix(5), elem.asInstanceOf[Double]))
      else new DoubleBatchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Batch[Double]])
    }
    else lifted :+ elem
  }

  override def +: [B >: Double](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Double]) {
      if (prefix.length == 6)
        new DoubleBatchN(
          length + 1,
          new DoubleBatch3(elem.asInstanceOf[Double], prefix(0), prefix(1)),
          new DoubleBatch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new DoubleBatchN(length + 1, (elem +: prefix).asInstanceOf[Batch[Double]], tree, suffix)
    }
    else elem +: lifted
  }

  override def traverse(f: Double => Unit): Unit = {
    prefix.traverse(f)
    tree.flatTraverse(f)
    suffix.traverse(f)
  }

  private[this] def lifted: Batch[Double] = new RefBatchN(length, prefix, tree, suffix)
}
