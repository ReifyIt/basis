//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import scala.annotation._

private[collections] final class FloatStitch1(_1: Float) extends Stitch[Float] {
  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def apply(index: Int): Float = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (index == 0) new FloatStitch1(elem.asInstanceOf[Float])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Float = _1

  override def tail: Stitch[Float] = Stitch.empty

  override def body: Stitch[Float] = Stitch.empty

  override def foot: Float = _1

  override def drop(lower: Int): Stitch[Float] = if (lower <= 0) this else Stitch.empty

  override def take(upper: Int): Stitch[Float] = if (upper <= 0) Stitch.empty else this

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch2(_1, elem.asInstanceOf[Float])
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch2(elem.asInstanceOf[Float], _1)
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = f(_1)

  private[this] def lifted: Stitch[Float] = new RefStitch1(_1)
}

private[collections] final class FloatStitch2(_1: Float, _2: Float) extends Stitch[Float] {
  override def isEmpty: Boolean = false

  override def length: Int = 2

  override def apply(index: Int): Float = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (index == 0) new FloatStitch2(elem.asInstanceOf[Float], _2)
      else if (index == 1) new FloatStitch2(_1, elem.asInstanceOf[Float])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Float = _1

  override def tail: Stitch[Float] = new FloatStitch1(_2)

  override def body: Stitch[Float] = new FloatStitch1(_1)

  override def foot: Float = _2

  override def drop(lower: Int): Stitch[Float] = {
    if (lower <= 0) this
    else if (lower == 1) new FloatStitch1(_2)
    else Stitch.empty
  }

  override def take(upper: Int): Stitch[Float] = {
    if (upper <= 0) Stitch.empty
    else if (upper == 1) new FloatStitch1(_1)
    else this
  }

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch3(_1, _2, elem.asInstanceOf[Float])
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch3(elem.asInstanceOf[Float], _1, _2)
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = {
    f(_1); f(_2)
  }

  private[this] def lifted: Stitch[Float] = new RefStitch2(_1, _2)
}

private[collections] final class FloatStitch3(_1: Float, _2: Float, _3: Float) extends Stitch[Float] {
  override def isEmpty: Boolean = false

  override def length: Int = 3

  override def apply(index: Int): Float = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatStitch3(elem.asInstanceOf[Float], _2, _3)
      case 1 => new FloatStitch3(_1, elem.asInstanceOf[Float], _3)
      case 2 => new FloatStitch3(_1, _2, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Float = _1

  override def tail: Stitch[Float] = new FloatStitch2(_2, _3)

  override def body: Stitch[Float] = new FloatStitch2(_1, _2)

  override def foot: Float = _3

  @tailrec override def drop(lower: Int): Stitch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatStitch2(_2, _3)
    case 2 => new FloatStitch1(_3)
    case 3 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }

  @tailrec override def take(upper: Int): Stitch[Float] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new FloatStitch1(_1)
    case 2 => new FloatStitch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch4(_1, _2, _3, elem.asInstanceOf[Float])
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch4(elem.asInstanceOf[Float], _1, _2, _3)
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = {
    f(_1); f(_2); f(_3)
  }

  private[this] def lifted: Stitch[Float] = new RefStitch3(_1, _2, _3)
}

private[collections] final class FloatStitch4
    (_1: Float, _2: Float, _3: Float, _4: Float)
  extends Stitch[Float] {

  override def isEmpty: Boolean = false

  override def length: Int = 4

  override def apply(index: Int): Float = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatStitch4(elem.asInstanceOf[Float], _2, _3, _4)
      case 1 => new FloatStitch4(_1, elem.asInstanceOf[Float], _3, _4)
      case 2 => new FloatStitch4(_1, _2, elem.asInstanceOf[Float], _4)
      case 3 => new FloatStitch4(_1, _2, _3, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Float = _1

  override def tail: Stitch[Float] = new FloatStitch3(_2, _3, _4)

  override def body: Stitch[Float] = new FloatStitch3(_1, _2, _3)

  override def foot: Float = _4

  @tailrec override def drop(lower: Int): Stitch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatStitch3(_2, _3, _4)
    case 2 => new FloatStitch2(_3, _4)
    case 3 => new FloatStitch1(_4)
    case 4 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }

  @tailrec override def take(upper: Int): Stitch[Float] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new FloatStitch1(_1)
    case 2 => new FloatStitch2(_1, _2)
    case 3 => new FloatStitch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch5(_1, _2, _3, _4, elem.asInstanceOf[Float])
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch5(elem.asInstanceOf[Float], _1, _2, _3, _4)
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4)
  }

  private[this] def lifted: Stitch[Float] = new RefStitch4(_1, _2, _3, _4)
}

private[collections] final class FloatStitch5
    (_1: Float, _2: Float, _3: Float, _4: Float, _5: Float)
  extends Stitch[Float] {

  override def isEmpty: Boolean = false

  override def length: Int = 5

  override def apply(index: Int): Float = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatStitch5(elem.asInstanceOf[Float], _2, _3, _4, _5)
      case 1 => new FloatStitch5(_1, elem.asInstanceOf[Float], _3, _4, _5)
      case 2 => new FloatStitch5(_1, _2, elem.asInstanceOf[Float], _4, _5)
      case 3 => new FloatStitch5(_1, _2, _3, elem.asInstanceOf[Float], _5)
      case 4 => new FloatStitch5(_1, _2, _3, _4, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Float = _1

  override def tail: Stitch[Float] = new FloatStitch4(_2, _3, _4, _5)

  override def body: Stitch[Float] = new FloatStitch4(_1, _2, _3, _4)

  override def foot: Float = _5

  @tailrec override def drop(lower: Int): Stitch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatStitch4(_2, _3, _4, _5)
    case 2 => new FloatStitch3(_3, _4, _5)
    case 3 => new FloatStitch2(_4, _5)
    case 4 => new FloatStitch1(_5)
    case 5 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }

  @tailrec override def take(upper: Int): Stitch[Float] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new FloatStitch1(_1)
    case 2 => new FloatStitch2(_1, _2)
    case 3 => new FloatStitch3(_1, _2, _3)
    case 4 => new FloatStitch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Float])
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) new FloatStitch6(elem.asInstanceOf[Float], _1, _2, _3, _4, _5)
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }

  private[this] def lifted: Stitch[Float] = new RefStitch5(_1, _2, _3, _4, _5)
}

private[collections] final class FloatStitch6
    (_1: Float, _2: Float, _3: Float, _4: Float, _5: Float, _6: Float)
  extends Stitch[Float] {

  override def isEmpty: Boolean = false

  override def length: Int = 6

  override def apply(index: Int): Float = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatStitch6(elem.asInstanceOf[Float], _2, _3, _4, _5, _6)
      case 1 => new FloatStitch6(_1, elem.asInstanceOf[Float], _3, _4, _5, _6)
      case 2 => new FloatStitch6(_1, _2, elem.asInstanceOf[Float], _4, _5, _6)
      case 3 => new FloatStitch6(_1, _2, _3, elem.asInstanceOf[Float], _5, _6)
      case 4 => new FloatStitch6(_1, _2, _3, _4, elem.asInstanceOf[Float], _6)
      case 5 => new FloatStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Float = _1

  override def tail: Stitch[Float] = new FloatStitch5(_2, _3, _4, _5, _6)

  override def body: Stitch[Float] = new FloatStitch5(_1, _2, _3, _4, _5)

  override def foot: Float = _6

  @tailrec override def drop(lower: Int): Stitch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatStitch5(_2, _3, _4, _5, _6)
    case 2 => new FloatStitch4(_3, _4, _5, _6)
    case 3 => new FloatStitch3(_4, _5, _6)
    case 4 => new FloatStitch2(_5, _6)
    case 5 => new FloatStitch1(_6)
    case 6 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }

  @tailrec override def take(upper: Int): Stitch[Float] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new FloatStitch1(_1)
    case 2 => new FloatStitch2(_1, _2)
    case 3 => new FloatStitch3(_1, _2, _3)
    case 4 => new FloatStitch4(_1, _2, _3, _4)
    case 5 => new FloatStitch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float])
      new FloatStitchN(7, new FloatStitch4(_1, _2, _3, _4), Stitch.empty, new FloatStitch3(_5, _6, elem.asInstanceOf[Float]))
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float])
      new FloatStitchN(7, new FloatStitch3(elem.asInstanceOf[Float], _1, _2), Stitch.empty, new FloatStitch4(_3, _4, _5, _6))
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }

  private[this] def lifted: Stitch[Float] = new RefStitch6(_1, _2, _3, _4, _5, _6)
}

private[collections] final class FloatStitchN
    (override val length: Int, prefix: Stitch[Float], tree: Stitch[Stitch[Float]], suffix: Stitch[Float])
  extends Stitch[Float] {

  override def isEmpty: Boolean = false

  override def apply(index: Int): Float = {
    val n = index - prefix.length
    if (n < 0) prefix(index)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) tree(n >> 2)(n & 3)
      else suffix(k)
    }
  }

  override def update[B >: Float](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) {
      val n = index - prefix.length
      if (n < 0) new FloatStitchN(length, prefix.update(index, elem).asInstanceOf[Stitch[Float]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new FloatStitchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Stitch[Float]]),
            suffix)
        else new FloatStitchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Stitch[Float]])
      }
    }
    else lifted.update(index, elem)
  }

  override def head: Float = prefix.head

  override def tail: Stitch[Float] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new FloatStitchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new FloatStitchN(length - 1, prefix.tail, tree, suffix)
  }

  override def body: Stitch[Float] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new FloatStitchN(length - 1, prefix, tree.body, tree.foot)
    }
    else new FloatStitchN(length - 1, prefix, tree, suffix.body)
  }

  override def foot: Float = suffix.foot

  override def drop(lower: Int): Stitch[Float] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new FloatStitchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new FloatStitchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }

  override def take(upper: Int): Stitch[Float] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new FloatStitchN(upper, prefix, split.body, split.foot.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new FloatStitchN(upper, prefix, tree, suffix.take(k))
    }
  }

  override def :+ [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (suffix.length == 6)
        new FloatStitchN(
          length + 1,
          prefix,
          tree :+ new FloatStitch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new FloatStitch3(suffix(4), suffix(5), elem.asInstanceOf[Float]))
      else new FloatStitchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Stitch[Float]])
    }
    else lifted :+ elem
  }

  override def +: [B >: Float](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (prefix.length == 6)
        new FloatStitchN(
          length + 1,
          new FloatStitch3(elem.asInstanceOf[Float], prefix(0), prefix(1)),
          new FloatStitch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new FloatStitchN(length + 1, (elem +: prefix).asInstanceOf[Stitch[Float]], tree, suffix)
    }
    else elem +: lifted
  }

  override def traverse(f: Float => Unit): Unit = {
    prefix.traverse(f)
    tree.flatTraverse(f)
    suffix.traverse(f)
  }

  private[this] def lifted: Stitch[Float] = new RefStitchN(length, prefix, tree, suffix)
}
