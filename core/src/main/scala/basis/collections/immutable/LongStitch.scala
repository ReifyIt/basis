//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import scala.annotation._

private[collections] final class LongStitch1(_1: Long) extends Stitch[Long] {
  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def apply(index: Int): Long = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) {
      if (index == 0) new LongStitch1(elem.asInstanceOf[Long])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Long = _1

  override def tail: Stitch[Long] = Stitch.empty

  override def body: Stitch[Long] = Stitch.empty

  override def foot: Long = _1

  override def drop(lower: Int): Stitch[Long] = if (lower <= 0) this else Stitch.empty

  override def take(upper: Int): Stitch[Long] = if (upper <= 0) Stitch.empty else this

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch2(_1, elem.asInstanceOf[Long])
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch2(elem.asInstanceOf[Long], _1)
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = f(_1)

  private[this] def lifted: Stitch[Long] = new RefStitch1(_1)
}

private[collections] final class LongStitch2(_1: Long, _2: Long) extends Stitch[Long] {
  override def isEmpty: Boolean = false

  override def length: Int = 2

  override def apply(index: Int): Long = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) {
      if (index == 0) new LongStitch2(elem.asInstanceOf[Long], _2)
      else if (index == 1) new LongStitch2(_1, elem.asInstanceOf[Long])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Long = _1

  override def tail: Stitch[Long] = new LongStitch1(_2)

  override def body: Stitch[Long] = new LongStitch1(_1)

  override def foot: Long = _2

  override def drop(lower: Int): Stitch[Long] = {
    if (lower <= 0) this
    else if (lower == 1) new LongStitch1(_2)
    else Stitch.empty
  }

  override def take(upper: Int): Stitch[Long] = {
    if (upper <= 0) Stitch.empty
    else if (upper == 1) new LongStitch1(_1)
    else this
  }

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch3(_1, _2, elem.asInstanceOf[Long])
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch3(elem.asInstanceOf[Long], _1, _2)
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = {
    f(_1); f(_2)
  }

  private[this] def lifted: Stitch[Long] = new RefStitch2(_1, _2)
}

private[collections] final class LongStitch3(_1: Long, _2: Long, _3: Long) extends Stitch[Long] {
  override def isEmpty: Boolean = false

  override def length: Int = 3

  override def apply(index: Int): Long = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) (index: @switch) match {
      case 0 => new LongStitch3(elem.asInstanceOf[Long], _2, _3)
      case 1 => new LongStitch3(_1, elem.asInstanceOf[Long], _3)
      case 2 => new LongStitch3(_1, _2, elem.asInstanceOf[Long])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Long = _1

  override def tail: Stitch[Long] = new LongStitch2(_2, _3)

  override def body: Stitch[Long] = new LongStitch2(_1, _2)

  override def foot: Long = _3

  @tailrec override def drop(lower: Int): Stitch[Long] = (lower: @switch) match {
    case 0 => this
    case 1 => new LongStitch2(_2, _3)
    case 2 => new LongStitch1(_3)
    case 3 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }

  @tailrec override def take(upper: Int): Stitch[Long] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new LongStitch1(_1)
    case 2 => new LongStitch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch4(_1, _2, _3, elem.asInstanceOf[Long])
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch4(elem.asInstanceOf[Long], _1, _2, _3)
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = {
    f(_1); f(_2); f(_3)
  }

  private[this] def lifted: Stitch[Long] = new RefStitch3(_1, _2, _3)
}

private[collections] final class LongStitch4
    (_1: Long, _2: Long, _3: Long, _4: Long)
  extends Stitch[Long] {

  override def isEmpty: Boolean = false

  override def length: Int = 4

  override def apply(index: Int): Long = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) (index: @switch) match {
      case 0 => new LongStitch4(elem.asInstanceOf[Long], _2, _3, _4)
      case 1 => new LongStitch4(_1, elem.asInstanceOf[Long], _3, _4)
      case 2 => new LongStitch4(_1, _2, elem.asInstanceOf[Long], _4)
      case 3 => new LongStitch4(_1, _2, _3, elem.asInstanceOf[Long])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Long = _1

  override def tail: Stitch[Long] = new LongStitch3(_2, _3, _4)

  override def body: Stitch[Long] = new LongStitch3(_1, _2, _3)

  override def foot: Long = _4

  @tailrec override def drop(lower: Int): Stitch[Long] = (lower: @switch) match {
    case 0 => this
    case 1 => new LongStitch3(_2, _3, _4)
    case 2 => new LongStitch2(_3, _4)
    case 3 => new LongStitch1(_4)
    case 4 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }

  @tailrec override def take(upper: Int): Stitch[Long] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new LongStitch1(_1)
    case 2 => new LongStitch2(_1, _2)
    case 3 => new LongStitch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch5(_1, _2, _3, _4, elem.asInstanceOf[Long])
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch5(elem.asInstanceOf[Long], _1, _2, _3, _4)
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4)
  }

  private[this] def lifted: Stitch[Long] = new RefStitch4(_1, _2, _3, _4)
}

private[collections] final class LongStitch5
    (_1: Long, _2: Long, _3: Long, _4: Long, _5: Long)
  extends Stitch[Long] {

  override def isEmpty: Boolean = false

  override def length: Int = 5

  override def apply(index: Int): Long = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) (index: @switch) match {
      case 0 => new LongStitch5(elem.asInstanceOf[Long], _2, _3, _4, _5)
      case 1 => new LongStitch5(_1, elem.asInstanceOf[Long], _3, _4, _5)
      case 2 => new LongStitch5(_1, _2, elem.asInstanceOf[Long], _4, _5)
      case 3 => new LongStitch5(_1, _2, _3, elem.asInstanceOf[Long], _5)
      case 4 => new LongStitch5(_1, _2, _3, _4, elem.asInstanceOf[Long])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Long = _1

  override def tail: Stitch[Long] = new LongStitch4(_2, _3, _4, _5)

  override def body: Stitch[Long] = new LongStitch4(_1, _2, _3, _4)

  override def foot: Long = _5

  @tailrec override def drop(lower: Int): Stitch[Long] = (lower: @switch) match {
    case 0 => this
    case 1 => new LongStitch4(_2, _3, _4, _5)
    case 2 => new LongStitch3(_3, _4, _5)
    case 3 => new LongStitch2(_4, _5)
    case 4 => new LongStitch1(_5)
    case 5 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }

  @tailrec override def take(upper: Int): Stitch[Long] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new LongStitch1(_1)
    case 2 => new LongStitch2(_1, _2)
    case 3 => new LongStitch3(_1, _2, _3)
    case 4 => new LongStitch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Long])
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) new LongStitch6(elem.asInstanceOf[Long], _1, _2, _3, _4, _5)
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }

  private[this] def lifted: Stitch[Long] = new RefStitch5(_1, _2, _3, _4, _5)
}

private[collections] final class LongStitch6
    (_1: Long, _2: Long, _3: Long, _4: Long, _5: Long, _6: Long)
  extends Stitch[Long] {

  override def isEmpty: Boolean = false

  override def length: Int = 6

  override def apply(index: Int): Long = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) (index: @switch) match {
      case 0 => new LongStitch6(elem.asInstanceOf[Long], _2, _3, _4, _5, _6)
      case 1 => new LongStitch6(_1, elem.asInstanceOf[Long], _3, _4, _5, _6)
      case 2 => new LongStitch6(_1, _2, elem.asInstanceOf[Long], _4, _5, _6)
      case 3 => new LongStitch6(_1, _2, _3, elem.asInstanceOf[Long], _5, _6)
      case 4 => new LongStitch6(_1, _2, _3, _4, elem.asInstanceOf[Long], _6)
      case 5 => new LongStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Long])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Long = _1

  override def tail: Stitch[Long] = new LongStitch5(_2, _3, _4, _5, _6)

  override def body: Stitch[Long] = new LongStitch5(_1, _2, _3, _4, _5)

  override def foot: Long = _6

  @tailrec override def drop(lower: Int): Stitch[Long] = (lower: @switch) match {
    case 0 => this
    case 1 => new LongStitch5(_2, _3, _4, _5, _6)
    case 2 => new LongStitch4(_3, _4, _5, _6)
    case 3 => new LongStitch3(_4, _5, _6)
    case 4 => new LongStitch2(_5, _6)
    case 5 => new LongStitch1(_6)
    case 6 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }

  @tailrec override def take(upper: Int): Stitch[Long] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new LongStitch1(_1)
    case 2 => new LongStitch2(_1, _2)
    case 3 => new LongStitch3(_1, _2, _3)
    case 4 => new LongStitch4(_1, _2, _3, _4)
    case 5 => new LongStitch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long])
      new LongStitchN(7, new LongStitch4(_1, _2, _3, _4), Stitch.empty, new LongStitch3(_5, _6, elem.asInstanceOf[Long]))
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long])
      new LongStitchN(7, new LongStitch3(elem.asInstanceOf[Long], _1, _2), Stitch.empty, new LongStitch4(_3, _4, _5, _6))
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }

  private[this] def lifted: Stitch[Long] = new RefStitch6(_1, _2, _3, _4, _5, _6)
}

private[collections] final class LongStitchN
    (override val length: Int, prefix: Stitch[Long], tree: Stitch[Stitch[Long]], suffix: Stitch[Long])
  extends Stitch[Long] {

  override def isEmpty: Boolean = false

  override def apply(index: Int): Long = {
    val n = index - prefix.length
    if (n < 0) prefix(index)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) tree(n >> 2)(n & 3)
      else suffix(k)
    }
  }

  override def update[B >: Long](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) {
      val n = index - prefix.length
      if (n < 0) new LongStitchN(length, prefix.update(index, elem).asInstanceOf[Stitch[Long]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new LongStitchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Stitch[Long]]),
            suffix)
        else new LongStitchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Stitch[Long]])
      }
    }
    else lifted.update(index, elem)
  }

  override def head: Long = prefix.head

  override def tail: Stitch[Long] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new LongStitchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new LongStitchN(length - 1, prefix.tail, tree, suffix)
  }

  override def body: Stitch[Long] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new LongStitchN(length - 1, prefix, tree.body, tree.foot)
    }
    else new LongStitchN(length - 1, prefix, tree, suffix.body)
  }

  override def foot: Long = suffix.foot

  override def drop(lower: Int): Stitch[Long] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new LongStitchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new LongStitchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }

  override def take(upper: Int): Stitch[Long] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new LongStitchN(upper, prefix, split.body, split.foot.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new LongStitchN(upper, prefix, tree, suffix.take(k))
    }
  }

  override def :+ [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) {
      if (suffix.length == 6)
        new LongStitchN(
          length + 1,
          prefix,
          tree :+ new LongStitch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new LongStitch3(suffix(4), suffix(5), elem.asInstanceOf[Long]))
      else new LongStitchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Stitch[Long]])
    }
    else lifted :+ elem
  }

  override def +: [B >: Long](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Long]) {
      if (prefix.length == 6)
        new LongStitchN(
          length + 1,
          new LongStitch3(elem.asInstanceOf[Long], prefix(0), prefix(1)),
          new LongStitch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new LongStitchN(length + 1, (elem +: prefix).asInstanceOf[Stitch[Long]], tree, suffix)
    }
    else elem +: lifted
  }

  override def traverse(f: Long => Unit): Unit = {
    prefix.traverse(f)
    tree.flatTraverse(f)
    suffix.traverse(f)
  }

  private[this] def lifted: Stitch[Long] = new RefStitchN(length, prefix, tree, suffix)
}
