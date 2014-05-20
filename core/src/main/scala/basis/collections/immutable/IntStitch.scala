//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import scala.annotation._

private[collections] final class IntStitch1(_1: Int) extends Stitch[Int] {
  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def apply(index: Int): Int = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (index == 0) new IntStitch1(elem.asInstanceOf[Int])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Int = _1

  override def tail: Stitch[Int] = Stitch.empty

  override def body: Stitch[Int] = Stitch.empty

  override def foot: Int = _1

  override def drop(lower: Int): Stitch[Int] = if (lower <= 0) this else Stitch.empty

  override def take(upper: Int): Stitch[Int] = if (upper <= 0) Stitch.empty else this

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch2(_1, elem.asInstanceOf[Int])
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch2(elem.asInstanceOf[Int], _1)
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = f(_1)

  private[this] def lifted: Stitch[Int] = new RefStitch1(_1)
}

private[collections] final class IntStitch2(_1: Int, _2: Int) extends Stitch[Int] {
  override def isEmpty: Boolean = false

  override def length: Int = 2

  override def apply(index: Int): Int = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (index == 0) new IntStitch2(elem.asInstanceOf[Int], _2)
      else if (index == 1) new IntStitch2(_1, elem.asInstanceOf[Int])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Int = _1

  override def tail: Stitch[Int] = new IntStitch1(_2)

  override def body: Stitch[Int] = new IntStitch1(_1)

  override def foot: Int = _2

  override def drop(lower: Int): Stitch[Int] = {
    if (lower <= 0) this
    else if (lower == 1) new IntStitch1(_2)
    else Stitch.empty
  }

  override def take(upper: Int): Stitch[Int] = {
    if (upper <= 0) Stitch.empty
    else if (upper == 1) new IntStitch1(_1)
    else this
  }

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch3(_1, _2, elem.asInstanceOf[Int])
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch3(elem.asInstanceOf[Int], _1, _2)
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = {
    f(_1); f(_2)
  }

  private[this] def lifted: Stitch[Int] = new RefStitch2(_1, _2)
}

private[collections] final class IntStitch3(_1: Int, _2: Int, _3: Int) extends Stitch[Int] {
  override def isEmpty: Boolean = false

  override def length: Int = 3

  override def apply(index: Int): Int = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntStitch3(elem.asInstanceOf[Int], _2, _3)
      case 1 => new IntStitch3(_1, elem.asInstanceOf[Int], _3)
      case 2 => new IntStitch3(_1, _2, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Int = _1

  override def tail: Stitch[Int] = new IntStitch2(_2, _3)

  override def body: Stitch[Int] = new IntStitch2(_1, _2)

  override def foot: Int = _3

  @tailrec override def drop(lower: Int): Stitch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntStitch2(_2, _3)
    case 2 => new IntStitch1(_3)
    case 3 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }

  @tailrec override def take(upper: Int): Stitch[Int] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new IntStitch1(_1)
    case 2 => new IntStitch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch4(_1, _2, _3, elem.asInstanceOf[Int])
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch4(elem.asInstanceOf[Int], _1, _2, _3)
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = {
    f(_1); f(_2); f(_3)
  }

  private[this] def lifted: Stitch[Int] = new RefStitch3(_1, _2, _3)
}

private[collections] final class IntStitch4
    (_1: Int, _2: Int, _3: Int, _4: Int)
  extends Stitch[Int] {

  override def isEmpty: Boolean = false

  override def length: Int = 4

  override def apply(index: Int): Int = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntStitch4(elem.asInstanceOf[Int], _2, _3, _4)
      case 1 => new IntStitch4(_1, elem.asInstanceOf[Int], _3, _4)
      case 2 => new IntStitch4(_1, _2, elem.asInstanceOf[Int], _4)
      case 3 => new IntStitch4(_1, _2, _3, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Int = _1

  override def tail: Stitch[Int] = new IntStitch3(_2, _3, _4)

  override def body: Stitch[Int] = new IntStitch3(_1, _2, _3)

  override def foot: Int = _4

  @tailrec override def drop(lower: Int): Stitch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntStitch3(_2, _3, _4)
    case 2 => new IntStitch2(_3, _4)
    case 3 => new IntStitch1(_4)
    case 4 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }

  @tailrec override def take(upper: Int): Stitch[Int] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new IntStitch1(_1)
    case 2 => new IntStitch2(_1, _2)
    case 3 => new IntStitch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch5(_1, _2, _3, _4, elem.asInstanceOf[Int])
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch5(elem.asInstanceOf[Int], _1, _2, _3, _4)
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4)
  }

  private[this] def lifted: Stitch[Int] = new RefStitch4(_1, _2, _3, _4)
}

private[collections] final class IntStitch5
    (_1: Int, _2: Int, _3: Int, _4: Int, _5: Int)
  extends Stitch[Int] {

  override def isEmpty: Boolean = false

  override def length: Int = 5

  override def apply(index: Int): Int = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntStitch5(elem.asInstanceOf[Int], _2, _3, _4, _5)
      case 1 => new IntStitch5(_1, elem.asInstanceOf[Int], _3, _4, _5)
      case 2 => new IntStitch5(_1, _2, elem.asInstanceOf[Int], _4, _5)
      case 3 => new IntStitch5(_1, _2, _3, elem.asInstanceOf[Int], _5)
      case 4 => new IntStitch5(_1, _2, _3, _4, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Int = _1

  override def tail: Stitch[Int] = new IntStitch4(_2, _3, _4, _5)

  override def body: Stitch[Int] = new IntStitch4(_1, _2, _3, _4)

  override def foot: Int = _5

  @tailrec override def drop(lower: Int): Stitch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntStitch4(_2, _3, _4, _5)
    case 2 => new IntStitch3(_3, _4, _5)
    case 3 => new IntStitch2(_4, _5)
    case 4 => new IntStitch1(_5)
    case 5 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }

  @tailrec override def take(upper: Int): Stitch[Int] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new IntStitch1(_1)
    case 2 => new IntStitch2(_1, _2)
    case 3 => new IntStitch3(_1, _2, _3)
    case 4 => new IntStitch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Int])
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch6(elem.asInstanceOf[Int], _1, _2, _3, _4, _5)
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }

  private[this] def lifted: Stitch[Int] = new RefStitch5(_1, _2, _3, _4, _5)
}

private[collections] final class IntStitch6
    (_1: Int, _2: Int, _3: Int, _4: Int, _5: Int, _6: Int)
  extends Stitch[Int] {

  override def isEmpty: Boolean = false

  override def length: Int = 6

  override def apply(index: Int): Int = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntStitch6(elem.asInstanceOf[Int], _2, _3, _4, _5, _6)
      case 1 => new IntStitch6(_1, elem.asInstanceOf[Int], _3, _4, _5, _6)
      case 2 => new IntStitch6(_1, _2, elem.asInstanceOf[Int], _4, _5, _6)
      case 3 => new IntStitch6(_1, _2, _3, elem.asInstanceOf[Int], _5, _6)
      case 4 => new IntStitch6(_1, _2, _3, _4, elem.asInstanceOf[Int], _6)
      case 5 => new IntStitch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lifted.update(index, elem)
  }

  override def head: Int = _1

  override def tail: Stitch[Int] = new IntStitch5(_2, _3, _4, _5, _6)

  override def body: Stitch[Int] = new IntStitch5(_1, _2, _3, _4, _5)

  override def foot: Int = _6

  @tailrec override def drop(lower: Int): Stitch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntStitch5(_2, _3, _4, _5, _6)
    case 2 => new IntStitch4(_3, _4, _5, _6)
    case 3 => new IntStitch3(_4, _5, _6)
    case 4 => new IntStitch2(_5, _6)
    case 5 => new IntStitch1(_6)
    case 6 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }

  @tailrec override def take(upper: Int): Stitch[Int] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new IntStitch1(_1)
    case 2 => new IntStitch2(_1, _2)
    case 3 => new IntStitch3(_1, _2, _3)
    case 4 => new IntStitch4(_1, _2, _3, _4)
    case 5 => new IntStitch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int])
      new IntStitchN(7, new IntStitch4(_1, _2, _3, _4), Stitch.empty, new IntStitch3(_5, _6, elem.asInstanceOf[Int]))
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int])
      new IntStitchN(7, new IntStitch3(elem.asInstanceOf[Int], _1, _2), Stitch.empty, new IntStitch4(_3, _4, _5, _6))
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }

  private[this] def lifted: Stitch[Int] = new RefStitch6(_1, _2, _3, _4, _5, _6)
}

private[collections] final class IntStitchN
    (override val length: Int, prefix: Stitch[Int], tree: Stitch[Stitch[Int]], suffix: Stitch[Int])
  extends Stitch[Int] {

  override def isEmpty: Boolean = false

  override def apply(index: Int): Int = {
    val n = index - prefix.length
    if (n < 0) prefix(index)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) tree(n >> 2)(n & 3)
      else suffix(k)
    }
  }

  override def update[B >: Int](index: Int, elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) {
      val n = index - prefix.length
      if (n < 0) new IntStitchN(length, prefix.update(index, elem).asInstanceOf[Stitch[Int]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new IntStitchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Stitch[Int]]),
            suffix)
        else new IntStitchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Stitch[Int]])
      }
    }
    else lifted.update(index, elem)
  }

  override def head: Int = prefix.head

  override def tail: Stitch[Int] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new IntStitchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new IntStitchN(length - 1, prefix.tail, tree, suffix)
  }

  override def body: Stitch[Int] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new IntStitchN(length - 1, prefix, tree.body, tree.foot)
    }
    else new IntStitchN(length - 1, prefix, tree, suffix.body)
  }

  override def foot: Int = suffix.foot

  override def drop(lower: Int): Stitch[Int] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new IntStitchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new IntStitchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }

  override def take(upper: Int): Stitch[Int] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new IntStitchN(upper, prefix, split.body, split.foot.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new IntStitchN(upper, prefix, tree, suffix.take(k))
    }
  }

  override def :+ [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (suffix.length == 6)
        new IntStitchN(
          length + 1,
          prefix,
          tree :+ new IntStitch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new IntStitch3(suffix(4), suffix(5), elem.asInstanceOf[Int]))
      else new IntStitchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Stitch[Int]])
    }
    else lifted :+ elem
  }

  override def +: [B >: Int](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (prefix.length == 6)
        new IntStitchN(
          length + 1,
          new IntStitch3(elem.asInstanceOf[Int], prefix(0), prefix(1)),
          new IntStitch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new IntStitchN(length + 1, (elem +: prefix).asInstanceOf[Stitch[Int]], tree, suffix)
    }
    else elem +: lifted
  }

  override def traverse(f: Int => Unit): Unit = {
    prefix.traverse(f)
    tree.flatTraverse(f)
    suffix.traverse(f)
  }

  private[this] def lifted: Stitch[Int] = new RefStitchN(length, prefix, tree, suffix)
}
