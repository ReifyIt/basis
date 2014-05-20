//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import scala.annotation._
import scala.Predef.<:<

private[collections] final class RefStitch1[+A](_1: A) extends Stitch[A] {
  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def apply(index: Int): A = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = {
    if (index == 0) new RefStitch1(elem)
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def head: A = _1

  override def tail: Stitch[A] = Stitch.empty

  override def body: Stitch[A] = Stitch.empty

  override def foot: A = _1

  override def drop(lower: Int): Stitch[A] = if (lower <= 0) this else Stitch.empty

  override def take(upper: Int): Stitch[A] = if (upper <= 0) Stitch.empty else this

  override def :+ [B >: A](elem: B): Stitch[B] = new RefStitch2(_1, elem)

  override def +: [B >: A](elem: B): Stitch[B] = new RefStitch2(elem, _1)

  override def traverse(f: A => Unit): Unit = {
    f(_1)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    _1.traverse(f)
  }
}

private[collections] final class RefStitch2[+A](_1: A, _2: A) extends Stitch[A] {
  override def isEmpty: Boolean = false

  override def length: Int = 2

  override def apply(index: Int): A = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = {
    if (index == 0) new RefStitch2(elem, _2)
    else if (index == 1) new RefStitch2(_1, elem)
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def head: A = _1

  override def tail: Stitch[A] = new RefStitch1(_2)

  override def body: Stitch[A] = new RefStitch1(_1)

  override def foot: A = _2

  override def drop(lower: Int): Stitch[A] = {
    if (lower <= 0) this
    else if (lower == 1) new RefStitch1(_2)
    else Stitch.empty
  }

  override def take(upper: Int): Stitch[A] = {
    if (upper <= 0) Stitch.empty
    else if (upper == 1) new RefStitch1(_1)
    else this
  }

  override def :+ [B >: A](elem: B): Stitch[B] = new RefStitch3(_1, _2, elem)

  override def +: [B >: A](elem: B): Stitch[B] = new RefStitch3(elem, _1, _2)

  override def traverse(f: A => Unit): Unit = {
    f(_1); f(_2)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    _1.traverse(f); _2.traverse(f)
  }
}

private[collections] final class RefStitch3[+A](_1: A, _2: A, _3: A) extends Stitch[A] {
  override def isEmpty: Boolean = false

  override def length: Int = 3

  override def apply(index: Int): A = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = (index: @switch) match {
    case 0 => new RefStitch3(elem, _2, _3)
    case 1 => new RefStitch3(_1, elem, _3)
    case 2 => new RefStitch3(_1, _2, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def head: A = _1

  override def tail: Stitch[A] = new RefStitch2(_2, _3)

  override def body: Stitch[A] = new RefStitch2(_1, _2)

  override def foot: A = _3

  @tailrec override def drop(lower: Int): Stitch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefStitch2(_2, _3)
    case 2 => new RefStitch1(_3)
    case 3 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }

  @tailrec override def take(upper: Int): Stitch[A] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new RefStitch1(_1)
    case 2 => new RefStitch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }

  override def :+ [B >: A](elem: B): Stitch[B] = new RefStitch4(_1, _2, _3, elem)

  override def +: [B >: A](elem: B): Stitch[B] = new RefStitch4(elem, _1, _2, _3)

  override def traverse(f: A => Unit): Unit = {
    f(_1); f(_2); f(_3)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    _1.traverse(f); _2.traverse(f); _3.traverse(f)
  }
}

private[collections] final class RefStitch4[+A]
    (_1: A, _2: A, _3: A, _4: A)
  extends Stitch[A] {

  override def isEmpty: Boolean = false

  override def length: Int = 4

  override def apply(index: Int): A = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = (index: @switch) match {
    case 0 => new RefStitch4(elem, _2, _3, _4)
    case 1 => new RefStitch4(_1, elem, _3, _4)
    case 2 => new RefStitch4(_1, _2, elem, _4)
    case 3 => new RefStitch4(_1, _2, _3, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def head: A = _1

  override def tail: Stitch[A] = new RefStitch3(_2, _3, _4)

  override def body: Stitch[A] = new RefStitch3(_1, _2, _3)

  override def foot: A = _4

  @tailrec override def drop(lower: Int): Stitch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefStitch3(_2, _3, _4)
    case 2 => new RefStitch2(_3, _4)
    case 3 => new RefStitch1(_4)
    case 4 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }

  @tailrec override def take(upper: Int): Stitch[A] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new RefStitch1(_1)
    case 2 => new RefStitch2(_1, _2)
    case 3 => new RefStitch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }

  override def :+ [B >: A](elem: B): Stitch[B] = new RefStitch5(_1, _2, _3, _4, elem)

  override def +: [B >: A](elem: B): Stitch[B] = new RefStitch5(elem, _1, _2, _3, _4)

  override def traverse(f: A => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    _1.traverse(f); _2.traverse(f); _3.traverse(f); _4.traverse(f)
  }
}

private[collections] final class RefStitch5[+A]
    (_1: A, _2: A, _3: A, _4: A, _5: A)
  extends Stitch[A] {

  override def isEmpty: Boolean = false

  override def length: Int = 5

  override def apply(index: Int): A = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = (index: @switch) match {
    case 0 => new RefStitch5(elem, _2, _3, _4, _5)
    case 1 => new RefStitch5(_1, elem, _3, _4, _5)
    case 2 => new RefStitch5(_1, _2, elem, _4, _5)
    case 3 => new RefStitch5(_1, _2, _3, elem, _5)
    case 4 => new RefStitch5(_1, _2, _3, _4, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def head: A = _1

  override def tail: Stitch[A] = new RefStitch4(_2, _3, _4, _5)

  override def body: Stitch[A] = new RefStitch4(_1, _2, _3, _4)

  override def foot: A = _5

  @tailrec override def drop(lower: Int): Stitch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefStitch4(_2, _3, _4, _5)
    case 2 => new RefStitch3(_3, _4, _5)
    case 3 => new RefStitch2(_4, _5)
    case 4 => new RefStitch1(_5)
    case 5 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }

  @tailrec override def take(upper: Int): Stitch[A] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new RefStitch1(_1)
    case 2 => new RefStitch2(_1, _2)
    case 3 => new RefStitch3(_1, _2, _3)
    case 4 => new RefStitch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }

  override def :+ [B >: A](elem: B): Stitch[B] =
    new RefStitch6(_1, _2, _3, _4, _5, elem)

  override def +: [B >: A](elem: B): Stitch[B] =
    new RefStitch6(elem, _1, _2, _3, _4, _5)

  override def traverse(f: A => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    _1.traverse(f); _2.traverse(f); _3.traverse(f); _4.traverse(f); _5.traverse(f)
  }
}

private[collections] final class RefStitch6[+A]
    (_1: A, _2: A, _3: A, _4: A, _5: A, _6: A)
  extends Stitch[A] {

  override def isEmpty: Boolean = false

  override def length: Int = 6

  override def apply(index: Int): A = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = (index: @switch) match {
    case 0 => new RefStitch6(elem, _2, _3, _4, _5, _6)
    case 1 => new RefStitch6(_1, elem, _3, _4, _5, _6)
    case 2 => new RefStitch6(_1, _2, elem, _4, _5, _6)
    case 3 => new RefStitch6(_1, _2, _3, elem, _5, _6)
    case 4 => new RefStitch6(_1, _2, _3, _4, elem, _6)
    case 5 => new RefStitch6(_1, _2, _3, _4, _5, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  override def head: A = _1

  override def tail: Stitch[A] = new RefStitch5(_2, _3, _4, _5, _6)

  override def body: Stitch[A] = new RefStitch5(_1, _2, _3, _4, _5)

  override def foot: A = _6

  @tailrec override def drop(lower: Int): Stitch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefStitch5(_2, _3, _4, _5, _6)
    case 2 => new RefStitch4(_3, _4, _5, _6)
    case 3 => new RefStitch3(_4, _5, _6)
    case 4 => new RefStitch2(_5, _6)
    case 5 => new RefStitch1(_6)
    case 6 => Stitch.empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }

  @tailrec override def take(upper: Int): Stitch[A] = (upper: @switch) match {
    case 0 => Stitch.empty
    case 1 => new RefStitch1(_1)
    case 2 => new RefStitch2(_1, _2)
    case 3 => new RefStitch3(_1, _2, _3)
    case 4 => new RefStitch4(_1, _2, _3, _4)
    case 5 => new RefStitch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }

  override def :+ [B >: A](elem: B): Stitch[B] =
    new RefStitchN(7, new RefStitch4(_1, _2, _3, _4), Stitch.empty, new RefStitch3(_5, _6, elem))

  override def +: [B >: A](elem: B): Stitch[B] =
    new RefStitchN(7, new RefStitch3(elem, _1, _2), Stitch.empty, new RefStitch4(_3, _4, _5, _6))

  override def traverse(f: A => Unit): Unit = {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    _1.traverse(f); _2.traverse(f); _3.traverse(f); _4.traverse(f); _5.traverse(f); _6.traverse(f)
  }
}

private[collections] final class RefStitchN[+A]
    (override val length: Int, prefix: Stitch[A], tree: Stitch[Stitch[A]], suffix: Stitch[A])
  extends Stitch[A] {

  override def isEmpty: Boolean = false

  override def apply(index: Int): A = {
    val n = index - prefix.length
    if (n < 0) prefix(index)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) tree(n >> 2)(n & 3)
      else suffix(k)
    }
  }

  override def update[B >: A](index: Int, elem: B): Stitch[B] = {
    val n = index - prefix.length
    if (n < 0) new RefStitchN(length, prefix.update(index, elem), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0)
        new RefStitchN(
          length,
          prefix,
          tree.update(n >> 2, tree(n >> 2).update(n & 3, elem)),
          suffix)
      else new RefStitchN(length, prefix, tree, suffix.update(index, elem))
    }
  }

  override def head: A = prefix.head

  override def tail: Stitch[A] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new RefStitchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new RefStitchN(length - 1, prefix.tail, tree, suffix)
  }

  override def body: Stitch[A] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new RefStitchN(length - 1, prefix, tree.body, tree.foot)
    }
    else new RefStitchN(length - 1, prefix, tree, suffix.body)
  }

  override def foot: A = suffix.foot

  override def drop(lower: Int): Stitch[A] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new RefStitchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new RefStitchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }

  override def take(upper: Int): Stitch[A] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new RefStitchN(upper, prefix, split.body, split.foot.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new RefStitchN(upper, prefix, tree, suffix.take(k))
    }
  }

  override def :+ [B >: A](elem: B): Stitch[B] = {
    if (suffix.length == 6)
      new RefStitchN(
        length + 1,
        prefix,
        tree :+ new RefStitch4(suffix(0), suffix(1), suffix(2), suffix(3)),
        new RefStitch3(suffix(4), suffix(5), elem))
    else new RefStitchN(length + 1, prefix, tree, suffix :+ elem)
  }

  override def +: [B >: A](elem: B): Stitch[B] = {
    if (prefix.length == 6)
      new RefStitchN(
        length + 1,
        new RefStitch3(elem, prefix(0), prefix(1)),
        new RefStitch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
        suffix)
    else new RefStitchN(length + 1, elem +: prefix, tree, suffix)
  }

  override def traverse(f: A => Unit): Unit = {
    prefix.traverse(f)
    tree.flatTraverse(f)
    suffix.traverse(f)
  }

  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    prefix.flatTraverse(f)
    var i = 0
    val n = tree.length
    while (i < n) {
      tree(i).flatTraverse(f)
      i += 1
    }
    suffix.flatTraverse(f)
  }
}
