/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

import scala.Predef.<:<
import scala.annotation.{switch, tailrec}

private[containers] final class RefBatch1[+A](_1: A) extends Batch[A] {
  override def isEmpty: Boolean = false
  
  override def length: Int = 1
  
  override def apply(index: Int): A = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = {
    if (index == 0) new RefBatch1(elem)
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def head: A = _1
  
  override def last: A = _1
  
  override def init: Batch[A] = Batch.Empty
  
  override def tail: Batch[A] = Batch.Empty
  
  override def drop(lower: Int): Batch[A] = if (lower <= 0) this else Batch.Empty
  
  override def take(upper: Int): Batch[A] = if (upper <= 0) Batch.Empty else this
  
  override def append[B >: A](elem: B): Batch[B] = new RefBatch2(_1, elem)
  
  override def prepend[B >: A](elem: B): Batch[B] = new RefBatch2(elem, _1)
  
  override def traverse(f: A => Unit) {
    f(_1)
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    _1 traverse f
  }
}

private[containers] final class RefBatch2[+A](_1: A, _2: A) extends Batch[A] {
  override def isEmpty: Boolean = false
  
  override def length: Int = 2
  
  override def apply(index: Int): A = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = {
    if (index == 0) new RefBatch2(elem, _2)
    else if (index == 1) new RefBatch2(_1, elem)
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def head: A = _1
  
  override def last: A = _2
  
  override def init: Batch[A] = new RefBatch1(_1)
  
  override def tail: Batch[A] = new RefBatch1(_2)
  
  override def drop(lower: Int): Batch[A] = {
    if (lower <= 0) this
    else if (lower == 1) new RefBatch1(_2)
    else Batch.Empty
  }
  
  override def take(upper: Int): Batch[A] = {
    if (upper <= 0) Batch.Empty
    else if (upper == 1) new RefBatch1(_1)
    else this
  }
  
  override def append[B >: A](elem: B): Batch[B] = new RefBatch3(_1, _2, elem)
  
  override def prepend[B >: A](elem: B): Batch[B] = new RefBatch3(elem, _1, _2)
  
  override def traverse(f: A => Unit) {
    f(_1); f(_2)
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    _1 traverse f; _2 traverse f
  }
}

private[containers] final class RefBatch3[+A](_1: A, _2: A, _3: A) extends Batch[A] {
  override def isEmpty: Boolean = false
  
  override def length: Int = 3
  
  override def apply(index: Int): A = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = (index: @switch) match {
    case 0 => new RefBatch3(elem, _2, _3)
    case 1 => new RefBatch3(_1, elem, _3)
    case 2 => new RefBatch3(_1, _2, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def head: A = _1
  
  override def last: A = _3
  
  override def init: Batch[A] = new RefBatch2(_1, _2)
  
  override def tail: Batch[A] = new RefBatch2(_2, _3)
  
  @tailrec override def drop(lower: Int): Batch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefBatch2(_2, _3)
    case 2 => new RefBatch1(_3)
    case 3 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }
  
  @tailrec override def take(upper: Int): Batch[A] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new RefBatch1(_1)
    case 2 => new RefBatch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }
  
  override def append[B >: A](elem: B): Batch[B] = new RefBatch4(_1, _2, _3, elem)
  
  override def prepend[B >: A](elem: B): Batch[B] = new RefBatch4(elem, _1, _2, _3)
  
  override def traverse(f: A => Unit) {
    f(_1); f(_2); f(_3)
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    _1 traverse f; _2 traverse f; _3 traverse f
  }
}

private[containers] final class RefBatch4[+A]
    (_1: A, _2: A, _3: A, _4: A)
  extends Batch[A] {
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 4
  
  override def apply(index: Int): A = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = (index: @switch) match {
    case 0 => new RefBatch4(elem, _2, _3, _4)
    case 1 => new RefBatch4(_1, elem, _3, _4)
    case 2 => new RefBatch4(_1, _2, elem, _4)
    case 3 => new RefBatch4(_1, _2, _3, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def head: A = _1
  
  override def last: A = _4
  
  override def init: Batch[A] = new RefBatch3(_1, _2, _3)
  
  override def tail: Batch[A] = new RefBatch3(_2, _3, _4)
  
  @tailrec override def drop(lower: Int): Batch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefBatch3(_2, _3, _4)
    case 2 => new RefBatch2(_3, _4)
    case 3 => new RefBatch1(_4)
    case 4 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }
  
  @tailrec override def take(upper: Int): Batch[A] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new RefBatch1(_1)
    case 2 => new RefBatch2(_1, _2)
    case 3 => new RefBatch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }
  
  override def append[B >: A](elem: B): Batch[B] = new RefBatch5(_1, _2, _3, _4, elem)
    
  override def prepend[B >: A](elem: B): Batch[B] = new RefBatch5(elem, _1, _2, _3, _4)
  
  override def traverse(f: A => Unit) {
    f(_1); f(_2); f(_3); f(_4)
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    _1 traverse f; _2 traverse f; _3 traverse f; _4 traverse f
  }
}

private[containers] final class RefBatch5[+A]
    (_1: A, _2: A, _3: A, _4: A, _5: A)
  extends Batch[A] {
  
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
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = (index: @switch) match {
    case 0 => new RefBatch5(elem, _2, _3, _4, _5)
    case 1 => new RefBatch5(_1, elem, _3, _4, _5)
    case 2 => new RefBatch5(_1, _2, elem, _4, _5)
    case 3 => new RefBatch5(_1, _2, _3, elem, _5)
    case 4 => new RefBatch5(_1, _2, _3, _4, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def head: A = _1
  
  override def last: A = _5
  
  override def init: Batch[A] = new RefBatch4(_1, _2, _3, _4)
  
  override def tail: Batch[A] = new RefBatch4(_2, _3, _4, _5)
  
  @tailrec override def drop(lower: Int): Batch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefBatch4(_2, _3, _4, _5)
    case 2 => new RefBatch3(_3, _4, _5)
    case 3 => new RefBatch2(_4, _5)
    case 4 => new RefBatch1(_5)
    case 5 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }
  
  @tailrec override def take(upper: Int): Batch[A] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new RefBatch1(_1)
    case 2 => new RefBatch2(_1, _2)
    case 3 => new RefBatch3(_1, _2, _3)
    case 4 => new RefBatch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }
  
  override def append[B >: A](elem: B): Batch[B] =
    new RefBatch6(_1, _2, _3, _4, _5, elem)
    
  override def prepend[B >: A](elem: B): Batch[B] =
    new RefBatch6(elem, _1, _2, _3, _4, _5)
  
  override def traverse(f: A => Unit) {
    f(_1); f(_2); f(_3); f(_4); f(_5)
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    _1 traverse f; _2 traverse f; _3 traverse f; _4 traverse f; _5 traverse f
  }
}

private[containers] final class RefBatch6[+A]
    (_1: A, _2: A, _3: A, _4: A, _5: A, _6: A)
  extends Batch[A] {
  
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
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = (index: @switch) match {
    case 0 => new RefBatch6(elem, _2, _3, _4, _5, _6)
    case 1 => new RefBatch6(_1, elem, _3, _4, _5, _6)
    case 2 => new RefBatch6(_1, _2, elem, _4, _5, _6)
    case 3 => new RefBatch6(_1, _2, _3, elem, _5, _6)
    case 4 => new RefBatch6(_1, _2, _3, _4, elem, _6)
    case 5 => new RefBatch6(_1, _2, _3, _4, _5, elem)
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def head: A = _1
  
  override def last: A = _6
  
  override def init: Batch[A] = new RefBatch5(_1, _2, _3, _4, _5)
  
  override def tail: Batch[A] = new RefBatch5(_2, _3, _4, _5, _6)
  
  @tailrec override def drop(lower: Int): Batch[A] = (lower: @switch) match {
    case 0 => this
    case 1 => new RefBatch5(_2, _3, _4, _5, _6)
    case 2 => new RefBatch4(_3, _4, _5, _6)
    case 3 => new RefBatch3(_4, _5, _6)
    case 4 => new RefBatch2(_5, _6)
    case 5 => new RefBatch1(_6)
    case 6 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }
  
  @tailrec override def take(upper: Int): Batch[A] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new RefBatch1(_1)
    case 2 => new RefBatch2(_1, _2)
    case 3 => new RefBatch3(_1, _2, _3)
    case 4 => new RefBatch4(_1, _2, _3, _4)
    case 5 => new RefBatch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }
  
  override def append[B >: A](elem: B): Batch[B] =
    new RefBatchN(7, new RefBatch4(_1, _2, _3, _4), Batch.Empty, new RefBatch3(_5, _6, elem))
    
  override def prepend[B >: A](elem: B): Batch[B] =
    new RefBatchN(7, new RefBatch3(elem, _1, _2), Batch.Empty, new RefBatch4(_3, _4, _5, _6))
  
  override def traverse(f: A => Unit) {
    f(_1); f(_2); f(_3); f(_4); f(_5); f(_6)
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    _1 traverse f; _2 traverse f; _3 traverse f; _4 traverse f; _5 traverse f; _6 traverse f
  }
}

private[containers] final class RefBatchN[+A]
    (override val length: Int, prefix: Batch[A], tree: Batch[Batch[A]], suffix: Batch[A])
  extends Batch[A] {
  
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
  
  override def update[B >: A](index: Int, elem: B): Batch[B] = {
    val n = index - prefix.length
    if (n < 0) new RefBatchN(length, prefix.update(index, elem), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0)
        new RefBatchN(
          length,
          prefix,
          tree.update(n >> 2, tree(n >> 2).update(n & 3, elem)),
          suffix)
      else new RefBatchN(length, prefix, tree, suffix.update(index, elem))
    }
  }
  
  override def head: A = prefix.head
  
  override def last: A = suffix.last
  
  override def init: Batch[A] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new RefBatchN(length - 1, prefix, tree.init, tree.last)
    }
    else new RefBatchN(length - 1, prefix, tree, suffix.init)
  }
  
  override def tail: Batch[A] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new RefBatchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new RefBatchN(length - 1, prefix.tail, tree, suffix)
  }
  
  override def drop(lower: Int): Batch[A] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new RefBatchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new RefBatchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }
  
  override def take(upper: Int): Batch[A] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new RefBatchN(upper, prefix, split.init, split.last.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new RefBatchN(upper, prefix, tree, suffix.take(k))
    }
  }
  
  override def append[B >: A](elem: B): Batch[B] = {
    if (suffix.length == 6)
      new RefBatchN(
        length + 1,
        prefix,
        tree :+ new RefBatch4(suffix(0), suffix(1), suffix(2), suffix(3)),
        new RefBatch3(suffix(4), suffix(5), elem))
    else new RefBatchN(length + 1, prefix, tree, suffix :+ elem)
  }
  
  override def prepend[B >: A](elem: B): Batch[B] = {
    if (prefix.length == 6)
      new RefBatchN(
        length + 1,
        new RefBatch3(elem, prefix(0), prefix(1)),
        new RefBatch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
        suffix)
    else new RefBatchN(length + 1, elem +: prefix, tree, suffix)
  }
  
  override def traverse(f: A => Unit) {
    prefix traverse f
    tree flatTraverse f
    suffix traverse f
  }
  
  override def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    prefix flatTraverse f
    var i = 0
    val n = tree.length
    while (i < n) {
      tree(i) flatTraverse f
      i += 1
    }
    suffix flatTraverse f
  }
}
