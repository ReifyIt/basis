/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

import scala.annotation.{switch, tailrec}

private[containers] final class IntBatch1(_1: Int) extends Batch[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 1
  
  override def apply(index: Int): Int = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (index == 0) new IntBatch1(elem.asInstanceOf[Int])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = _1
  
  override def last: Int = _1
  
  override def init: Batch[Int] = Batch.Empty
  
  override def tail: Batch[Int] = Batch.Empty
  
  override def drop(lower: Int): Batch[Int] = if (lower <= 0) this else Batch.Empty
  
  override def take(upper: Int): Batch[Int] = if (upper <= 0) Batch.Empty else this
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch2(_1, elem.asInstanceOf[Int])
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch2(elem.asInstanceOf[Int], _1)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatch1(_1)
}

private[containers] final class IntBatch2(_1: Int, _2: Int) extends Batch[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 2
  
  override def apply(index: Int): Int = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (index == 0) new IntBatch2(elem.asInstanceOf[Int], _2)
      else if (index == 1) new IntBatch2(_1, elem.asInstanceOf[Int])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = _1
  
  override def last: Int = _2
  
  override def init: Batch[Int] = new IntBatch1(_1)
  
  override def tail: Batch[Int] = new IntBatch1(_2)
  
  override def drop(lower: Int): Batch[Int] = {
    if (lower <= 0) this
    else if (lower == 1) new IntBatch1(_2)
    else Batch.Empty
  }
  
  override def take(upper: Int): Batch[Int] = {
    if (upper <= 0) Batch.Empty
    else if (upper == 1) new IntBatch1(_1)
    else this
  }
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch3(_1, _2, elem.asInstanceOf[Int])
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch3(elem.asInstanceOf[Int], _1, _2)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatch2(_1, _2)
}

private[containers] final class IntBatch3(_1: Int, _2: Int, _3: Int) extends Batch[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 3
  
  override def apply(index: Int): Int = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntBatch3(elem.asInstanceOf[Int], _2, _3)
      case 1 => new IntBatch3(_1, elem.asInstanceOf[Int], _3)
      case 2 => new IntBatch3(_1, _2, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = _1
  
  override def last: Int = _3
  
  override def init: Batch[Int] = new IntBatch2(_1, _2)
  
  override def tail: Batch[Int] = new IntBatch2(_2, _3)
  
  @tailrec override def drop(lower: Int): Batch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntBatch2(_2, _3)
    case 2 => new IntBatch1(_3)
    case 3 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }
  
  @tailrec override def take(upper: Int): Batch[Int] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new IntBatch1(_1)
    case 2 => new IntBatch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch4(_1, _2, _3, elem.asInstanceOf[Int])
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch4(elem.asInstanceOf[Int], _1, _2, _3)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatch3(_1, _2, _3)
}

private[containers] final class IntBatch4
    (_1: Int, _2: Int, _3: Int, _4: Int)
  extends Batch[Int] with Reified {
  
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 4
  
  override def apply(index: Int): Int = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntBatch4(elem.asInstanceOf[Int], _2, _3, _4)
      case 1 => new IntBatch4(_1, elem.asInstanceOf[Int], _3, _4)
      case 2 => new IntBatch4(_1, _2, elem.asInstanceOf[Int], _4)
      case 3 => new IntBatch4(_1, _2, _3, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = _1
  
  override def last: Int = _4
  
  override def init: Batch[Int] = new IntBatch3(_1, _2, _3)
  
  override def tail: Batch[Int] = new IntBatch3(_2, _3, _4)
  
  @tailrec override def drop(lower: Int): Batch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntBatch3(_2, _3, _4)
    case 2 => new IntBatch2(_3, _4)
    case 3 => new IntBatch1(_4)
    case 4 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }
  
  @tailrec override def take(upper: Int): Batch[Int] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new IntBatch1(_1)
    case 2 => new IntBatch2(_1, _2)
    case 3 => new IntBatch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch5(_1, _2, _3, _4, elem.asInstanceOf[Int])
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch5(elem.asInstanceOf[Int], _1, _2, _3, _4)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatch4(_1, _2, _3, _4)
}

private[containers] final class IntBatch5
    (_1: Int, _2: Int, _3: Int, _4: Int, _5: Int)
  extends Batch[Int] with Reified {
  
  protected override def T: TypeHint[Int] = TypeHint.Int
  
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
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntBatch5(elem.asInstanceOf[Int], _2, _3, _4, _5)
      case 1 => new IntBatch5(_1, elem.asInstanceOf[Int], _3, _4, _5)
      case 2 => new IntBatch5(_1, _2, elem.asInstanceOf[Int], _4, _5)
      case 3 => new IntBatch5(_1, _2, _3, elem.asInstanceOf[Int], _5)
      case 4 => new IntBatch5(_1, _2, _3, _4, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = _1
  
  override def last: Int = _5
  
  override def init: Batch[Int] = new IntBatch4(_1, _2, _3, _4)
  
  override def tail: Batch[Int] = new IntBatch4(_2, _3, _4, _5)
  
  @tailrec override def drop(lower: Int): Batch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntBatch4(_2, _3, _4, _5)
    case 2 => new IntBatch3(_3, _4, _5)
    case 3 => new IntBatch2(_4, _5)
    case 4 => new IntBatch1(_5)
    case 5 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }
  
  @tailrec override def take(upper: Int): Batch[Int] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new IntBatch1(_1)
    case 2 => new IntBatch2(_1, _2)
    case 3 => new IntBatch3(_1, _2, _3)
    case 4 => new IntBatch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Int])
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch6(elem.asInstanceOf[Int], _1, _2, _3, _4, _5)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatch5(_1, _2, _3, _4, _5)
}

private[containers] final class IntBatch6
    (_1: Int, _2: Int, _3: Int, _4: Int, _5: Int, _6: Int)
  extends Batch[Int] with Reified {
  
  protected override def T: TypeHint[Int] = TypeHint.Int
  
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
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) (index: @switch) match {
      case 0 => new IntBatch6(elem.asInstanceOf[Int], _2, _3, _4, _5, _6)
      case 1 => new IntBatch6(_1, elem.asInstanceOf[Int], _3, _4, _5, _6)
      case 2 => new IntBatch6(_1, _2, elem.asInstanceOf[Int], _4, _5, _6)
      case 3 => new IntBatch6(_1, _2, _3, elem.asInstanceOf[Int], _5, _6)
      case 4 => new IntBatch6(_1, _2, _3, _4, elem.asInstanceOf[Int], _6)
      case 5 => new IntBatch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Int])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = _1
  
  override def last: Int = _6
  
  override def init: Batch[Int] = new IntBatch5(_1, _2, _3, _4, _5)
  
  override def tail: Batch[Int] = new IntBatch5(_2, _3, _4, _5, _6)
  
  @tailrec override def drop(lower: Int): Batch[Int] = (lower: @switch) match {
    case 0 => this
    case 1 => new IntBatch5(_2, _3, _4, _5, _6)
    case 2 => new IntBatch4(_3, _4, _5, _6)
    case 3 => new IntBatch3(_4, _5, _6)
    case 4 => new IntBatch2(_5, _6)
    case 5 => new IntBatch1(_6)
    case 6 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }
  
  @tailrec override def take(upper: Int): Batch[Int] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new IntBatch1(_1)
    case 2 => new IntBatch2(_1, _2)
    case 3 => new IntBatch3(_1, _2, _3)
    case 4 => new IntBatch4(_1, _2, _3, _4)
    case 5 => new IntBatch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int])
      new IntBatchN(7, new IntBatch4(_1, _2, _3, _4), Batch.Empty, new IntBatch3(_5, _6, elem.asInstanceOf[Int]))
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int])
      new IntBatchN(7, new IntBatch3(elem.asInstanceOf[Int], _1, _2), Batch.Empty, new IntBatch4(_3, _4, _5, _6))
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatch6(_1, _2, _3, _4, _5, _6)
}

private[containers] final class IntBatchN
    (override val length: Int, prefix: Batch[Int], tree: Batch[Batch[Int]], suffix: Batch[Int])
  extends Batch[Int] with Reified {
  
  protected override def T: TypeHint[Int] = TypeHint.Int
  
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
  
  override def update[B >: Int](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) {
      val n = index - prefix.length
      if (n < 0) new IntBatchN(length, prefix.update(index, elem).asInstanceOf[Batch[Int]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new IntBatchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Batch[Int]]),
            suffix)
        else new IntBatchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Batch[Int]])
      }
    }
    else lift.update(index, elem)
  }
  
  override def head: Int = prefix.head
  
  override def last: Int = suffix.last
  
  override def init: Batch[Int] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new IntBatchN(length - 1, prefix, tree.init, tree.last)
    }
    else new IntBatchN(length - 1, prefix, tree, suffix.init)
  }
  
  override def tail: Batch[Int] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new IntBatchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new IntBatchN(length - 1, prefix.tail, tree, suffix)
  }
  
  override def drop(lower: Int): Batch[Int] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new IntBatchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new IntBatchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }
  
  override def take(upper: Int): Batch[Int] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new IntBatchN(upper, prefix, split.init, split.last.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new IntBatchN(upper, prefix, tree, suffix.take(k))
    }
  }
  
  override def append[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (suffix.length == 6)
        new IntBatchN(
          length + 1,
          prefix,
          tree :+ new IntBatch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new IntBatch3(suffix(4), suffix(5), elem.asInstanceOf[Int]))
      else new IntBatchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Batch[Int]])
    }
    else lift.append(elem)
  }
  
  override def prepend[B >: Int](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) {
      if (prefix.length == 6)
        new IntBatchN(
          length + 1,
          new IntBatch3(elem.asInstanceOf[Int], prefix(0), prefix(1)),
          new IntBatch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new IntBatchN(length + 1, (elem +: prefix).asInstanceOf[Batch[Int]], tree, suffix)
    }
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Int] = new RefBatchN(length, prefix, tree, suffix)
}
