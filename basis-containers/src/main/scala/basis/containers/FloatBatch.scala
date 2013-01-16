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

private[containers] final class FloatBatch1(_1: Float) extends Batch[Float] with Reified {
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 1
  
  override def apply(index: Int): Float = {
    if (index == 0) _1
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (index == 0) new FloatBatch1(elem.asInstanceOf[Float])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = _1
  
  override def last: Float = _1
  
  override def init: Batch[Float] = Batch.Empty
  
  override def tail: Batch[Float] = Batch.Empty
  
  override def drop(lower: Int): Batch[Float] = if (lower <= 0) this else Batch.Empty
  
  override def take(upper: Int): Batch[Float] = if (upper <= 0) Batch.Empty else this
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch2(_1, elem.asInstanceOf[Float])
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch2(elem.asInstanceOf[Float], _1)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatch1(_1)
}

private[containers] final class FloatBatch2(_1: Float, _2: Float) extends Batch[Float] with Reified {
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 2
  
  override def apply(index: Int): Float = {
    if (index == 0) _1
    else if (index == 1) _2
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (index == 0) new FloatBatch2(elem.asInstanceOf[Float], _2)
      else if (index == 1) new FloatBatch2(_1, elem.asInstanceOf[Float])
      else throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = _1
  
  override def last: Float = _2
  
  override def init: Batch[Float] = new FloatBatch1(_1)
  
  override def tail: Batch[Float] = new FloatBatch1(_2)
  
  override def drop(lower: Int): Batch[Float] = {
    if (lower <= 0) this
    else if (lower == 1) new FloatBatch1(_2)
    else Batch.Empty
  }
  
  override def take(upper: Int): Batch[Float] = {
    if (upper <= 0) Batch.Empty
    else if (upper == 1) new FloatBatch1(_1)
    else this
  }
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch3(_1, _2, elem.asInstanceOf[Float])
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch3(elem.asInstanceOf[Float], _1, _2)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatch2(_1, _2)
}

private[containers] final class FloatBatch3(_1: Float, _2: Float, _3: Float) extends Batch[Float] with Reified {
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 3
  
  override def apply(index: Int): Float = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatBatch3(elem.asInstanceOf[Float], _2, _3)
      case 1 => new FloatBatch3(_1, elem.asInstanceOf[Float], _3)
      case 2 => new FloatBatch3(_1, _2, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = _1
  
  override def last: Float = _3
  
  override def init: Batch[Float] = new FloatBatch2(_1, _2)
  
  override def tail: Batch[Float] = new FloatBatch2(_2, _3)
  
  @tailrec override def drop(lower: Int): Batch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatBatch2(_2, _3)
    case 2 => new FloatBatch1(_3)
    case 3 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(3)
  }
  
  @tailrec override def take(upper: Int): Batch[Float] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new FloatBatch1(_1)
    case 2 => new FloatBatch2(_1, _2)
    case 3 => this
    case _ => if (upper < 0) take(0) else take(3)
  }
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch4(_1, _2, _3, elem.asInstanceOf[Float])
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch4(elem.asInstanceOf[Float], _1, _2, _3)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatch3(_1, _2, _3)
}

private[containers] final class FloatBatch4
    (_1: Float, _2: Float, _3: Float, _4: Float)
  extends Batch[Float] with Reified {
  
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override def isEmpty: Boolean = false
  
  override def length: Int = 4
  
  override def apply(index: Int): Float = (index: @switch) match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatBatch4(elem.asInstanceOf[Float], _2, _3, _4)
      case 1 => new FloatBatch4(_1, elem.asInstanceOf[Float], _3, _4)
      case 2 => new FloatBatch4(_1, _2, elem.asInstanceOf[Float], _4)
      case 3 => new FloatBatch4(_1, _2, _3, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = _1
  
  override def last: Float = _4
  
  override def init: Batch[Float] = new FloatBatch3(_1, _2, _3)
  
  override def tail: Batch[Float] = new FloatBatch3(_2, _3, _4)
  
  @tailrec override def drop(lower: Int): Batch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatBatch3(_2, _3, _4)
    case 2 => new FloatBatch2(_3, _4)
    case 3 => new FloatBatch1(_4)
    case 4 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(4)
  }
  
  @tailrec override def take(upper: Int): Batch[Float] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new FloatBatch1(_1)
    case 2 => new FloatBatch2(_1, _2)
    case 3 => new FloatBatch3(_1, _2, _3)
    case 4 => this
    case _ => if (upper < 0) take(0) else take(4)
  }
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch5(_1, _2, _3, _4, elem.asInstanceOf[Float])
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch5(elem.asInstanceOf[Float], _1, _2, _3, _4)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatch4(_1, _2, _3, _4)
}

private[containers] final class FloatBatch5
    (_1: Float, _2: Float, _3: Float, _4: Float, _5: Float)
  extends Batch[Float] with Reified {
  
  protected override def T: TypeHint[Float] = TypeHint.Float
  
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
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatBatch5(elem.asInstanceOf[Float], _2, _3, _4, _5)
      case 1 => new FloatBatch5(_1, elem.asInstanceOf[Float], _3, _4, _5)
      case 2 => new FloatBatch5(_1, _2, elem.asInstanceOf[Float], _4, _5)
      case 3 => new FloatBatch5(_1, _2, _3, elem.asInstanceOf[Float], _5)
      case 4 => new FloatBatch5(_1, _2, _3, _4, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = _1
  
  override def last: Float = _5
  
  override def init: Batch[Float] = new FloatBatch4(_1, _2, _3, _4)
  
  override def tail: Batch[Float] = new FloatBatch4(_2, _3, _4, _5)
  
  @tailrec override def drop(lower: Int): Batch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatBatch4(_2, _3, _4, _5)
    case 2 => new FloatBatch3(_3, _4, _5)
    case 3 => new FloatBatch2(_4, _5)
    case 4 => new FloatBatch1(_5)
    case 5 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(5)
  }
  
  @tailrec override def take(upper: Int): Batch[Float] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new FloatBatch1(_1)
    case 2 => new FloatBatch2(_1, _2)
    case 3 => new FloatBatch3(_1, _2, _3)
    case 4 => new FloatBatch4(_1, _2, _3, _4)
    case 5 => this
    case _ => if (upper < 0) take(0) else take(5)
  }
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Float])
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) new FloatBatch6(elem.asInstanceOf[Float], _1, _2, _3, _4, _5)
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatch5(_1, _2, _3, _4, _5)
}

private[containers] final class FloatBatch6
    (_1: Float, _2: Float, _3: Float, _4: Float, _5: Float, _6: Float)
  extends Batch[Float] with Reified {
  
  protected override def T: TypeHint[Float] = TypeHint.Float
  
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
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) (index: @switch) match {
      case 0 => new FloatBatch6(elem.asInstanceOf[Float], _2, _3, _4, _5, _6)
      case 1 => new FloatBatch6(_1, elem.asInstanceOf[Float], _3, _4, _5, _6)
      case 2 => new FloatBatch6(_1, _2, elem.asInstanceOf[Float], _4, _5, _6)
      case 3 => new FloatBatch6(_1, _2, _3, elem.asInstanceOf[Float], _5, _6)
      case 4 => new FloatBatch6(_1, _2, _3, _4, elem.asInstanceOf[Float], _6)
      case 5 => new FloatBatch6(_1, _2, _3, _4, _5, elem.asInstanceOf[Float])
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = _1
  
  override def last: Float = _6
  
  override def init: Batch[Float] = new FloatBatch5(_1, _2, _3, _4, _5)
  
  override def tail: Batch[Float] = new FloatBatch5(_2, _3, _4, _5, _6)
  
  @tailrec override def drop(lower: Int): Batch[Float] = (lower: @switch) match {
    case 0 => this
    case 1 => new FloatBatch5(_2, _3, _4, _5, _6)
    case 2 => new FloatBatch4(_3, _4, _5, _6)
    case 3 => new FloatBatch3(_4, _5, _6)
    case 4 => new FloatBatch2(_5, _6)
    case 5 => new FloatBatch1(_6)
    case 6 => Batch.Empty
    case _ => if (lower < 0) drop(0) else drop(6)
  }
  
  @tailrec override def take(upper: Int): Batch[Float] = (upper: @switch) match {
    case 0 => Batch.Empty
    case 1 => new FloatBatch1(_1)
    case 2 => new FloatBatch2(_1, _2)
    case 3 => new FloatBatch3(_1, _2, _3)
    case 4 => new FloatBatch4(_1, _2, _3, _4)
    case 5 => new FloatBatch5(_1, _2, _3, _4, _5)
    case 6 => this
    case _ => if (upper < 0) take(0) else take(6)
  }
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float])
      new FloatBatchN(7, new FloatBatch4(_1, _2, _3, _4), Batch.Empty, new FloatBatch3(_5, _6, elem.asInstanceOf[Float]))
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float])
      new FloatBatchN(7, new FloatBatch3(elem.asInstanceOf[Float], _1, _2), Batch.Empty, new FloatBatch4(_3, _4, _5, _6))
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatch6(_1, _2, _3, _4, _5, _6)
}

private[containers] final class FloatBatchN
    (override val length: Int, prefix: Batch[Float], tree: Batch[Batch[Float]], suffix: Batch[Float])
  extends Batch[Float] with Reified {
  
  protected override def T: TypeHint[Float] = TypeHint.Float
  
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
  
  override def update[B >: Float](index: Int, elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) {
      val n = index - prefix.length
      if (n < 0) new FloatBatchN(length, prefix.update(index, elem).asInstanceOf[Batch[Float]], tree, suffix)
      else {
        val k = n - (tree.length << 2)
        if (k < 0)
          new FloatBatchN(
            length,
            prefix,
            tree.update(n >> 2, tree(n >> 2).update(n & 3, elem).asInstanceOf[Batch[Float]]),
            suffix)
        else new FloatBatchN(length, prefix, tree, suffix.update(index, elem).asInstanceOf[Batch[Float]])
      }
    }
    else lift.update(index, elem)
  }
  
  override def head: Float = prefix.head
  
  override def last: Float = suffix.last
  
  override def init: Batch[Float] = {
    if (suffix.length == 1) {
      if (tree.isEmpty) prefix
      else new FloatBatchN(length - 1, prefix, tree.init, tree.last)
    }
    else new FloatBatchN(length - 1, prefix, tree, suffix.init)
  }
  
  override def tail: Batch[Float] = {
    if (prefix.length == 1) {
      if (tree.isEmpty) suffix
      else new FloatBatchN(length - 1, tree.head, tree.tail, suffix)
    }
    else new FloatBatchN(length - 1, prefix.tail, tree, suffix)
  }
  
  override def drop(lower: Int): Batch[Float] = {
    val n = lower - prefix.length
    if (lower <= 0) this
    else if (n < 0) new FloatBatchN(length - lower, prefix.drop(lower), tree, suffix)
    else {
      val k = n - (tree.length << 2)
      if (k < 0) {
        val split = tree.drop(n >> 2)
        new FloatBatchN(length - lower, split.head.drop(n & 3), split.tail, suffix)
      }
      else suffix.drop(k)
    }
  }
  
  override def take(upper: Int): Batch[Float] = {
    val n = upper - prefix.length
    if (upper == length) this
    else if (n <= 0) prefix.take(upper)
    else {
      val k = n - (tree.length << 2)
      if (k <= 0) {
        val split = tree.take(((n + 3) & ~3) >> 2)
        new FloatBatchN(upper, prefix, split.init, split.last.take(((((n & 3) ^ 3) + 1) & 4) | (n & 3)))
      }
      else new FloatBatchN(upper, prefix, tree, suffix.take(k))
    }
  }
  
  override def append[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (suffix.length == 6)
        new FloatBatchN(
          length + 1,
          prefix,
          tree :+ new FloatBatch4(suffix(0), suffix(1), suffix(2), suffix(3)),
          new FloatBatch3(suffix(4), suffix(5), elem.asInstanceOf[Float]))
      else new FloatBatchN(length + 1, prefix, tree, (suffix :+ elem).asInstanceOf[Batch[Float]])
    }
    else lift.append(elem)
  }
  
  override def prepend[B >: Float](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Float]) {
      if (prefix.length == 6)
        new FloatBatchN(
          length + 1,
          new FloatBatch3(elem.asInstanceOf[Float], prefix(0), prefix(1)),
          new FloatBatch4(prefix(2), prefix(3), prefix(4), prefix(5)) +: tree,
          suffix)
      else new FloatBatchN(length + 1, (elem +: prefix).asInstanceOf[Batch[Float]], tree, suffix)
    }
    else lift.prepend(elem)
  }
  
  private[this] def lift: Batch[Float] = new RefBatchN(length, prefix, tree, suffix)
}
