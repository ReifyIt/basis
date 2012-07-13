/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis.collection._

sealed abstract class List[+A] extends Listed[A] {
  override type Scope <: List[A]
  
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: List[A]
  
  def :: [B >: A](head: B): List[B] = new ::(head, this)
}

final class ::[A](override val head: A, private[container] var rest: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
  
  override def tail: List[A] = rest
}

object :: {
  def unapply[A](cons: ::[A]): Some[(A, List[A])] = Some(cons.head, cons.tail)
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  
  override def tail: List[Nothing] = throw new UnsupportedOperationException("tail of empty list")
  
  override def toString: String = "Nil"
}

object List {
  implicit def Builder[A]: Builder[A] = new Builder[A]
  
  final class Builder[A] extends Collector[Any, A] {
    override type Product = List[A]
    
    private[this] var last: ::[A] = _
    private[this] var first: List[A] = Nil
    private[this] var aliased: Boolean = false
    
    private[this] def prepare(): Unit = if (aliased) {
      var rest = first
      last = new ::(rest.head, Nil)
      first = last
      rest = rest.tail
      while (!rest.isEmpty) {
        val link = last
        last = new ::(rest.head, Nil)
        link.rest = last
        rest = rest.tail
      }
      aliased = false
    }
    
    override def expect(count: Int): Unit = ()
    
    override def += (item: A) {
      if (first.isEmpty) {
        last = new ::(item, Nil)
        first = last
      }
      else {
        prepare()
        val link = last
        last = new ::(item, Nil)
        link.rest = last
      }
    }
    
    override def ++= (elements: Incremental[A]): Unit = elements match {
      case items: ::[A] =>
        prepare()
        last.rest = items
        while (!last.rest.isEmpty) last = last.rest.asInstanceOf[::[A]]
        aliased = true
      case _ => super.++=(elements)
    }
    
    override def result(): List[A] = {
      if (!first.isEmpty) aliased = true
      first
    }
    
    override def clear() {
      first = Nil
      last = null
      aliased = false
    }
  }
}
