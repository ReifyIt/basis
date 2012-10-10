/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

final class ListBuffer[-Source, A] extends Buffer[Source, A] {
  override type State = LinkedList[A]
  
  private[this] var last: ::[A] = _
  
  private[this] var first: LinkedList[A] = Nil
  
  private[this] var aliased: Boolean = false
  
  private[this] def prepare(): Unit = if (aliased) {
    var these = first
    last = new ::(these.head, Nil)
    first = last
    these = these.tail
    while (!these.isEmpty) {
      val link = last
      last = new ::(these.head, Nil)
      link.tail = last
      these = these.tail
    }
    aliased = false
  }
  
  override def += (that: A): this.type = {
    if (first.isEmpty) {
      last = new ::(that, Nil)
      first = last
    }
    else {
      prepare()
      val link = last
      last = new ::(that, Nil)
      link.tail = last
    }
    this
  }
  
  override def expect(count: Int): this.type = this
  /*
  override def ++= (those: basis.Enumerator[A]): Unit = those match {
    case those: ::[A] =>
      prepare()
      last.tail = those
      while (!last.tail.isEmpty) last = last.tail.asInstanceOf[::[A]]
      aliased = true
    case _ => super.++=(those)
  }
  */
  override def check: LinkedList[A] = {
    if (!first.isEmpty) aliased = true
    first
  }
  
  override def clear() {
    last = null
    first = Nil
    aliased = false
  }
}
