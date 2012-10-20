/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collection._

/** A specialized iterator over Unicode® characters.
  * 
  * @define collection  reader
  */
trait Reader extends Any with Iterator[Char] {
  override def head: Char
  
  override def dup: Reader
}

/** Singular readers. */
object Reader {
  object empty extends Reader {
    override def isDone: Boolean = false
    
    override def isEmpty: Boolean = true
    
    override def head: Char =
      throw new scala.NoSuchElementException("Empty reader has no character.")
    
    override def step(): Unit =
      throw new java.lang.UnsupportedOperationException("Can't advance empty reader.")
    
    override def dup: empty.type = this
    
    protected override def foreach[U](f: Char => U): Unit = ()
    
    override def toString: String = "empty"
  }
  
  object done extends Reader {
    override def isDone: Boolean = true
    
    override def isEmpty: Boolean = true
    
    override def head: Char =
      throw new scala.NoSuchElementException("Done reader has no character.")
    
    override def step(): Unit =
      throw new java.lang.UnsupportedOperationException("Can't advance done reader.")
    
    override def dup: done.type = this
    
    protected override def foreach[U](f: Char => U): Unit = ()
    
    override def toString: String = "done"
  }
}
