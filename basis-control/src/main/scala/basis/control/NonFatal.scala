/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** Non-fatal throwable extractor. */
object NonFatal {
  /** Returns `true` for non-fatal throwables, and `false` for fatal ones. */
  def apply(throwable: Throwable): Boolean = throwable match {
    case _: StackOverflowError => true
    case _: VirtualMachineError  |
         _: ThreadDeath          |
         _: InterruptedException |
         _: LinkageError         |
         _: NotImplementedError => false
    case _ => true
  }
  
  /** Extracts non-fatal throwables, returning `None` for fatal ones. */
  def unapply(throwable: Throwable): Option[Throwable] =
    if (NonFatal(throwable)) Some(throwable) else None
}
