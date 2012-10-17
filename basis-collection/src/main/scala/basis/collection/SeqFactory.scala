/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

trait SeqFactory[CC[X] <: Seq[X]] extends ContainerFactory[CC] {
  implicit def Equal[A : Equal]: Equal[CC[A]] = new SeqEqual[A]
  
  implicit def Hash[A : Hash]: Hash[CC[A]] = new SeqHash[A]
}
