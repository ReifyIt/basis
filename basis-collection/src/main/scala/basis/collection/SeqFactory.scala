/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

import scala.language.higherKinds

trait SeqFactory[CC[X] <: Seq[X]] extends ContainerFactory[CC] {
  import scala.language.experimental.macros
  
  implicit def Equal[A : Equal]: Equal[CC[A]] = new SeqEqual[A]
  
  implicit def Hash[A : Hash]: Hash[CC[A]] = new SeqHash[A]
}
