/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object text extends ImplicitStringBuffers

package text {
  private[text] class LowPriorityImplicitStringBuffers {
    implicit def String2Buffer: String2Buffer = new String2Buffer
    
    implicit def String4Buffer: String4Buffer = new String4Buffer
  }
  
  private[text] class ImplicitStringBuffers extends LowPriorityImplicitStringBuffers {
    implicit def String1Buffer: String1Buffer = new String1Buffer
  }
}
