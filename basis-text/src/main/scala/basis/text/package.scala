/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object text extends StringBuffers {
  implicit override def UTF8Buffer: UTF8Buffer = new UTF8Buffer
}

package text {
  private[text] class StringBuffers {
    implicit def UTF8Buffer: UTF8Buffer = new UTF8Buffer
    
    implicit def UTF16Buffer: UTF16Buffer = new UTF16Buffer
    
    implicit def UTF32Buffer: UTF32Buffer = new UTF32Buffer
  }
}
