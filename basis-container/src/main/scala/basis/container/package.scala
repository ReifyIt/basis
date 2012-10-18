/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object container {
  implicit def ArrayOps[A](self: Array[A]): ArrayOps[self.Self, A] =
    new ArrayOps[self.Self, A](self)
  
  implicit def ListOps[A](self: List[A]): ListOps[A] = new ListOps[A](self)
}
