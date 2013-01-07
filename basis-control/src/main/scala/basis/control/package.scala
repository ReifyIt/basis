/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Control-flow utilities. */
package object control {
  /** Implicitly adds standard operations to `Try` results. */
  implicit def TryOps[T](result: Try[T]): TryOps[T] = macro TryMacros.TryOps[T]
}
