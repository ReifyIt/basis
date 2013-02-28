/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.parallel

import basis.collections._

class General {
  implicit def GeneralEnumeratorOps[A](these: Compound[Enumerator[A]]): GeneralEnumeratorOps[A] =
    new GeneralEnumeratorOps[A](these)
}
