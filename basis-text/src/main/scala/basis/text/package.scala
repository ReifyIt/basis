/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Unicode® string implementations.
  * 
  * @groupname  Strings   Unicode® strings
  * @groupprio  Strings   1
  * 
  * @groupname  Unicode   Unicode® formats
  * @groupprio  Unicode   2
  * 
  * @groupname  Builders  String builders
  * @groupprio  Builders  3
  */
package object text {
  /** Implicitly returns a new String builder.
    * @group Builders */
  implicit def StringBuilder: StringBuilder[Any] { type State = String } = new JavaStringBuilder
}
