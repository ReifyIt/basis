/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

import basis.collections._

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
  /** Implicitly adds extended operations to `String` values. */
  implicit def StringOps(s: String): StringOps = new StringOps(s)
  
  /** Implicitly returns a new string builder.
    * @group Builders */
  implicit def StringBuilder: StringBuilder { type Scope = String; type State = String } =
    new JavaStringBuilder
  
  /** Implicitly returns a new UTF-8 string builder.
    * @group Builders */
  implicit def String1Builder: StringBuilder { type Scope = UTF8; type State = String1 } =
    new String1Builder
  
  /** Implicitly returns a new UTF-16 string builder.
    * @group Builders */
  implicit def String2Builder: StringBuilder { type Scope = UTF16; type State = String2 } =
    new String2Builder
  
  /** Implicitly returns a new UTF-32 string builder.
    * @group Builders */
  implicit def String4Builder: StringBuilder { type Scope = UTF32; type State = String4 } =
    new String4Builder
  
  /** Returns a new builder that appends to the given builder code points decoded from input UTF-8 code units.
    * @group Builder */
  def UTF8Builder(self: Builder[Int]): Builder[Int] { type Scope = self.Scope; type State = self.State } =
    new UTF8Builder(self).asInstanceOf[Builder[Int] { type Scope = self.Scope; type State = self.State }]
  
  /** Returns a new builder that appends to the given builder code points decoded from input UTF-16 code units.
    * @group Builders */
  def UTF16Builder(self: Builder[Int]): Builder[Int] { type Scope = self.Scope; type State = self.State } =
    new UTF16Builder(self).asInstanceOf[Builder[Int] { type Scope = self.Scope; type State = self.State }]
}
