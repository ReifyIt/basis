//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

/** Unicode® collections.
  *
  * @contentDiagram hideNodes "basis.text.StringFactory"
  */
package object text {
  implicit def StringBuilder: StringBuilder with Family[String] with State[String] = new JStringBuilder

  implicit final class UStringContext(val __ : StringContext) extends AnyVal {
    def u(args: Any*): UString = new UString(__.s(args: _*))
  }
}
