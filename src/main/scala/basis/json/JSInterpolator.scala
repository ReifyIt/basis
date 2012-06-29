/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import language.experimental.macros

class JSInterpolator(stringContext: StringContext) {
  object json {
    def apply(args: JSValue*): JSValue = macro JSBuilderMacro.interpolate
    
    def unapplySeq(jsvalue: JSValue): Option[Seq[JSValue]] = JSMatcher.interpolate(jsvalue, stringContext.parts)
    
    // disabled until macro extractors work
    // def unapplySeq(jsvalue: JSValue): Option[Seq[JSValue]] = macro JSMatcherMacro.interpolate
  }
}
