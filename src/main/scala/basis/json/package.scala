/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** Contains data types, parsers, and string interpolation macros for
  * '''J'''avascript '''O'''bject '''NO'''tation.
  * 
  * ==Parsers==
  * 
  * [[basis.json.JSONParser]] provides a fast, single-character lookahead
  * recursive descent parser. [[basis.json.JSONContext]] instances compose
  * with parsers to build JSON values.
  * 
  * ==String interpolators==
  * 
  * Parsing of interpolated JSON text happens at compile-time when using one
  * of the following interpolation methods:
  * 
  *   - `json""` – statically parses any JSON value.
  *   - `jsobject""` – statically parses a JSON object.
  *   - `jsarray""` – statically parses a JSON array.
  * 
  * @example {{{
  * scala> import basis.json._
  * import basis.json._
  * 
  * scala> json""" [{}, [], "", 0, 0.0, true, false, null] """
  * res0: basis.json.JSArray = [{},[],"",0,0.0,true,false,null]
  * 
  * scala> def person(name: String, age: Int) = jsobject""" { "name" : $name, "age" : $age } """
  * person: (name: String, age: Int)basis.json.JSObject
  * 
  * scala> person("Bart Simpson", 10)
  * res1: basis.json.JSObject = {"name":"Bart Simpson","age":10}
  * }}}
  */
package object json {
  implicit class JSONStringContext(stringContext: StringContext) {
    import language.experimental.macros
    
    def jsarray(args: JSValue*): JSArray = macro JSONMacros.buildJSArray
    
    def jsobject(args: JSValue*): JSObject = macro JSONMacros.buildJSObject
    
    object json {
      def apply(args: JSValue*): JSValue = macro JSONMacros.buildJSValue
      
      def unapplySeq(jsvalue: JSValue): Option[Seq[JSValue]] = macro JSONMacros.matchJSValue
    }
  }
}
