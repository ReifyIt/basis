/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

import language.implicitConversions

/** Contains data types, parsers, and string interpolation macros for
  * '''J'''avascript '''O'''bject '''NO'''tation.
  * 
  * ==String interpolation==
  * 
  * The `json""` string prefix parses and interpolates JSON text at compile-time.
  * Interpolated JSON text gets replaced by code that efficiently generates an
  * equivalent tree at runtime with interpolated arguments built-in.
  * 
  * @example {{{
  * scala> import basis.json._
  * import basis.json._
  * 
  * scala> json""" [{}, [], "", 0, 0.0, true, false, null] """
  * res0: basis.json.JSArray = [{},[],"",0,0.0,true,false,null]
  * 
  * scala> def person(name: String, age: Int) = json""" { "name" : \$name, "age" : \$age } """
  * person: (name: String, age: Int)basis.json.JSObject
  * 
  * scala> person("Bart Simpson", 10)
  * res1: basis.json.JSObject = {"name":"Bart Simpson","age":10}
  * 
  * scala> val places = json"""{"San Francisco":{"areaCode":415},"New York":{"areaCode":212}}"""
  * places: basis.json.JSObject = {"San Francisco":{"areaCode":415},"New York":{"areaCode":212}}
  * 
  * scala> for (JSInteger(areaCode) <- places \ "San Francisco" \ "areaCode") yield areaCode + 235
  * res2: basis.json.JSObject = {"San Francisco":{"areaCode":650},"New York":{"areaCode":212}}
  * 
  * scala> (json""" ["Hello", 0, ", ", [null, "world", "!"]] """ \\ +JSString).foldLeft("")(_ + _.value)
  * res3: String = Hello, world!
  * }}}
  */
package object json {
  implicit def jsinterpolator(stringContext: StringContext): JSInterpolator =
    new JSInterpolator(stringContext)
}
