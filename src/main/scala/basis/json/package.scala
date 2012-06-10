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
  * ==Data types==
  * 
  * [[basis.json.JSONContext]] serves as the foundation for all JSON
  * implementations and provides abstract types and builders for JSON values.
  * Implementations include:
  * 
  *   - `JSONTree` – algebraic data types supporting jquery-style selectors;
  *     selection operations return updated root trees, enabling intuitive
  *     sub-tree transformations of immutable trees.
  *   - `JSONType` – represents JSON values with general Scala types.
  *   - `JSONExpr` – generates expressions that build data structures for
  *     another JSON implementation; used by string interpolators to
  *     transform textual JSON to builder operations at compile time.
  * 
  * ==Parsers==
  * 
  * [[basis.json.JSONParser]] provides a fast, single-character lookahead
  * recursive descent parser. [[basis.json.JSONContext]] instances compose
  * with parsers to build JSON values. `JSONParser`'s companion object
  * contains parser implementations for various input sources.
  * 
  * ==String interpolators==
  * 
  * Parsing of interpolated JSON text happens at compile time. `JSONTree` and
  * `JSONType` each provide the following string interpolation methods:
  * 
  *   - `json""` – statically parses any JSON value.
  *   - `jsobject""` – statically parses a JSON object.
  *   - `jsarray""` – statically parses a JSON array.
  * 
  * @example {{{
  * scala> import basis.json.JSONTree._
  * import basis.json.JSONTree._
  * 
  * scala> json""" [{}, [], "", 0, 0.0, true, false, null] """
  * res0: basis.json.JSONTree.JSArray = [{},[],"",0,0.0,true,false,null]
  * 
  * scala> def person(name: String, age: Int) =
  *      |   jsobject""" { "name" : $name, "age" : $age } """
  * person: (name: String, age: Int)basis.json.JSONTree.JSObject
  * 
  * scala> person("Bart Simpson", 10)
  * res1: basis.json.JSONTree.JSObject = {"name":"Bart Simpson","age":10}
  * }}}
  */
package object json
