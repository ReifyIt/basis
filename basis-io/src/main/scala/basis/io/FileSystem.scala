/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.io

/** A hierarchical organization of files.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @contentDiagram
  * @groupprio  Paths   1
  * @groupprio  Files   2
  * @groupprio  Api     3
  */
trait FileSystem {
  /** A location in this file system.
    * 
    * @group  Paths
    * @template
    */
  type Path <: PathApi
  
  /** A factory for paths in this file system.
    * @group Paths */
  val Path: PathFactory
  
  /** General path operations.
    * @group Api */
  trait PathApi { this: Path =>
    /** Returns `true` if this path has no base. */
    def isRoot: Boolean
    
    /** Returns the parent component of this path. */
    def base: Path
    
    /** Returns the name of this path component. */
    def name: String
    
    /** Returns a named subpath with this path as its base. */
    def / (name: String): Path
  }
  
  /** Path factory methods.
    * @group Api */
  trait PathFactory {
    /** Returns a new path parsed from the given string. */
    def apply(name: String): Path
  }
  
  /** A resource in this file system.
    * 
    * @group  Files
    * @template
    */
  type File <: FileApi
  
  /** File factory methods.
    * @group Files */
  val File: FileFactory
  
  /** General file operations.
    * @group Api */
  trait FileApi { this: File =>
    /** Returns `true` if this file has an open file descriptor. */
    def isOpen: Boolean
    
    /** Closes the file descriptor associated with this file. */
    def close(): Unit
    
    /** Returns the size in bytes of this file. */
    def size: Long
    
    /** Set sthe size in bytes of this file. */
    def size_=(newSize: Long): Unit
  }
  
  /** File factory methods.
    * @group Api */
  trait FileFactory {
    /** Returns an open file located at the given path. */
    def apply(path: Path): File
  }
}
