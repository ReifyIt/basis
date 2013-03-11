/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.io

trait FileSystem {
  type Path <: PathApi
  
  trait PathApi { this: Path =>
    def isRoot: Boolean
    
    def base: Path
    
    def name: String
    
    def / (name: String): Path
  }
  
  trait PathFactory {
    def apply(name: String): Path
  }
  
  val Path: PathFactory
  
  
  type File <: FileApi
  
  trait FileApi { this: File =>
    def isOpen: Boolean
    
    def close(): Unit
    
    def size: Long
    
    def size_=(newSize: Long): Unit
  }
  
  trait FileFactory {
    def apply(path: Path): File
  }
  
  val File: FileFactory
}
