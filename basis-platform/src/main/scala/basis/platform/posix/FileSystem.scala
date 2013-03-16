/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.platform
package posix

import basis.io._
import basis.memory._
import basis.util._

object FileSystem extends DataFileSystem {
  sealed abstract class Path extends PathApi {
    override def isRoot: Boolean
    
    override def base: Path
    
    override def name: String
    
    override def / (name: String): Path = new SubPath(this, name)
    
    override def toString: String = {
      val s = new java.lang.StringBuilder
      addString(s)
      s.toString
    }
    
    private[posix] def addString(s: java.lang.StringBuilder): Unit
  }
  
  private[posix] final class RootPath extends Path {
    override def isRoot: Boolean = true
    
    override def base: Path = throw new UnsupportedOperationException("base of root path")
    
    override def name: String = throw new NoSuchElementException("name of root path")
    
    private[posix] override def addString(s: java.lang.StringBuilder): Unit = s.append('/')
  }
  
  private[posix] final class SubPath(parent: Path, override val name: String) extends Path {
    override def isRoot: Boolean = parent == null
    
    override def base: Path = {
      if (parent == null) throw new UnsupportedOperationException("base of relative path root")
      parent
    }
    
    private[posix] override def addString(s: java.lang.StringBuilder) {
      if (parent != null) {
        parent.addString(s)
        if (!parent.isRoot) s.append('/')
      }
      s.append(name)
    }
  }
  
  object Path extends PathFactory {
    def apply(name: String): Path = new SubPath(null, name)
  }
  
  val Root : Path = new RootPath
  
  
  final class File private[posix] (private[posix] var fd: Int) extends DataFileApi {
    override def isOpen: Boolean = fd >= 0
    
    @native override def close(): Unit
    
    @native override def size: Long
    
    @native override def size_=(newSize: Long): Unit
    
    override def data: FileData = FileData.map(0, size, MapProt.ReadWrite, MapMode.Shared, this, 0)
    
    JNILibrary.init()
  }
  
  object File extends super.FileFactory {
    override def apply(path: Path): File = {
      import OpenMode._
      import FileMode._
      open(path, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
    }
    
    @native def open(path: Path, flags: OpenMode, mode: FileMode): File
    
    JNILibrary.init()
  }
  
  
  final class FileData private[posix] (_base: Long, _size: Long) extends NativeData(_base, _size) {
    @native protected override def finalize(): Unit
    
    override def copy(size: Long): Data = {
      val newData = Data(size)
      Data.copy(this, 0, newData, 0, this.size min size)
      newData
    }
  }
  
  object FileData {
    @native def map(address: Long, length: Long, prot: MapProt, flags: MapMode, file: File, offset: Long): FileData
  }
  
  
  final class FileMode(val value: Int) extends AnyVal {
    def contains(mode: FileMode): Boolean = (value | mode.value) == value
    
    @inline def & (mode: FileMode): FileMode = new FileMode(value & mode.value)
    
    @inline def | (mode: FileMode): FileMode = new FileMode(value | mode.value)
  }
  
  object FileMode {
    val S_IFBLK: FileMode = new FileMode(0)
    val S_IFCHR: FileMode = new FileMode(0)
    val S_IFIFO: FileMode = new FileMode(0)
    val S_IFREG: FileMode = new FileMode(0)
    val S_IFDIR: FileMode = new FileMode(0)
    val S_IFLNK: FileMode = new FileMode(0)
    val S_IFSOCK: FileMode = new FileMode(0)
    
    val S_IRUSR: FileMode = new FileMode(0)
    val S_IWUSR: FileMode = new FileMode(0)
    val S_IXUSR: FileMode = new FileMode(0)
    val S_IRGRP: FileMode = new FileMode(0)
    val S_IWGRP: FileMode = new FileMode(0)
    val S_IXGRP: FileMode = new FileMode(0)
    val S_IROTH: FileMode = new FileMode(0)
    val S_IWOTH: FileMode = new FileMode(0)
    val S_IXOTH: FileMode = new FileMode(0)
    val S_ISUID: FileMode = new FileMode(0)
    val S_ISGID: FileMode = new FileMode(0)
    val S_ISVTX: FileMode = new FileMode(0)
    
    def UserRead: FileMode = S_IRUSR
    def UserWrite: FileMode = S_IWUSR
    def UserExec: FileMode = S_IXUSR
    def GroupRead: FileMode = S_IRGRP
    def GroupWrite: FileMode = S_IWGRP
    def GroupExec: FileMode = S_IXGRP
    def OtherRead: FileMode = S_IROTH
    def OtherWrite: FileMode = S_IWOTH
    def OtherExec: FileMode = S_IXOTH
    
    JNILibrary.init()
  }
  
  
  final class OpenMode(val value: Int) extends AnyVal {
    def contains(mode: OpenMode): Boolean = (value | mode.value) == value
    
    @inline def & (mode: OpenMode): OpenMode = new OpenMode(value & mode.value)
    
    @inline def | (mode: OpenMode): OpenMode = new OpenMode(value | mode.value)
  }
  
  object OpenMode {
    val O_RDONLY: OpenMode = new OpenMode(0)
    val O_WRONLY: OpenMode = new OpenMode(0)
    val O_RDWR: OpenMode = new OpenMode(0)
    
    val O_CREAT: OpenMode = new OpenMode(0)
    val O_EXCL: OpenMode = new OpenMode(0)
    val O_NOCTTY: OpenMode = new OpenMode(0)
    val O_TRUNC: OpenMode = new OpenMode(0)
    
    val O_APPEND: OpenMode = new OpenMode(0)
    val O_DSYNC: OpenMode = new OpenMode(0)
    val O_NONBLOCK: OpenMode = new OpenMode(0)
    val O_RSYNC: OpenMode = new OpenMode(0)
    val O_SYNC: OpenMode = new OpenMode(0)
    
    def Read: OpenMode = O_RDONLY
    def Write: OpenMode = O_WRONLY
    def ReadWrite: OpenMode = O_RDWR
    def Create: OpenMode = O_CREAT
    def Truncate: OpenMode = O_TRUNC
    def Append: OpenMode = O_APPEND
    
    JNILibrary.init()
  }
  
  
  final class MapMode(val value: Int) extends AnyVal {
    def contains(mode: MapMode): Boolean = (value | mode.value) == value
    
    @inline def & (mode: MapMode): MapMode = new MapMode(value & mode.value)
    
    @inline def | (mode: MapMode): MapMode = new MapMode(value | mode.value)
  }
  
  object MapMode {
    val MAP_SHARED: MapMode = new MapMode(0)
    val MAP_PRIVATE: MapMode = new MapMode(0)
    val MAP_FIXED: MapMode = new MapMode(0)
    
    def Shared: MapMode = MAP_SHARED
    def Private: MapMode = MAP_PRIVATE
    def Fixed: MapMode = MAP_FIXED
    
    JNILibrary.init()
  }
  
  
  final class MapProt(val value: Int) extends AnyVal {
    def contains(prot: MapProt): Boolean = (value | prot.value) == value
    
    @inline def & (prot: MapProt): MapProt = new MapProt(value & prot.value)
    
    @inline def | (prot: MapProt): MapProt = new MapProt(value | prot.value)
  }
  
  object MapProt {
    val PROT_READ: MapProt = new MapProt(0)
    val PROT_WRITE: MapProt = new MapProt(0)
    val PROT_EXEC: MapProt = new MapProt(0)
    val PROT_NONE: MapProt = new MapProt(0)
    
    def Read: MapProt = PROT_READ
    def Write: MapProt = PROT_WRITE
    def ReadWrite: MapProt = PROT_READ | PROT_WRITE
    
    JNILibrary.init()
  }
}
