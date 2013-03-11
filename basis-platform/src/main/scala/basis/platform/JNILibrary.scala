/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.platform

import java.io.{File, FileOutputStream}

private[platform] object JNILibrary {
  load()
  
  def init(): Unit = ()
  
  private def load() = {
    System.getProperty("os.name") match {
      case "Mac OS X" =>
        loadEmbeddedLibrary("/libbasis-platform.macosx-universal.jnilib", File.createTempFile("libbasis-platform", "jnilib"))
    }
  }
  
  private def loadEmbeddedLibrary(path: String, file: File) {
    val in = getClass.getResourceAsStream(path)
    val out = try new FileOutputStream(file) catch { case e: Throwable => in.close(); throw e }
    try {
      val buffer = new Array[Byte](1024)
      var count = in.read(buffer)
      while (count > 0) {
        out.write(buffer, 0, count)
        count = in.read(buffer)
      }
    }
    finally {
      in.close()
      out.close()
    }
    System.load(file.getCanonicalPath)
  }
}
