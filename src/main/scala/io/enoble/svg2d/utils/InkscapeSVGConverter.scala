package io
package enoble
package svg2d
package utils

import java.io.{FileFilter, File}

import scala.sys.process._

object InkscapeSVGConverter {
  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      Console.err.println("Wrong number of arguments!")
      sys.exit(1)
    }
    val inkscapePath = args(0)
    val inSvgFolderName = args(1)
    val outSvgFolderName = args(2)
    def prc(file: File) = {
      val fileName = file.getName
      s"$inkscapePath -l $outSvgFolderName/$fileName $inSvgFolderName/$fileName"
    }
    val inSvgFolder = new File(inSvgFolderName)
    val fileFilter = new FileFilter() {
      override def accept(pathname: File): Boolean = pathname.getName.endsWith(".svg")
    }
    val outSvgFolder = new File(outSvgFolderName)
    outSvgFolder.mkdirs()
    val files = inSvgFolder.listFiles(fileFilter)
    files.map(prc).foreach(_.!!)
  }

}
