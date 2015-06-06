package io.enoble.svg2d.utils

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
    def prc(file: String) = s"$inkscapePath -l $outSvgFolderName/$file $inSvgFolderName/$file"
    val inSvgFolder = new File(inSvgFolderName)
    val fileFilter = new FileFilter() {
      override def accept(pathname: File): Boolean = pathname.getName.endsWith(".svg")
    }
    val outSvgFolder = new File(outSvgFolderName)
    val files = inSvgFolder.listFiles(fileFilter)
    files.par.map(prc _ compose ((_: File).getName)).foreach(_.!!)
  }

}