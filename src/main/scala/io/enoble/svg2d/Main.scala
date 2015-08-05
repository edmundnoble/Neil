package io.enoble.svg2d

import java.io.File

import io.enoble.svg2d.parsing.Parse

import scalaz._
import Scalaz._
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      Console.err.println("Wrong number of arguments!")
      sys.exit(1)
    }
    val xml = scala.xml.XML.loadFile(new File(args(0)))
    val parsed = Parse.parseAll(xml)
    println(parsed)
  }
}

