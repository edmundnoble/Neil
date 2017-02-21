package io
package enoble
package svg2d
package utils

import java.io.File

import io.enoble.svg2d.xmlparse.Parse
import io.enoble.svg2d.render.{AndroidRenderer, InitialRenderer}

import scalaz._
import Scalaz._
import scala.io.Source

object Timed {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      Console.err.println("Wrong number of arguments!")
      sys.exit(1)
    }
    val xml = scala.xml.XML.loadFile(new File(args(0)))
    var time = System.nanoTime
    val parsed = Parse.parseAll(AndroidRenderer)(xml)
    var timeAfter = System.nanoTime
    println(s"Result: $parsed")
    println(s"Time taken: ${(timeAfter - time) / 1000000.0} milliseconds")
    time = System.nanoTime
    val code =
     parsed.flatten.map(_.asString)
    timeAfter = System.nanoTime
    println(s"Time taken to generate code: ${(timeAfter - time) / 1000000.0} milliseconds")
    println(s"code: $code")
  }

}
