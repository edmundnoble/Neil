package io.enoble.svg2d.utils

import java.io.File

import io.enoble.svg2d.parsing.Parse
import io.enoble.svg2d.render.InitialRenderer

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
    val parsed = Parse.parseAll(InitialRenderer)(xml)
    var timeAfter = System.nanoTime
    println(s"Result: $parsed")
    println(s"Time taken: ${(timeAfter - time) / 1000000.0} milliseconds")
    time = System.nanoTime
    val code = "" // TODO:
    // parsed.map(_.toAndroidCode.run(Map[String, Int]())._2.asString)
    timeAfter = System.nanoTime
    println(s"Time taken to generate code: ${(timeAfter - time) / 1000000.0} milliseconds")

  }

}
