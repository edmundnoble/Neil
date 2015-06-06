package io.enoble.svg2d

import java.io.File

import com.squareup.javapoet.MethodSpec
import io.enoble.svg2d.drawables.Parse

import scala.util.parsing.combinator._
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
    println(Parse.parseAll(xml))
  }
}

