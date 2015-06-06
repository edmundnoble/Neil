package io.enoble.svg2d

import java.io.File

import io.enoble.svg2d.drawables.Parse

import scalaz._
import Scalaz._

object Scratch {
  def main(args: Array[String]): Unit = {
    val svgFolder = new File("svg")
    val xml = svgFolder.listFiles().map(scala.xml.XML.loadFile)
    val nodes = xml.map(Parse.traverse(x => if (x.label == "text") List(x) else Nil))
    nodes.flatten.foreach(println)
  }
}
