package io.enoble.svg2d.utils

import java.io.File

import io.enoble.svg2d.xmlparse.Parse

import scalaz.Scalaz._
import scalaz._

object Scratch {
  def main(args: Array[String]): Unit = {
    val svgFolder = new File("svg")
    val xml = svgFolder.listFiles().map(scala.xml.XML.loadFile)
    val nodes = xml.map(Parse.foldXml(x => if (x.label == "text") List(x) else Nil)(_)(Nil, _ ++ _))
    nodes.flatten.foreach(println)
  }
}
