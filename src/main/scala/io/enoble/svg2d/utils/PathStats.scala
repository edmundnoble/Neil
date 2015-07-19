package io.enoble.svg2d.utils

import java.io.File

import io.enoble.svg2d.parsing.Parse

import scalaz._
import Scalaz._

object PathStats {
  def main(args: Array[String]): Unit = {
    val svgFolder = new File("svg")
    val xml = svgFolder.listFiles().toVector.map(scala.xml.XML.loadFile)
    val nodes = xml.map(Parse.pathStats).foldMap(identity)
    val unique = nodes.toList.sortBy(-_._2)
    println("Path stats:")
    unique.foreach(println)
  }

}

