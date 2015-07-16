package io.enoble.svg2d.utils

import java.io.File

import io.enoble.svg2d.parsing.Parse

import scalaz.Scalaz._
import scalaz._

object PathStats {
  def main(args: Array[String]): Unit = {
    val svgFolder = new File("svg")
    val xml = svgFolder.listFiles().map(scala.xml.XML.loadFile)
    val nodes = xml.map(Parse.pathStats).foldLeft(Map[Char, Int]())(_ |+| _)
    val unique = nodes.toList.sortBy(-_._2)
    println("Path stats:")
    unique.foreach(println)
  }

}

