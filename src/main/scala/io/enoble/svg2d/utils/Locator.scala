package io
package enoble
package svg2d
package utils

import java.io.File
import cats.implicits._

import io.enoble.svg2d.xmlparse.Parse

object Locator {
  def main(args: Array[String]): Unit = {
    val svgFolder = new File("svg")
    val xml = svgFolder.listFiles().map(scala.xml.XML.loadFile)
    val nodes = xml.map(Parse.elementStats).foldLeft(Map[String, Int]())(_ |+| _)
    val unique = nodes.toList.sortBy(-_._2)
    println("Element stats:")
    unique.foreach(println)
    val attrs = xml.map(Parse.attributeStats).foldLeft(Map[String, Int]())(_ |+| _)
    val sorted = attrs.toList.sortBy(-_._2)
    println("Attribute stats:")
    sorted.foreach(println)
  }

}
