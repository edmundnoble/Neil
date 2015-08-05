package io.enoble.svg2d

import java.io.File
import java.nio.file.{Files, Paths}
import java.text.DecimalFormat

import io.enoble.svg2d.parsing.Parse

import scalaz._
import Scalaz._
import scala.io.Source

import scala.collection.JavaConverters._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      Console.err.println("Wrong number of arguments!")
      sys.exit(1)
    }
    val filePath = new File(args(0))
    val isDir = filePath.isDirectory
    if (isDir) {
      val svgFiles = filePath.listFiles()
      val xmlFiles = svgFiles.map(f => xml.XML.loadFile(f))
      val parsed = xmlFiles.map(Parse.parseAll)
      val (successes, failures) = parsed.partition(_.isDefined)
      val successCount = successes.length
      val failureCount = failures.length
      val successRate = (successCount * 100) / (successCount + failureCount).toDouble
      println(s"Successes: $successCount")
      println(s"Failures: $failureCount")
      println(f"Success rate: $successRate%2.2f%%")
    } else {
      val xml = scala.xml.XML.loadFile(filePath)
      val parsed = Parse.parseAll(xml)
      println(parsed)
    }
  }
}

