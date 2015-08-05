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
    val filePath = Paths.get(args(0))
    val isDir = Files.isDirectory(filePath)
    if (isDir) {
      val svgFiles = Files.list(filePath).iterator().asScala
      val xmlFiles = svgFiles.map(f => xml.XML.loadFile(f.toFile))
      val parsed = xmlFiles.map(Parse.parseAll)
      val (successes, failures) = parsed.partition(_.isDefined)
      val successCount = successes.length
      val failureCount = failures.length
      println(s"Successes: $successCount")
      println(s"Failures: $failureCount")
      val df = new DecimalFormat("00.00")
      println(s"Success rate: ${df.format(successCount * 100 / (successCount + failureCount).toDouble)}%")
    } else {
      val xml = scala.xml.XML.loadFile(filePath.toFile)
      val parsed = Parse.parseAll(xml)
      println(parsed)
    }
  }
}

