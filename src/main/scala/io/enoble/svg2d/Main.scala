package io.enoble.svg2d

import java.io.File
import java.nio.file.{Files, Paths}
import java.text.DecimalFormat

import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.data.{AndroidCode, ObjectiveCCode, Renderable, SwiftCode}
import io.enoble.svg2d.xmlparse.Parse
import io.enoble.svg2d.render._
import scopt.Read

import scalaz._
import Scalaz._
import scala.io.Source
import scala.collection.JavaConverters._

object Main {
  sealed trait OutputType
  case object Swift extends OutputType
  case object ObjectiveC extends OutputType
  case object Android extends OutputType
  case object Raw extends OutputType

  val outputTypeMapping: Map[String, Main.OutputType] = List(
    List("s", "swift") -> Swift,
    List("c", "objc", "objectivec") -> ObjectiveC,
    List("a", "android", "j", "java") -> Android,
    List("r", "raw") -> Raw
  ).flatMap(p => p._1.map(_ -> p._2))(collection.breakOut)

  def parseOutputType(str: String): OutputType =
    outputTypeMapping.getOrElse(str.toLowerCase(), throw new IllegalArgumentException(s"$str is not a valid output type: try 's', 'c', 'a', or 'r'"))

  case class MainConfig(outputType: OutputType = null, inputFolder: File = null, outputFolder: Option[File] = None)

  implicit val outputTypeInstances: Read[OutputType] = Read.reads(parseOutputType)

  val parser = new scopt.OptionParser[MainConfig]("neil") {
      head("neil", "0.0.2")
      opt[OutputType]('t', "otype") required() valueName "<outputType>" action { (x, c) =>
        c.copy(outputType = x)
      } text "output type; valid output types are 's' (Swift), 'c' (Objective C), 'a' (Android), and 'r' (Raw)"
      opt[File]('i', "input") required() valueName "<file>" action { (x, c) =>
        c.copy(inputFolder = x) } text "input folder of svg's"
      opt[File]('o', "output") optional() valueName "<file>" action { (x, c) =>
        c.copy(outputFolder = Some(x)) } text "output code generation folder (or none, for stdout)"
      help("help") text "prints this usage text"
  }

  def main(args: Array[String]): Unit = {
    val configParsed: Option[MainConfig] =
      parser.parse(args, MainConfig())
    configParsed.foreach { config =>
      val filePath = config.inputFolder
      val renderer: FinalSVG[Renderable] = (config.outputType match {
        case Swift => SwiftRenderer
        case ObjectiveC => ObjectiveCRenderer
        case Android => AndroidRenderer
        case Raw => InitialRenderer
      }).asInstanceOf[FinalSVG[Renderable]]
      val isDir = filePath.isDirectory
      if (isDir) {
        val svgFiles = filePath.listFiles().view
        val xmlFiles = svgFiles.map(f => xml.XML.loadFile(f))
        val parsed = xmlFiles.map(Parse.parseAll(renderer))
        val (successes, failures) = parsed.partition(_.isDefined)
        val successCount = successes.length
        val failureCount = failures.length
        val successRate = (successCount * 100) / (successCount + failureCount).toDouble
        println(s"Successes: $successCount")
        println(s"Failures: $failureCount")
        println(f"Success rate: $successRate%2.2f%%")
        val failedFiles = parsed.zipWithIndex.map(x => (x._1, svgFiles(x._2))).filter(_._1.isEmpty).map(_._2.getName)
        println(s"Failed files: \n${failedFiles.mkString("\n")}")
        val stringVector = parsed.map(_.map(_.map(_.asString))).toVector
        println(stringVector)
      } else {
        val xml = scala.xml.XML.loadFile(filePath)
        val parsed = Parse.parseAll(renderer)(xml)
        println(parsed.fold {
          "Parsing failed!"
        } { codes =>
          codes.map(_.asString).toString
        })
      }
    }
  }
}

