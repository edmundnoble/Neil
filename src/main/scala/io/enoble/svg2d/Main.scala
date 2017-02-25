package io
package enoble
package svg2d

import java.io.File

import cats.Monoid
import cats.implicits._
import io.enoble.svg2d.ast.{FastMonoid, FinalSVG, InitialSVG}
import io.enoble.svg2d.render._
import io.enoble.svg2d.utils.TCPairC
import io.enoble.svg2d.xmlparse.Parse
import scopt.Read

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
      c.copy(inputFolder = x)
    } text "input folder of svg's"
    opt[File]('o', "output") optional() valueName "<file>" action { (x, c) =>
      c.copy(outputFolder = Some(x))
    } text "output code generation folder (or none, for stdout)"
    help("help") text "prints this usage text"
  }

  def parseAndRenderOutput[A](renderer: FinalSVG[A], svgContent: xml.Elem): Option[Option[A]] = {
    Parse.parseAll(renderer)(svgContent)
  }

  def main(args: Array[String]): Unit = {
    main(args, identity)
  }

  def main(args: Array[String], transformer: FastMonoid[String, Vector[() => Unit]] => FastMonoid[String, Vector[() => Unit]]): Unit = {
    val configParsed: Option[MainConfig] =
      parser.parse(args, MainConfig())
    configParsed.foreach(runApp(_, transformer))
  }

  def runApp(config: MainConfig, transformer: FastMonoid[String, Vector[() => Unit]] => FastMonoid[String, Vector[() => Unit]]) = {
    val filePath = config.inputFolder
    val stringyOutputMonoid = transformer(PrintRenderer(System.out, FastMonoid.Id[String]))
    val renderer: FinalSVG[Vector[() => Unit]] = config.outputType match {
      case Swift => SwiftRenderer(stringyOutputMonoid)
      case ObjectiveC => ObjectiveCRenderer(stringyOutputMonoid)
      case Android => AndroidRenderer(stringyOutputMonoid)
      case Raw => InitialRenderer(PrintRenderer(System.out, FastMonoid.Vec[InitialSVG]()))
    }
    runWithRenderer(filePath, renderer)
  }

  def runWithRenderer(filePath: File, renderer: FinalSVG[Vector[() => Unit]]): Unit = {
    val isDir = filePath.isDirectory
    if (isDir) {
      val svgFiles = filePath.listFiles()
      val xmlFiles = svgFiles.iterator.map(f => xml.XML.loadFile(f))
      val parsed: List[Option[Option[Vector[() => Unit]]]] =
        xmlFiles.map(Parse.parseAll(renderer)).toList
      val (successes, failures) = parsed.partition(_.isDefined)
      val successCount = successes.length
      val failureCount = failures.length
      val successRate = (successCount * 100) / (successCount + failureCount).toDouble
      println(s"Successes: $successCount")
      println(s"Failures: $failureCount")
      println(f"Success rate: $successRate%2.2f%%")
      val failedFiles = parsed.zipWithIndex.collect { case (Some(_), d) => svgFiles(d).getName }
      println(s"Failed files: \n${failedFiles.mkString("\n")}")
      println(parsed)
    } else {
      val xml = scala.xml.XML.loadFile(filePath)
      val parsed = parseAndRenderOutput(renderer, xml)
      parsed.map(_.getOrElse(Vector.empty).foreach(_.apply())).getOrElse {
        println("Parsing failed!")
      }
    }
  }
}

