package io
package enoble
package svg2d

import java.io.File
import java.util.concurrent.Executors

import cats.Monoid
import cats.implicits._
import io.enoble.svg2d.ast.{FastMonoid, FinalSVG, InitialSVG}
import io.enoble.svg2d.render._
import io.enoble.svg2d.utils.TCPairC
import io.enoble.svg2d.xmlparse.Parse
import monix.eval.Task
import monix.execution.Scheduler
import monix.cats._
import scopt.Read

import scala.concurrent.Await
import scala.concurrent.duration.Duration

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
    } text "input svg file/folder of svg's"
    opt[File]('o', "output") optional() valueName "<file>" action { (x, c) =>
      c.copy(outputFolder = Some(x))
    } text "output code generation folder/file (or none, for stdout)"
    help("help") text "prints this usage text"
  }

  // a) separate outputs for each output type

  def parseAndRenderOutput[A](renderer: FinalSVG[A], svgContent: xml.Elem): Option[Option[A]] = {
    Parse.parseAll(renderer)(svgContent)
  }

  def main(args: Array[String]): Unit = {
    main(args, identity)
  }

  def main(args: Array[String], transformer: FastMonoid[String, Vector[String]] => FastMonoid[String, Vector[String]]): Unit = {
    val configParsed: Option[MainConfig] =
      parser.parse(args, MainConfig())
    if (configParsed.isEmpty) sys.exit(1)
    implicit val workThreadPoolScheduler =
      Scheduler(Executors.newFixedThreadPool(2))
    val future =
        runApp(configParsed.get, transformer).runAsync(Scheduler.global)
    Await.result(future, Duration.Inf)
    workThreadPoolScheduler.shutdown()
  }

  def runApp(config: MainConfig,
             transformer: FastMonoid[String, Vector[String]] => FastMonoid[String, Vector[String]])(implicit workThreadPoolScheduler: Scheduler): Task[Unit] = {
    val filePath = config.inputFolder
    val stringyOutputMonoid: FastMonoid[String, Vector[String]] =
      transformer(FastMonoid.Vec[String])
    val renderer: FinalSVG[Vector[String]] = config.outputType match {
      case Swift => SwiftRenderer(stringyOutputMonoid)
      case ObjectiveC => ObjectiveCRenderer(stringyOutputMonoid)
      case Android => AndroidRenderer(stringyOutputMonoid)
      case Raw => InitialRenderer(FastMonoid.ToString[InitialSVG]())
    }
    runWithRenderer(filePath, renderer)
  }

  def runWithRenderer(filePath: File, renderer: FinalSVG[Vector[String]])(implicit sch: Scheduler): Task[Unit] =
    Task.defer {
      val isDir = filePath.isDirectory
      if (isDir) {
        val svgFiles = filePath.listFiles().toVector
        for {
          parsed <-
          Task.wander(svgFiles)(f => Task.fork(Task.eval(Parse.parseAll(renderer)(xml.XML.loadFile(f))), sch))
          // ultimate effect terminator
          _ <- Task.eval(parsed.foreach(_.foreach(_.foreach(_.foreach(print)))))
          _ <- Task.eval {
            val (successes, failures) = parsed.partition(_.isDefined)
            val successCount = successes.length
            val failureCount = failures.length
            val successRate = (successCount * 100) / (successCount + failureCount).toDouble
            if (failureCount > 0) {
              System.err.println(s"Successes: $successCount")
              System.err.println(s"Failures: $failureCount")
              System.err.println(f"Success rate: $successRate%2.2f%%")
            }
            val failedFiles = parsed.zipWithIndex.collect { case (Some(_), d) => svgFiles(d).getName }

            if (failureCount > 0) {
              def truncate(l: Int)(s: String) = if (s.length < l) s else s.substring(0, l) + "..."

              System.err.println(s"Failed files: \n${truncate(100)(failedFiles.mkString("\n"))}")
            }
          }
        } yield ()
      } else Task.eval {
        val xml = scala.xml.XML.loadFile(filePath)
        val parsed = parseAndRenderOutput(renderer, xml)
        parsed.map(_.getOrElse(Vector.empty).foreach(print)).getOrElse {
          println("Parsing failed!")
        }
      }
    }
}

