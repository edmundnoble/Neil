package io
package enoble
package svg2d

import java.io.{BufferedWriter, File, FileOutputStream, FilenameFilter, OutputStreamWriter, PrintStream}
import java.nio.ByteBuffer
import java.nio.file.{OpenOption, Path, Paths, StandardOpenOption}
import java.util.concurrent.Executors

import cats.implicits._
import io.enoble.svg2d.ast._
import io.enoble.svg2d.render._
import io.enoble.svg2d.xmlparse.{Parse, XMLConsumer}
import monix.eval.Task
import monix.execution.Scheduler
import caseapp._
import caseapp.core.{ArgParser, Messages, WithHelp}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.xml.MetaData
import Main._

@AppName("Neil")
@AppVersion("0.1.0")
@ProgName("neil")
final case class MainConfig(debug: Boolean = false,
                            timed: Boolean = false,
                            @ExtraName("i")
                            input: ExistingFile,
                            @ExtraName("a")
                            android: Option[File],
                            @ExtraName("s")
                            swift: Option[File],
                            @ExtraName("o")
                            objc: Option[File]) {

  def main(transformer: FastMonoid[String, Vector[String]] => FastMonoid[String, Vector[String]]): Unit = {
    implicit val workThreadPoolScheduler =
      Scheduler(Executors.newFixedThreadPool(2))
    val time = System.nanoTime
    val future =
      runApp(transformer)(workThreadPoolScheduler).runAsync(Scheduler.global).map(_ => workThreadPoolScheduler.shutdown())
    Await.result(future, Duration.Inf)
    val timeAfter = System.nanoTime
    if (timed)
      println(s"Time taken to generate code: ${(timeAfter - time) / 1000000.0} milliseconds")
  }

  def runApp(transformer: FastMonoid[String, Vector[String]] => FastMonoid[String, Vector[String]])(implicit workThreadPoolScheduler: Scheduler): Task[Unit] = Task.defer {
    val stringyOutputMonoid: FastMonoid[String, Vector[String]] =
      transformer(FastMonoid.Vec[String]())

    val swiftRenderer = SwiftRenderer(stringyOutputMonoid)
    val objcRenderer = ObjectiveCRenderer(stringyOutputMonoid)
    val androidRenderer = AndroidRenderer(stringyOutputMonoid)

    if (input.file.isDirectory) {
      android.foreach(_.mkdirs())
      swift.foreach(_.mkdirs())
      objc.foreach(_.mkdirs())
    } else {
      android.foreach(_.createNewFile())
      swift.foreach(_.createNewFile())
      objc.foreach(_.createNewFile())
    }
    assert(android.forall(_.isDirectory == input.file.isDirectory) &&
      swift.forall(_.isDirectory == input.file.isDirectory) &&
      objc.forall(_.isDirectory == input.file.isDirectory))
    if (input.file.isDirectory) {
      val svgInputFiles = input.file.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".svg")
      })
      val androidOutputFiles = svgInputFiles.map(n => android.map(f => new File(f, n.getName)))
      val swiftOutputFiles = svgInputFiles.map(n => swift.map(f => new File(f, n.getName)))
      val objcOutputFiles = svgInputFiles.map(n => objc.map(f => new File(f, n.getName)))
      Task.gatherUnordered(
        svgInputFiles.zipWithIndex.map {
          case (i, idx) =>
            runWithRenderer(i,
              androidRenderer, Surrounders.android, androidOutputFiles(idx),
              swiftRenderer, Surrounders.swift, swiftOutputFiles(idx),
              objcRenderer, Surrounders.objc, objcOutputFiles(idx))
        }
      ).map(_ => ())
    } else {
      runWithRenderer(input.file,
        androidRenderer, Surrounders.android, android,
        swiftRenderer, Surrounders.swift, swift,
        objcRenderer, Surrounders.objc, objc)
    }
  }

  def run[A](file: File, renderer: FinalSVG[A])(implicit sch: Scheduler): Task[A] =
    Task.fork(Task.eval {
      val time = System.nanoTime
      val parser = new xmlparse.XmlParser[A](Source.fromFile(file), XMLConsumerForRenderers(renderer))
      parser.run()
      val timeAfter = System.nanoTime
      if (timed) {
        println(s"accumulating input from file ${file.getName} took ${(timeAfter - time) / 1000000.0} milliseconds")
      }
      parser.getState
    }, sch)

  def runWithRenderer[AP, SP, OP](inputFile: File,
                                  andy: FinalSVG[Vector[String]] {type Paths = AP}, andySurrounders: Surrounders, androidOutput: Option[File],
                                  swifty: FinalSVG[Vector[String]] {type Paths = SP}, swiftySurrounders: Surrounders, swiftOutput: Option[File],
                                  objcy: FinalSVG[Vector[String]] {type Paths = OP}, objcySurrounders: Surrounders, objcOutput: Option[File])(implicit sch: Scheduler): Task[Unit] = Task.defer {
    val androidWriter: Option[FileOutputStream] =
      androidOutput.map(f => new FileOutputStream(f))
    val swiftWriter: Option[FileOutputStream] =
      swiftOutput.map(f => new FileOutputStream(f))
    val objcWriter: Option[FileOutputStream] =
      objcOutput.map(f => new FileOutputStream(f))

    val renderer = new FinalSVG[Vector[() => Unit]] {
      override type Paths = (Option[AP], Option[SP], Option[OP], Option[Vector[String]])
      override val path: FinalPath[Paths] = new FinalPath[Paths] {
        override val empty: Paths =
          (androidWriter.as(andy.path.empty),
            swiftWriter.as(swifty.path.empty),
            objcWriter.as(objcy.path.empty),
            if (debug) Some(Vector.empty) else None)

        override def append(fst: Paths, snd: Paths): Paths =
          (fst._1.map2(snd._1)(andy.path.append),
            fst._2.map2(snd._2)(swifty.path.append),
            fst._3.map2(snd._3)(objcy.path.append),
            if (debug) (fst._4 |@| snd._4).map(_ ++ _) else None)

        override def closePath(): Paths =
          (Some(andy.path.closePath()),
            Some(swifty.path.closePath()),
            Some(objcy.path.closePath()),
            if (debug) Some(Vector("ClosePath")) else None)

        override def moveTo(x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.moveTo(x, y)),
            swiftWriter.as(swifty.path.moveTo(x, y)),
            objcWriter.as(objcy.path.moveTo(x, y)),
            if (debug) Some(Vector(s"MoveTo($x, $y)")) else None)

        override def moveToRel(dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.moveToRel(dx, dy)),
            swiftWriter.as(swifty.path.moveToRel(dx, dy)),
            objcWriter.as(objcy.path.moveToRel(dx, dy)),
            if (debug) Some(Vector(s"MoveToRel($dx, $dy)")) else None)

        override def lineTo(x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.lineTo(x, y)),
            swiftWriter.as(swifty.path.lineTo(x, y)),
            objcWriter.as(objcy.path.lineTo(x, y)),
            if (debug) Some(Vector(s"LineTo($x, $y)")) else None)

        override def lineToRel(dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.lineToRel(dx, dy)),
            swiftWriter.as(swifty.path.lineToRel(dx, dy)),
            objcWriter.as(objcy.path.lineToRel(dx, dy)),
            if (debug) Some(Vector(s"LineToRel($dx, $dy)")) else None)

        override def verticalLineTo(y: Double): Paths =
          (androidWriter.as(andy.path.verticalLineTo(y)),
            swiftWriter.as(swifty.path.verticalLineTo(y)),
            objcWriter.as(objcy.path.verticalLineTo(y)),
            if (debug) Some(Vector(s"LineTo($y)")) else None)

        override def verticalLineToRel(dy: Double): Paths =
          (androidWriter.as(andy.path.verticalLineToRel(dy)),
            swiftWriter.as(swifty.path.verticalLineToRel(dy)),
            objcWriter.as(objcy.path.verticalLineToRel(dy)),
            if (debug) Some(Vector(s"LineToRel($dy)")) else None)

        override def horizLineTo(x: Double): Paths =
          (androidWriter.as(andy.path.horizLineTo(x)),
            swiftWriter.as(swifty.path.horizLineTo(x)),
            objcWriter.as(objcy.path.horizLineTo(x)),
            if (debug) Some(Vector(s"HorizLineTo($x)")) else None)

        override def horizLineToRel(dx: Double): Paths =
          (androidWriter.as(andy.path.horizLineToRel(dx)),
            swiftWriter.as(swifty.path.horizLineToRel(dx)),
            objcWriter.as(objcy.path.horizLineToRel(dx)),
            if (debug) Some(Vector(s"HorizLineToRel($dx)")) else None)

        override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.cubic(x1, y1, x2, y2, x, y)),
            swiftWriter.as(swifty.path.cubic(x1, y1, x2, y2, x, y)),
            objcWriter.as(objcy.path.cubic(x1, y1, x2, y2, x, y)),
            if (debug) Some(Vector(s"Cubic($x1, $y1, $x2, $y2, $x, $y)")) else None)

        override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.cubicRel(x1, y1, x2, y2, dx, dy)),
            swiftWriter.as(swifty.path.cubicRel(x1, y1, x2, y2, dx, dy)),
            objcWriter.as(objcy.path.cubicRel(x1, y1, x2, y2, dx, dy)),
            if (debug) Some(Vector(s"CubicRel($x1, $y1, $x2, $y2, $dx, $dy)")) else None)

        override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.smoothCubic(x2, y2, x, y)),
            swiftWriter.as(swifty.path.smoothCubic(x2, y2, x, y)),
            objcWriter.as(objcy.path.smoothCubic(x2, y2, x, y)),
            if (debug) Some(Vector(s"SmoothCubic($x2, $y2, $x, $y)")) else None)

        override def smoothQuad(x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.smoothQuad(x, y)),
            swiftWriter.as(swifty.path.smoothQuad(x, y)),
            objcWriter.as(objcy.path.smoothQuad(x, y)),
            if (debug) Some(Vector(s"SmoothQuad($x, $y)")) else None)

        override def smoothQuadRel(dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.smoothQuadRel(dx, dy)),
            swiftWriter.as(swifty.path.smoothQuadRel(dx, dy)),
            objcWriter.as(objcy.path.smoothQuadRel(dx, dy)),
            if (debug) Some(Vector(s"SmoothQuadRel($dx, $dy)")) else None)

        override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)),
            swiftWriter.as(swifty.path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)),
            objcWriter.as(objcy.path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)),
            if (debug) Some(Vector(s"Elliptic($rx, $ry, $rotX, $largeArc, $sweep, $x, $y)")) else None)

        override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.smoothCubicRel(x2, y2, dx, dy)),
            swiftWriter.as(swifty.path.smoothCubicRel(x2, y2, dx, dy)),
            objcWriter.as(objcy.path.smoothCubicRel(x2, y2, dx, dy)),
            if (debug) Some(Vector(s"SmoothCubicRel($x2, $y2, $dx, $dy)")) else None)

        override def quad(x1: Double, y1: Double, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.quad(x1, y1, x, y)),
            swiftWriter.as(swifty.path.quad(x1, y1, x, y)),
            objcWriter.as(objcy.path.quad(x1, y1, x, y)),
            if (debug) Some(Vector(s"Quad($x1, $y1, $x, $y)")) else None)

        override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.quadRel(x1, y1, dx, dy)),
            swiftWriter.as(swifty.path.quadRel(x1, y1, dx, dy)),
            objcWriter.as(objcy.path.quadRel(x1, y1, dx, dy)),
            if (debug) Some(Vector(s"QuadRel($x1, $y1, $dx, $dy)")) else None)

        override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)),
            swiftWriter.as(swifty.path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)),
            objcWriter.as(objcy.path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)),
            if (debug) Some(Vector(s"EllipticRel($rx, $ry, $rotX, $largeArc, $sweep, $dx, $dy)")) else None)
      }

      override val empty: scala.Vector[() => Unit] = Vector.empty

      override def append(fst: scala.Vector[() => Unit], snd: scala.Vector[() => Unit]): scala.Vector[() => Unit] =
        fst ++ snd

      override def circle(x: Double, y: Double, r: Double): scala.Vector[() => Unit] = Vector({ () =>
        androidWriter.foreach(writer => andy.circle(x, y, r).foreach(writer.writeStr))
        objcWriter.foreach(writer => objcy.circle(x, y, r).foreach(writer.writeStr))
        swiftWriter.foreach(writer => swifty.circle(x, y, r).foreach(writer.writeStr))
        if (debug) println(s"circle($x, $y, $r)")
      })

      override def ellipse(x: Double, y: Double, rx: Double, ry: Double): scala.Vector[() => Unit] = Vector({ () =>
        androidWriter.foreach(writer => andy.ellipse(x, y, rx, ry).foreach(writer.writeStr))
        objcWriter.foreach(writer => objcy.ellipse(x, y, rx, ry).foreach(writer.writeStr))
        swiftWriter.foreach(writer => swifty.ellipse(x, y, rx, ry).foreach(writer.writeStr))
        if (debug) println(s"ellipse($x, $y, $rx, $ry)")
      })

      override def rect(x: Double, y: Double, rx: Double, ry: Double): scala.Vector[() => Unit] = Vector({ () =>
        androidWriter.foreach(writer => andy.rect(x, y, rx, ry).foreach(writer.writeStr))
        objcWriter.foreach(writer => objcy.rect(x, y, rx, ry).foreach(writer.writeStr))
        swiftWriter.foreach(writer => swifty.rect(x, y, rx, ry).foreach(writer.writeStr))
        if (debug) println(s"rect($x, $y, $rx, $ry)")
      })

      override def text(text: String, x: Double, y: Double): scala.Vector[() => Unit] = Vector({ () =>
        androidWriter.foreach(writer => andy.text(text, x, y).foreach(writer.writeStr))
        swiftWriter.foreach(writer => andy.text(text, x, y).foreach(writer.writeStr))
        objcWriter.foreach(writer => andy.text(text, x, y).foreach(writer.writeStr))
        if (debug) println(s"text($text, $x, $y)")
      })

      override def includePath(paths: Paths): scala.Vector[() => Unit] = Vector({ () =>
        androidWriter.foreach(writer => andy.includePath(paths._1.get).foreach(writer.writeStr))
        swiftWriter.foreach(writer => swifty.includePath(paths._2.get).foreach(writer.writeStr))
        objcWriter.foreach(writer => objcy.includePath(paths._3.get).foreach(writer.writeStr))
        if (debug) paths._4.foreach(_.foreach(System.out.println))
      })
    }

    androidWriter.foreach(_.writeStr(andySurrounders.prefixFromName(inputFile.getName)))
    swiftWriter.foreach(_.writeStr(swiftySurrounders.prefixFromName(inputFile.getName)))
    objcWriter.foreach(_.writeStr(objcySurrounders.prefixFromName(inputFile.getName)))
    run(inputFile, renderer).map(_.foreach(_ ())).map { _ =>
      androidWriter.foreach(_.writeStr(andySurrounders.suffix))
      swiftWriter.foreach(_.writeStr(swiftySurrounders.suffix))
      objcWriter.foreach(_.writeStr(objcySurrounders.suffix))
      androidWriter.foreach(_.close())
      swiftWriter.foreach(_.close())
      objcWriter.foreach(_.close())
    }

  }

}

object Main {

  final case class XMLConsumerForRenderers[A](finalSVG: FinalSVG[A]) extends XMLConsumer[A] {
    // here add styles
    override def elemStart(label: String, attrs: MetaData): A =
      Parse.parsers.get(label).flatMap(_.apply(attrs, finalSVG)).getOrElse(finalSVG.empty)

    // here remove styles
    override def elemEnd(label: String): A = finalSVG.empty

    // TODO: handle <tspan>
    override def text(label: String, attrs: MetaData, text: String): A =
      Parse.terminalParsers.get(label).flatMap(_.apply(attrs, text, finalSVG)).getOrElse(finalSVG.empty)

    override def entityRef(entity: String): A = finalSVG.empty

    override val start: A = finalSVG.empty

    override def progress(fst: A, snd: A): A = finalSVG.append(fst, snd)
  }

  implicit val fileArgParser: ArgParser[File] =
    ArgParser.instance[File]("file") { s =>
      Right(new File(s))
    }

  implicit val existingFileArgParser: ArgParser[ExistingFile] =
    ArgParser.instance[ExistingFile]("existingFile") { s =>
      val file = new File(s)
      if (file.exists()) {
        Right(ExistingFile(file))
      } else {
        Left(s"file $s does not appear to exist")
      }
    }

  final case class ExistingFile(file: File) extends AnyVal

  def main(args: Array[String]): Unit = {
    Parser[MainConfig].withHelp.detailedParse(args) match {
      case Left(err) =>
        Console.err.println(err)
        sys.exit(1)

      case Right((WithHelp(usage, help, t), _, _)) =>
        if (help) {
          println(Messages[MainConfig].withHelp.helpMessage)
          sys.exit(0)
        }

        if (usage) {
          println(Messages[MainConfig].withHelp.usageMessage)
          sys.exit(0)
        }
        t.main(identity)
    }
  }

  implicit final class fileOutputStreamWriteStrOp(val stream: FileOutputStream) extends AnyVal {
    def writeStr(str: String) = stream.write(str.getBytes())
  }

}

