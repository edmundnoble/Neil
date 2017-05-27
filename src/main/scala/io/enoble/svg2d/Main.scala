package io
package enoble
package svg2d

import java.io.{File, FileOutputStream, FilenameFilter}
import scala.{Stream => _}

import io.enoble.svg2d.xmlparse.XMLConsumer
import caseapp._
import caseapp.core.{ArgParser, Messages, WithHelp}
import cats.implicits._
import io.enoble.svg2d.Main._
import io.enoble.svg2d.ast._
import io.enoble.svg2d.render._
import monix.eval.Coeval

import scala.io.Source
import scala.xml.MetaData

sealed trait Timed
final case class TimeOverall(iterations: Int) extends Timed
case object TimeEachFile extends Timed
final case class TimeFirst(files: Int) extends Timed

@AppName("Neil")
@AppVersion("0.1.0")
@ProgName("neil")
final case class MainConfig(debug: Boolean = false,
                            timed: Option[Timed] = None,
                            @ExtraName("i")
                            input: ExistingFile,
                            @ExtraName("a")
                            android: Option[File],
                            @ExtraName("s")
                            swift: Option[File],
                            @ExtraName("o")
                            objc: Option[File]) {

  def main(transformer: FastMonoid[String, Steque[String]] => FastMonoid[String, Steque[String]]): Unit = {
    val time = System.nanoTime
    runApp(transformer).apply()
    val timeAfter = System.nanoTime
    timed.foreach { _ =>
      println(s"Time taken to generate code: ${(timeAfter - time) / 1000000.0} milliseconds")
    }
  }

  def runApp(transformer: FastMonoid[String, Steque[String]] => FastMonoid[String, Steque[String]]): Coeval[Unit] =
    Coeval.defer {
      val stringyOutputMonoid: FastMonoid[String, Steque[String]] =
        transformer(FastMonoid.Tq[String]())

      val swiftRenderer = SwiftRenderer(stringyOutputMonoid)
      val objcRenderer = ObjectiveCRenderer(stringyOutputMonoid)
      val androidRenderer = AndroidRenderer()(stringyOutputMonoid)

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
        }).toList
        val androidOutputFiles = svgInputFiles.map(n => android.map(f => new File(f, n.getName)))
        val swiftOutputFiles = svgInputFiles.map(n => swift.map(f => new File(f, n.getName)))
        val objcOutputFiles = svgInputFiles.map(n => objc.map(f => new File(f, n.getName)))
        Coeval.sequence(
          svgInputFiles.zipWithIndex.map { case (inputFile, idx) =>
            val timeFile = timed.exists {
              case TimeEachFile => true
              case TimeFirst(files) => idx < files
              case TimeOverall(iters) => false
            }
            runWithRenderer(
              inputFile,
              timeFile,
              androidRenderer, Surrounders.android, androidOutputFiles(idx),
              swiftRenderer, Surrounders.swift, swiftOutputFiles(idx),
              objcRenderer, Surrounders.objc, objcOutputFiles(idx)
            )
          }
        ).map(_ => ())
      } else {
        val timeFile = timed.exists {
          case TimeEachFile => true
          case TimeFirst(files) => files > 0
          case TimeOverall(_) => true
        }
        runWithRenderer(input.file, timeFile,
          androidRenderer, Surrounders.android, android,
          swiftRenderer, Surrounders.swift, swift,
          objcRenderer, Surrounders.objc, objc)
      }
    }

  def run[A](file: File, time: Boolean, renderer: FinalSVG[A]): Coeval[A] =
    Coeval.eval {
      val timeBefore = System.nanoTime
      val parser = new xmlparse.XmlParser[A](Source.fromFile(file), XMLConsumerForRenderers(renderer))
      parser.run()
      val timeAfter = System.nanoTime
      if (time) {
        println(s"accumulating input from file ${file.getName} took ${(timeAfter - timeBefore) / 1000000.0} milliseconds")
      }
      parser.getState
    }

  def runWithRenderer[AP, SP, OP](inputFile: File,
                                  time: Boolean,
                                  andy: FinalSVG[Steque[String]] {type Paths = AP}, andySurrounders: Surrounders, androidOutput: Option[File],
                                  swifty: FinalSVG[Steque[String]] {type Paths = SP}, swiftySurrounders: Surrounders, swiftOutput: Option[File],
                                  objcy: FinalSVG[Steque[String]] {type Paths = OP}, objcySurrounders: Surrounders, objcOutput: Option[File]): Coeval[Unit] = {
    val androidWriter: Option[FileOutputStream] = androidOutput.map(new FileOutputStream(_))
    val swiftWriter: Option[FileOutputStream] = swiftOutput.map(new FileOutputStream(_))
    val objcWriter: Option[FileOutputStream] = objcOutput.map(new FileOutputStream(_))

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      if (a.isEmpty || b.isEmpty) None
      else Some(f(a.get, b.get))
    }

    val renderer = new FinalSVG[Steque[() => Unit]] {
      override type Paths = (Option[AP], Option[SP], Option[OP], Option[Steque[String]])
      override val path: FinalPath[Paths] = new FinalPath[Paths] {
        override val empty: Paths =
          (androidWriter.map(_ => andy.path.empty),
            swiftWriter.map(_ => swifty.path.empty),
            objcWriter.map(_ => objcy.path.empty),
            if (debug) Some(Steque.empty) else None)

        override def append(fst: Paths, snd: Paths): Paths =
          (map2(fst._1, snd._1)(andy.path.append),
            map2(fst._2, snd._2)(swifty.path.append),
            map2(fst._3, snd._3)(objcy.path.append),
            if (debug) map2(fst._4, snd._4)(_ ++: _) else None)

        override val closePath: Paths =
          (Some(andy.path.closePath),
            Some(swifty.path.closePath),
            Some(objcy.path.closePath),
            if (debug) Some(Steque.single("ClosePath")) else None)

        override def moveTo(x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.moveTo(x, y)),
            swiftWriter.as(swifty.path.moveTo(x, y)),
            objcWriter.as(objcy.path.moveTo(x, y)),
            if (debug) Some(Steque.single(s"MoveTo($x, $y)")) else None)

        override def moveToRel(dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.moveToRel(dx, dy)),
            swiftWriter.as(swifty.path.moveToRel(dx, dy)),
            objcWriter.as(objcy.path.moveToRel(dx, dy)),
            if (debug) Some(Steque.single(s"MoveToRel($dx, $dy)")) else None)

        override def lineTo(x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.lineTo(x, y)),
            swiftWriter.as(swifty.path.lineTo(x, y)),
            objcWriter.as(objcy.path.lineTo(x, y)),
            if (debug) Some(Steque.single(s"LineTo($x, $y)")) else None)

        override def lineToRel(dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.lineToRel(dx, dy)),
            swiftWriter.as(swifty.path.lineToRel(dx, dy)),
            objcWriter.as(objcy.path.lineToRel(dx, dy)),
            if (debug) Some(Steque.single(s"LineToRel($dx, $dy)")) else None)

        override def verticalLineTo(y: Double): Paths =
          (androidWriter.as(andy.path.verticalLineTo(y)),
            swiftWriter.as(swifty.path.verticalLineTo(y)),
            objcWriter.as(objcy.path.verticalLineTo(y)),
            if (debug) Some(Steque.single(s"LineTo($y)")) else None)

        override def verticalLineToRel(dy: Double): Paths =
          (androidWriter.as(andy.path.verticalLineToRel(dy)),
            swiftWriter.as(swifty.path.verticalLineToRel(dy)),
            objcWriter.as(objcy.path.verticalLineToRel(dy)),
            if (debug) Some(Steque.single(s"LineToRel($dy)")) else None)

        override def horizLineTo(x: Double): Paths =
          (androidWriter.as(andy.path.horizLineTo(x)),
            swiftWriter.as(swifty.path.horizLineTo(x)),
            objcWriter.as(objcy.path.horizLineTo(x)),
            if (debug) Some(Steque.single(s"HorizLineTo($x)")) else None)

        override def horizLineToRel(dx: Double): Paths =
          (androidWriter.as(andy.path.horizLineToRel(dx)),
            swiftWriter.as(swifty.path.horizLineToRel(dx)),
            objcWriter.as(objcy.path.horizLineToRel(dx)),
            if (debug) Some(Steque.single(s"HorizLineToRel($dx)")) else None)

        override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.cubic(x1, y1, x2, y2, x, y)),
            swiftWriter.as(swifty.path.cubic(x1, y1, x2, y2, x, y)),
            objcWriter.as(objcy.path.cubic(x1, y1, x2, y2, x, y)),
            if (debug) Some(Steque.single(s"Cubic($x1, $y1, $x2, $y2, $x, $y)")) else None)

        override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.cubicRel(x1, y1, x2, y2, dx, dy)),
            swiftWriter.as(swifty.path.cubicRel(x1, y1, x2, y2, dx, dy)),
            objcWriter.as(objcy.path.cubicRel(x1, y1, x2, y2, dx, dy)),
            if (debug) Some(Steque.single(s"CubicRel($x1, $y1, $x2, $y2, $dx, $dy)")) else None)

        override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.smoothCubic(x2, y2, x, y)),
            swiftWriter.as(swifty.path.smoothCubic(x2, y2, x, y)),
            objcWriter.as(objcy.path.smoothCubic(x2, y2, x, y)),
            if (debug) Some(Steque.single(s"SmoothCubic($x2, $y2, $x, $y)")) else None)

        override def smoothQuad(x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.smoothQuad(x, y)),
            swiftWriter.as(swifty.path.smoothQuad(x, y)),
            objcWriter.as(objcy.path.smoothQuad(x, y)),
            if (debug) Some(Steque.single(s"SmoothQuad($x, $y)")) else None)

        override def smoothQuadRel(dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.smoothQuadRel(dx, dy)),
            swiftWriter.as(swifty.path.smoothQuadRel(dx, dy)),
            objcWriter.as(objcy.path.smoothQuadRel(dx, dy)),
            if (debug) Some(Steque.single(s"SmoothQuadRel($dx, $dy)")) else None)

        override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)),
            swiftWriter.as(swifty.path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)),
            objcWriter.as(objcy.path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)),
            if (debug) Some(Steque.single(s"Elliptic($rx, $ry, $rotX, $largeArc, $sweep, $x, $y)")) else None)

        override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.smoothCubicRel(x2, y2, dx, dy)),
            swiftWriter.as(swifty.path.smoothCubicRel(x2, y2, dx, dy)),
            objcWriter.as(objcy.path.smoothCubicRel(x2, y2, dx, dy)),
            if (debug) Some(Steque.single(s"SmoothCubicRel($x2, $y2, $dx, $dy)")) else None)

        override def quad(x1: Double, y1: Double, x: Double, y: Double): Paths =
          (androidWriter.as(andy.path.quad(x1, y1, x, y)),
            swiftWriter.as(swifty.path.quad(x1, y1, x, y)),
            objcWriter.as(objcy.path.quad(x1, y1, x, y)),
            if (debug) Some(Steque.single(s"Quad($x1, $y1, $x, $y)")) else None)

        override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.quadRel(x1, y1, dx, dy)),
            swiftWriter.as(swifty.path.quadRel(x1, y1, dx, dy)),
            objcWriter.as(objcy.path.quadRel(x1, y1, dx, dy)),
            if (debug) Some(Steque.single(s"QuadRel($x1, $y1, $dx, $dy)")) else None)

        override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): Paths =
          (androidWriter.as(andy.path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)),
            swiftWriter.as(swifty.path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)),
            objcWriter.as(objcy.path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)),
            if (debug) Some(Steque.single(s"EllipticRel($rx, $ry, $rotX, $largeArc, $sweep, $dx, $dy)")) else None)
      }

      override val empty: Steque[() => Unit] = Steque.empty

      override def append(fst: Steque[() => Unit], snd: Steque[() => Unit]): Steque[() => Unit] =
        fst ++: snd

      override def circle(x: Double, y: Double, r: Double): Steque[() => Unit] = Steque.single({ () =>
        androidWriter.foreach(writer => andy.circle(x, y, r).foreach(writer.writeStr(_)))
        objcWriter.foreach(writer => objcy.circle(x, y, r).foreach(writer.writeStr(_)))
        swiftWriter.foreach(writer => swifty.circle(x, y, r).foreach(writer.writeStr(_)))
        if (debug) println(s"circle($x, $y, $r)")
      })

      override def ellipse(x: Double, y: Double, rx: Double, ry: Double): Steque[() => Unit] = Steque.single({ () =>
        androidWriter.foreach(writer => andy.ellipse(x, y, rx, ry).foreach(writer.writeStr(_)))
        objcWriter.foreach(writer => objcy.ellipse(x, y, rx, ry).foreach(writer.writeStr(_)))
        swiftWriter.foreach(writer => swifty.ellipse(x, y, rx, ry).foreach(writer.writeStr(_)))
        if (debug) println(s"ellipse($x, $y, $rx, $ry)")
      })

      override def rect(x: Double, y: Double, rx: Double, ry: Double): Steque[() => Unit] = Steque.single({ () =>
        androidWriter.foreach(writer => andy.rect(x, y, rx, ry).foreach(writer.writeStr(_)))
        objcWriter.foreach(writer => objcy.rect(x, y, rx, ry).foreach(writer.writeStr(_)))
        swiftWriter.foreach(writer => swifty.rect(x, y, rx, ry).foreach(writer.writeStr(_)))
        if (debug) println(s"rect($x, $y, $rx, $ry)")
      })

      override def text(text: String, x: Double, y: Double): Steque[() => Unit] = Steque.single({ () =>
        androidWriter.foreach(writer => andy.text(text, x, y).foreach(writer.writeStr(_)))
        swiftWriter.foreach(writer => andy.text(text, x, y).foreach(writer.writeStr(_)))
        objcWriter.foreach(writer => andy.text(text, x, y).foreach(writer.writeStr(_)))
        if (debug) println(s"text($text, $x, $y)")
      })

      override def includePath(paths: Paths): Steque[() => Unit] = Steque.single({ () =>
        map2(androidWriter, paths._1)((writer, p) => andy.includePath(p).foreach(writer.writeStr(_)))
        map2(swiftWriter, paths._2)((writer, p) => swifty.includePath(p).foreach(writer.writeStr(_)))
        map2(objcWriter, paths._3)((writer, p) => objcy.includePath(p).foreach(writer.writeStr(_)))
        if (debug) paths._4.foreach(_.foreach(System.out.println))
      })
    }

    androidWriter.foreach(_.writeStr(andySurrounders.prefixFromName(inputFile.getName)))
    swiftWriter.foreach(_.writeStr(swiftySurrounders.prefixFromName(inputFile.getName)))
    objcWriter.foreach(_.writeStr(objcySurrounders.prefixFromName(inputFile.getName)))
    run(inputFile, time, renderer).map(_.foreach(_ ())).map { _ =>
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
      xmlparse.Parse.parsers.get(label).flatMap(_.apply(attrs, finalSVG)).getOrElse(finalSVG.empty)

    // here remove styles
    override def elemEnd(label: String): A = finalSVG.empty

    // TODO: handle <tspan>
    override def text(label: String, attrs: MetaData, text: String): A =
      xmlparse.Parse.terminalParsers.get(label).flatMap(_.apply(attrs, text, finalSVG)).getOrElse(finalSVG.empty)

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

  implicit val timedParser: ArgParser[Timed] =
    ArgParser.instance[Timed]("timed") { s =>
      if (s.startsWith("overall:"))
        ArgParser
          .int(current = None, s.substring("overall:".length), mandatory = true)
          .map(t => TimeOverall(iterations = t._2))
      else if (s == "overall") Right(TimeOverall(iterations = 1))
      else if (s == "each") Right(TimeEachFile)
      else if (s.startsWith("first:"))
        ArgParser
          .int(current = None, s.substring("first:".length), mandatory = true)
          .map(t => TimeFirst(t._2))
      else Left(s"$s is not a valid time setting: try each or overall")
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
        val iterations =
          t.timed.collect { case TimeOverall(iters) => iters }.getOrElse(1)
        (1 to iterations).foreach(_ => t.main(identity))
    }
  }

  implicit final class fileOutputStreamWriteStrOp(val stream: FileOutputStream) extends AnyVal {
    def writeStr(str: String) = stream.write(str.getBytes())
  }

}

