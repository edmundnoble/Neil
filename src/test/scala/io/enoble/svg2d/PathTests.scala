package io
package enoble
package svg2d

import io.enoble.svg2d.ast._
import io.enoble.svg2d.render.InitialPathRenderer
import io.enoble.svg2d.xmlparse.Path
import io.enoble.svg2d.xmlparse.Path.Parsers
import org.scalatest.FreeSpec

class PathTests extends FreeSpec {

  import fastparse.all.Parsed

  implicit val parsers: Parsers[Vector[InitialPath]] = Path.Parsers(InitialPathRenderer)

  import parsers._

  def parserTest(input: String, expectedOutput: Parsed[Vector[InitialPath]]): Unit = {
    assert(parsers.path.parse(input) == expectedOutput)
  }

  "A simple path should be parsable" in parserTest(
    "m 25.000985,1015.208 c -0.23015,0 -0.36032,0.05 -0.55274,0.2422 l -1.6914,-1.6914 c -0.1936,-0.1936 -0.32064,-0.2422 -0.55078,-0.2422 z",
    Parsed.Success(
      Vector(
        MoveToRel(25.000985, 1015.208),
        CubicRel(-0.23015, 0.0, -0.36032, 0.05, -0.55274, 0.2422),
        LineToRel(-1.6914, -1.6914),
        CubicRel(-0.1936, -0.1936, -0.32064, -0.2422, -0.55078, -0.2422),
        ClosePath
      ),
      135)
  )

  "Quadratics should be parsable" in parserTest(
    "m 0,0 q 0,0 0,0 Q 0,0 0,0",
    Parsed.Success(
      Vector(
        MoveToRel(0.0, 0.0),
        QuadRel(0.0, 0.0, 0.0, 0.0),
        Quad(0.0, 0.0, 0.0, 0.0)
      ),
      25
    )
  )

  "Elliptics should be parsable" in parserTest(
    "m 0,0 a 0,0 0 0,0 0,0 A 0,0 0 0,0 0,0",
    Parsed.Success(
      Vector(
        MoveToRel(0.0, 0.0),
        EllipticRel(0.0, 0.0, 0.0, largeArc = false, sweep = false, 0.0, 0.0),
        Elliptic(0.0, 0.0, 0.0, largeArc = false, sweep = false, 0.0, 0.0)
      ),
      37
    )
  )

  "A moveto should be parsable as a full path" in parserTest(
    "m 157.86809,496.83258 0,-32.796 -5.16279,0 0,32.796 5.16279,0",
    Parsed.Success(
      Vector(
        MoveToRel(157.86809, 496.83258),
        MoveToRel(0.0, -32.796),
        MoveToRel(-5.16279, 0.0),
        MoveToRel(0.0, 32.796),
        MoveToRel(5.16279, 0.0)
      ), 61
    )
  )

  "This path from camera_video_record.svg should be parsable" in {
    val testPath = "m 0,324 v 768 q 0,47 18.5,89 18.5,42 50,72.5 31.5,30.5 73.5,49 42,18.5 89,18.5 h 768 q 47,0 89,-18.5 42,-18.5 73,-49 31,-30.5 49.5,-72.5 18.5,-42 18.5,-89 V 852 l 483,471 q 23,23 55,23 13,-1 29,-7 47,-20 47,-69 V 143 q 0,-49 -47,-69 -16,-6 -29,-6 -33,0 -55,22 L 1229,561 V 324 q 0,-47 -18.5,-89 Q 1192,193 1161,161 1130,129 1088.5,110.5 1047,92 999,92 H 231 Q 184,92 142,110.5 100,129 68.5,161 37,193 18.5,234.5 0,276 0,324 z"
    val Parsed.Success(_, _) = path.parse(testPath)
  }

  "Paths with invalid elements should not be parsable" in {
    val testPath = "x 0,1"
    val Parsed.Failure(_, _, _) = path.parse(testPath)
  }
}