package io.enoble.svg2d

import io.enoble.svg2d.parsing.Path
import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

class PathTests extends FunSuite {

  import fastparse.all._
  import Path._
  import Path.Parsers._

  test("A simple path should be parsable") {
    val simplePath = "m 25.000985,1015.208 c -0.23015,0 -0.36032,0.05 -0.55274,0.2422 l -1.6914,-1.6914 c -0.1936,-0.1936 -0.32064,-0.2422 -0.55078,-0.2422 z"
    val parsed = path.parse(simplePath)
    assert(parsed == Result.Success(Path(ArrayBuffer(MoveToRel(ArrayBuffer((25.000985, 1015.208))), CubicRel(ArrayBuffer(((-0.23015, 0.0), (-0.36032, 0.05), (-0.55274, 0.2422)))),
      LineToRel(ArrayBuffer((-1.6914, -1.6914))), CubicRel(ArrayBuffer(((-0.1936, -0.1936), (-0.32064, -0.2422), (-0.55078, -0.2422)))), ClosePath())), simplePath.length))
  }

  test("A quadratic should be parsable") {
    val simpleQuad = "q 0,0 0,0"
    val parsed = quad.parse(simpleQuad)
    assert(parsed == Result.Success(QuadRel(ArrayBuffer(((0.0, 0.0), (0.0, 0.0)))), simpleQuad.length))
  }

  test("An elliptic should be parsable") {
    val simpleElliptic = "a 0,0 0 0,0 0,0"
    val parsed = ellipticalArc.parse(simpleElliptic)
    assert(parsed == Result.Success(EllipticRel(ArrayBuffer(EllipticParam((0.0, 0.0), 0.0, largeArc = false, sweep = false, (0.0, 0.0)))), simpleElliptic.length))
  }

  test("A moveto should be parsable as a full path") {
    val simpleMoveTo = "m 157.86809,496.83258 0,-32.796 -5.16279,0 0,32.796 5.16279,0"
    val parsed = path.parse(simpleMoveTo)
    assert(parsed == Result.Success(Path(ArrayBuffer(MoveToRel(ArrayBuffer((157.86809,496.83258), (0.0,-32.796), (-5.16279,0.0), (0.0,32.796), (5.16279,0.0))))), simpleMoveTo.length))
  }

  test("This path from camera_video_record.svg should be parsable") {
    val testPath = "m 0,324 v 768 q 0,47 18.5,89 18.5,42 50,72.5 31.5,30.5 73.5,49 42,18.5 89,18.5 h 768 q 47,0 89,-18.5 42,-18.5 73,-49 31,-30.5 49.5,-72.5 18.5,-42 18.5,-89 V 852 l 483,471 q 23,23 55,23 13,-1 29,-7 47,-20 47,-69 V 143 q 0,-49 -47,-69 -16,-6 -29,-6 -33,0 -55,22 L 1229,561 V 324 q 0,-47 -18.5,-89 Q 1192,193 1161,161 1130,129 1088.5,110.5 1047,92 999,92 H 231 Q 184,92 142,110.5 100,129 68.5,161 37,193 18.5,234.5 0,276 0,324 z"
    val Result.Success(parsed, _) = path.parse(testPath)
  }
}