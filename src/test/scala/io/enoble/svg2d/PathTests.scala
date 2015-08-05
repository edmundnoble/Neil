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
    assert(parsed == Result.Success(EllipticRel(ArrayBuffer(EllipticParam((0.0, 0.0), 0.0, false, false, (0.0, 0.0)))), simpleElliptic.length))
  }

  test("A moveto should be parsable as a full path") {
    val simpleMoveTo = "m 157.86809,496.83258 0,-32.796 -5.16279,0 0,32.796 5.16279,0"
    val parsed = path.parse(simpleMoveTo)
    assert(parsed == Result.Success(Path(ArrayBuffer(MoveToRel(ArrayBuffer((157.86809,496.83258), (0.0,-32.796), (-5.16279,0.0), (0.0,32.796), (5.16279,0.0))))), simpleMoveTo.length))
  }

}