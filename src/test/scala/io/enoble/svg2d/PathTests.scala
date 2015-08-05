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

}