package io
package enoble
package svg2d

import cats.data.State
import io.enoble.svg2d.ast._
import InitialPath._
import io.enoble.svg2d.render.AndroidRenderer
import io.enoble.svg2d.render.AndroidRenderer.PathState
import org.scalatest.FreeSpec

class AndroidPathRendererTests extends FreeSpec {

  val initialState = PathState(
    hereX = 0, hereY = 0,
    indentation = 0,
    lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN,
    lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN
  )

  val renderer = AndroidRenderer(sbActionMonoid).path

  final def rendererTest(input: State[PathState, SBAction], initialState: PathState = initialState,
                         expectedOutput: String, expectedState: Option[PathState] = None): Unit = {
    val (state, output) = input.run(initialState).value
    assert(output.asString == expectedOutput)
    expectedState.foreach(s => assert(state == s))
  }
  
  import renderer._

  "MoveTo" in rendererTest(
    input = moveTo(1.0, 1.0),
    expectedOutput = "path.moveTo(1.0, 1.0);\n"
  )

  "MoveTo with indentation" in rendererTest(
    input = moveTo(0.0, 0.0),
    initialState = initialState.copy(indentation = 1),
    expectedOutput = "    path.moveTo(0.0, 0.0);\n"
  )

  "Multiple MoveTo arguments" in rendererTest(
    input = combineAll(moveTo(0.0, 0.0), moveTo(1.0, 1.0)),
    expectedOutput = "path.moveTo(0.0, 0.0);\npath.moveTo(1.0, 1.0);\n"
  )

  "MoveToRel" in rendererTest(
    input = moveToRel(0.0, 0.0),
    expectedOutput = "path.rMoveTo(0.0, 0.0);\n"
  )

  "LineTo" in rendererTest(
    input = lineTo(0.0, 0.0),
    expectedOutput = "path.lineTo(0.0, 0.0);\n"
  )

  "LineToRel" in rendererTest(
    input = lineToRel(0.0, 0.0),
    expectedOutput = "path.rLineTo(0.0, 0.0);\n"
  )

  "VerticalLineTo" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), verticalLineTo(1.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.lineTo(1.0, 1.0);\n"
  )

  "VerticalLineToRel" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), verticalLineToRel(1.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rLineTo(0.0, 1.0);\n"
  )

  "HorizLineTo" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), horizLineTo(2.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.lineTo(2.0, 2.0);\n"
  )

  "HorizLineToRel" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), horizLineToRel(2.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rLineTo(2.0, 0.0);\n"
  )

  "Cubic" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), cubic(1, 2, 3, 4, 5, 6)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.cubicTo(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);\n"
  )

  "CubicRel" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), cubicRel(1, 2, 3, 4, 5, 6)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rCubicTo(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);\n"
  )

  "SmoothCubic" - {
    "basic" in rendererTest(
      input = combineAll(moveTo(100.0, 200.0), cubic(100, 100, 250, 100, 250, 200), smoothCubic(400, 300, 400, 200)),
      expectedOutput = "path.moveTo(100.0, 200.0);\npath.cubicTo(100.0, 100.0, 250.0, 100.0, 250.0, 200.0);\npath.cubicTo(250.0, 300.0, 400.0, 300.0, 400.0, 200.0);\n"
    )
  }

  "SmoothCubicRel" - {
    "basic" in rendererTest(
      input = combineAll(moveTo(100.0, 200.0), cubicRel(100, 100, 250, 100, 250, 200), smoothCubicRel(100, 100, 250, 100)),
      expectedOutput = "path.moveTo(100.0, 200.0);\npath.rCubicTo(100.0, 100.0, 250.0, 100.0, 250.0, 200.0);\npath.rCubicTo(0.0, 100.0, 100.0, 100.0, 250.0, 100.0);\n"
    )
  }

  "Quad" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), quad(1, 2, 3, 4)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.quadTo(1.0, 2.0, 3.0, 4.0);\n"
  )

  "QuadRel" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), quadRel(1, 2, 3, 4)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rQuadTo(1.0, 2.0, 3.0, 4.0);\n"
  )

  "SmoothQuad" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), quad(1, 2, 3, 4), smoothQuad(5, 6)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.quadTo(1.0, 2.0, 3.0, 4.0);\npath.quadTo(5.0, 6.0, 5.0, 6.0);\n"
  )

  "SmoothQuadRel" in rendererTest(
    input = combineAll(moveTo(1.0, 2.0), quadRel(1, 2, 3, 4), smoothQuadRel(5, 6)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rQuadTo(1.0, 2.0, 3.0, 4.0);\npath.rQuadTo(2.0, 2.0, 5.0, 6.0);\n"
  )

  //  "Elliptic" in rendererTest(
  //    input = moveTo(Vector((1.0, 2.0))), verticalLineTo(Vector(1.0))),
  //    expectedOutput = "path.moveTo(0.0, 0.0);\npath.rLineTo(0.0, 1.0);\n"
  //  )
  //
  //  "EllipticRel" in rendererTest(
  //    input = moveTo(Vector((1.0, 2.0))), verticalLineTo(Vector(1.0))),
  //    expectedOutput = "path.moveTo(0.0, 0.0);\npath.rLineTo(0.0, 1.0);\n"
  //  )

  "Empty" in rendererTest(
    input = empty,
    expectedOutput = ""
  )

}
