package io
package enoble
package svg2d

import io.enoble.svg2d.ast._
import io.enoble.svg2d.render.AndroidRenderer
import io.enoble.svg2d.render.AndroidRenderer.PathState
import org.scalatest.FreeSpec

class AndroidPathRendererTests extends FreeSpec {

  final def rendererTest[A](input: Vector[InitialPath], initialState: PathState = PathState(here = (0, 0), indentation = 0),
                            expectedOutput: String, expectedState: Option[PathState] = None): Unit = {
    val out = AndroidRenderer(rwrsbActionMonoid).path.renderInitial(input).run(initialState).value
    assert(out._2.asString == expectedOutput)
    expectedState.foreach(s => assert(out._1 == s))
  }

  "MoveTo" in rendererTest(
    input = Vector(MoveTo(1.0, 1.0)),
    expectedOutput = "path.moveTo(1.0, 1.0);\n"
  )

  "MoveTo with indentation" in rendererTest(
    input = Vector(MoveTo(0.0, 0.0)),
    initialState = PathState(here = (0, 0), indentation = 1),
    expectedOutput = "    path.moveTo(0.0, 0.0);\n"
  )

  "Multiple MoveTo arguments" in rendererTest(
    input = Vector(MoveTo(0.0, 0.0), MoveTo(1.0, 1.0)),
    expectedOutput = "path.moveTo(0.0, 0.0);\npath.moveTo(1.0, 1.0);\n"
  )

  "MoveToRel" in rendererTest(
    input = Vector(MoveToRel(0.0, 0.0)),
    expectedOutput = "path.rMoveTo(0.0, 0.0);\n"
  )

  "LineTo" in rendererTest(
    input = Vector(LineTo(0.0, 0.0)),
    expectedOutput = "path.lineTo(0.0, 0.0);\n"
  )

  "LineToRel" in rendererTest(
    input = Vector(LineToRel(0.0, 0.0)),
    expectedOutput = "path.rLineTo(0.0, 0.0);\n"
  )

  "VerticalLineTo" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), VerticalLineTo(1.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.lineTo(1.0, 1.0);\n"
  )

  "VerticalLineToRel" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), VerticalLineToRel(1.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rLineTo(0.0, 1.0);\n"
  )

  "HorizLineTo" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), HorizLineTo(2.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.lineTo(2.0, 2.0);\n"
  )

  "HorizLineToRel" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), HorizLineToRel(2.0)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rLineTo(2.0, 0.0);\n"
  )

  "Cubic" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), Cubic(1, 2, 3, 4, 5, 6)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.cubicTo(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);\n"
  )

  "CubicRel" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), CubicRel(1, 2, 3, 4, 5, 6)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rCubicTo(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);\n"
  )

  //  "SmoothCubic" in rendererTest(
  //    input = Vector(MoveTo(Vector((1.0, 2.0))), VerticalLineTo(Vector(1.0))),
  //    expectedOutput = "path.moveTo(0.0, 0.0);\npath.rLineTo(0.0, 1.0);\n"
  //  )
  //
  //  "SmoothCubicRel" in rendererTest(
  //    input = Vector(MoveTo(Vector((1.0, 2.0))), VerticalLineTo(Vector(1.0))),
  //    expectedOutput = "path.moveTo(0.0, 0.0);\npath.rLineTo(0.0, 1.0);\n"
  //  )

  "Quad" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), Quad(1, 2, 3, 4)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.quadTo(1.0, 2.0, 3.0, 4.0);\n"
  )

  "QuadRel" in rendererTest(
    input = Vector(MoveTo(1.0, 2.0), QuadRel(1, 2, 3, 4)),
    expectedOutput = "path.moveTo(1.0, 2.0);\npath.rQuadTo(1.0, 2.0, 3.0, 4.0);\n"
  )

  //  "Elliptic" in rendererTest(
  //    input = Vector(MoveTo(Vector((1.0, 2.0))), VerticalLineTo(Vector(1.0))),
  //    expectedOutput = "path.moveTo(0.0, 0.0);\npath.rLineTo(0.0, 1.0);\n"
  //  )
  //
  //  "EllipticRel" in rendererTest(
  //    input = Vector(MoveTo(Vector((1.0, 2.0))), VerticalLineTo(Vector(1.0))),
  //    expectedOutput = "path.moveTo(0.0, 0.0);\npath.rLineTo(0.0, 1.0);\n"
  //  )

  "Empty" in rendererTest(
    input = Vector.empty,
    expectedOutput = ""
  )

}
