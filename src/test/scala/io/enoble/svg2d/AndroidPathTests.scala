package io
package enoble
package svg2d

import io.enoble.svg2d.ast._
import io.enoble.svg2d.render.AndroidRenderer
import io.enoble.svg2d.render.AndroidRenderer.PathState
import org.scalatest.FreeSpec

class AndroidPathTests extends FreeSpec {

  def rendererTest[A](input: Vector[InitialPath], initialState: PathState = PathState(here = (0, 0), indentation = 0),
                      expectedOutput: String, expectedState: Option[PathState] = None): Unit = {
    val out = AndroidRenderer(rwrsbActionMonoid).path.renderInitial(input).run(initialState).value
    assert(out._2.asString == expectedOutput)
    expectedState.foreach(s => assert(out._1 == s))
  }


  "MoveTo" in rendererTest(
    input = Vector(MoveTo(Vector((1.0, 1.0)))),
    expectedOutput = "path.moveTo(1.0, 1.0);\n"
  )

  "MoveTo with indentation" in rendererTest(
    input = Vector(MoveTo(Vector((0.0, 0.0)))),
    initialState = PathState(here = (0, 0), indentation = 1),
    expectedOutput = "    path.moveTo(0.0, 0.0);\n"
  )

  "Multiple MoveTo arguments" in rendererTest(
    input = Vector(MoveTo(Vector((0.0, 0.0), (1.0, 1.0)))),
    expectedOutput = "path.moveTo(0.0, 0.0);\npath.moveTo(1.0, 1.0);\n"
  )

  "MoveToRel" in rendererTest(
    input = Vector(MoveToRel(Vector((0.0, 0.0)))),
    expectedOutput = "path.rMoveTo(0.0, 0.0);\n"
  )

  "LineTo" in rendererTest(
    input = Vector(LineTo(Vector((0.0, 0.0)))),
    expectedOutput = "path.lineTo(0.0, 0.0);\n"
  )

  "LineToRel" in rendererTest(
    input = Vector(LineToRel(Vector((0.0, 0.0)))),
    expectedOutput = "path.rLineTo(0.0, 0.0);\n"
  )

  "Empty" in rendererTest(
    input = Vector.empty,
    expectedOutput = ""
  )

}
