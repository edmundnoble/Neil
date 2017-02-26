package io.enoble.svg2d

import io.enoble.svg2d.ast._
import io.enoble.svg2d.render.AndroidRenderer
import org.scalatest.FreeSpec

class AndroidRendererTests extends FreeSpec {

  final def rendererTest[A](input: Vector[InitialSVG],
                            expectedOutput: String): Unit = {
    val out = AndroidRenderer(rwrsbActionMonoid).renderInitial(input)
    assert(out.asString == expectedOutput)
  }

  "DrawText" in rendererTest(
    Vector(DrawText("sample_text", 1, 1)),
    "c.drawText(\"sample_text\", 1.0, 1.0, p);\n"
  )

  "DrawEllipse" in rendererTest(
    Vector(DrawEllipse(1, 2, 3, 4)),
    "c.drawOval(new RectF(-0.5, 4.0, 2.5, 0.0), p);\n"
  )

  "DrawCircle" in rendererTest(
    Vector(DrawCircle(1, 2, 3)),
    "c.drawCircle(1.0, 2.0, 3.0, p);\n"
  )

  "DrawPath" in rendererTest(
    Vector(DrawPath(MoveTo(1, 1), VerticalLineTo(2))),
    "{\n    Path path = new Path();\n    path.moveTo(1.0, 1.0);\n    path.lineTo(1.0, 2.0);\n}\n"
  )

  "DrawPath empty" in rendererTest(
    Vector(DrawPath()),
    "{\n    Path path = new Path();\n}\n"
  )

  "Empty" in rendererTest(
    input = Vector.empty,
    expectedOutput = ""
  )


}
