package io.enoble.svg2d

import io.enoble.svg2d.render.AndroidRenderer
import org.scalatest.FreeSpec

class AndroidRendererTests extends FreeSpec {

  val renderer = AndroidRenderer(sbActionMonoid)

  import renderer._

  final def rendererTest(input: SBAction,
                         expectedOutput: String): Unit = {
    assert(input.asString == expectedOutput)
  }

  "DrawText" in rendererTest(
    text("sample_text", 1, 1),
    "c.drawText(\"sample_text\", 1.0, 1.0, p);\n"
  )

  "DrawEllipse" in rendererTest(
    ellipse(1, 2, 3, 4),
    "c.drawOval(new RectF(-0.5, 4.0, 2.5, 0.0), p);\n"
  )

  "DrawRect" in rendererTest(
    rect(1, 2, 3, 4),
    "c.drawRect(1.0, 6.0, 4.0, 2.0, p);\n"
  )

  "DrawCircle" in rendererTest(
    circle(1, 2, 3),
    "c.drawCircle(1.0, 2.0, 3.0, p);\n"
  )

  "DrawPath" in rendererTest(
    includePath(path.append(path.moveTo(1, 1), path.verticalLineTo(2))),
    "{\n    Path path = new Path();\n    path.moveTo(1.0, 1.0);\n    path.lineTo(1.0, 2.0);\n}\n"
  )

  "DrawPath empty" in rendererTest(
    includePath(path.empty),
    "{\n    Path path = new Path();\n}\n"
  )

  "Empty" in rendererTest(
    input = empty,
    expectedOutput = ""
  )


}
