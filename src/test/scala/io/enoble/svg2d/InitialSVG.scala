package io.enoble.svg2d

sealed trait InitialSVG

object InitialSVG {

  final case class DrawText(text: String, posX: Double, posY: Double) extends InitialSVG

  final case class DrawEllipse(x: Double, y: Double, rx: Double, ry: Double) extends InitialSVG

  final case class DrawCircle(x: Double, y: Double, r: Double) extends InitialSVG

  final case class DrawRect(x: Double, y: Double, w: Double, h: Double) extends InitialSVG

  final case class DrawPath(commands: InitialPath*) extends InitialSVG

}
