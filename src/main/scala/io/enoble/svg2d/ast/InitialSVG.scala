package io
package enoble
package svg2d
package ast

trait InitialSVG

final case class DrawText(text: String, posX: Double, posY: Double) extends InitialSVG
final case class DrawEllipse(x: Double, y: Double, rx: Double, ry: Double) extends InitialSVG
final case class DrawCircle(x: Double, y: Double, r: Double) extends InitialSVG
final case class DrawPath(commands: InitialPath*) extends InitialSVG
