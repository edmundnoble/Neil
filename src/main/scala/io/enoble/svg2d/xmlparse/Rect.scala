package io.enoble.svg2d.xmlparse

import io.enoble.svg2d.ast.FinalSVG

object Rect extends Model {
  override val label: String = "rect"
  override def apply[A](v1: xml.Elem, svg: FinalSVG[A]): Option[A] = {
    System.err.println("Tried to parse a rect!")
    Some(???)
  }
}

