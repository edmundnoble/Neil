package io.enoble.svg2d.xmlparse

import io.enoble.svg2d.ast.FinalSVG

trait Model {
  val label: String

  def apply[A](elem: xml.Elem, svg: FinalSVG[A]): Option[A]
}
