package io.enoble.svg2d.parsing

import io.enoble.svg2d.ast.FinalSVG

object RectParser extends Model {
  override def apply[A](v1: xml.Elem, svg: FinalSVG[A]): Option[Option[A]] =
    if (v1.label ~= "rect") {
      System.err.println("Tried to parse a rect!")
      Some(Some(???))
    } else {
      None
    }
}

