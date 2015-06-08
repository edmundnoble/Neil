package io.enoble.svg2d.parsing

import scala.xml.Elem

object Text extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "text"
  override def apply(v1: Elem): Option[Code] = {
    None
  }
}
