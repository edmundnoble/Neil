package io.enoble.svg2d.xmlparse

import io.enoble.svg2d.ast.FinalSVG

import scala.xml.MetaData

trait Model extends TerminalModel {
  val label: String

  def apply[A](attrs: MetaData, svg: FinalSVG[A]): Option[A]
  final def apply[A](attrs: MetaData, text: String, svg: FinalSVG[A]): Option[A] = apply(attrs, svg)
}

trait TerminalModel {
  val label: String

  def apply[A](attrs: MetaData, text: String, svg: FinalSVG[A]): Option[A]
}
