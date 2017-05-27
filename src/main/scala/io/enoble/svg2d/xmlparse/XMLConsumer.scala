package io.enoble.svg2d.xmlparse

import scala.xml.MetaData

trait XMLConsumer[A] {
  val start: A

  def progress(fst: A, snd: A): A

  def elemStart(label: String, attrs: MetaData): A

  def text(label: String, attrs: MetaData, text: String): A

  def elemEnd(label: String): A

  def entityRef(entity: String): A
}
