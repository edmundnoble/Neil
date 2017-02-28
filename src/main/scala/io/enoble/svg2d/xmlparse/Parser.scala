package io.enoble.svg2d.xmlparse

import scala.io.Source
import scala.xml.{MetaData, NamespaceBinding, NodeSeq}
import scala.xml.parsing.{ExternalSources, MarkupHandler, MarkupParser}

final class XmlParser[A](override val input: Source, consumer: XMLConsumer[A])
  extends MarkupHandler with MarkupParser with ExternalSources with Runnable {
  private var state: A = consumer.start

  def getState: A = state

  // track level for elem memory usage optimization
  private var level = 0
  // this is a dummy to satisfy MarkupHandler's API
  // memory usage optimization return one <ignore/> for top level to satisfy
  // MarkupParser.document() otherwise NodeSeq.Empty
  private var ignoreWritten = false
  private var textStack: List[(String, MetaData)] = Nil

  override def elem(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding, empty: Boolean, args: NodeSeq): NodeSeq =
    if (level == 1 && !ignoreWritten) {
      ignoreWritten = true
        <ignore/>
    } else NodeSeq.Empty

  override def procInstr(pos: Int, target: String, txt: String): NodeSeq = NodeSeq.Empty

  override def comment(pos: Int, comment: String): NodeSeq = NodeSeq.Empty

  override def entityRef(pos: Int, n: String): NodeSeq = {
    state = consumer.progress(state, consumer.entityRef(n))
    NodeSeq.Empty
  }

  override def text(pos: Int, txt: String): NodeSeq = {
    state = consumer.progress(state, consumer.text(textStack.head._1, textStack.head._2, txt))
    NodeSeq.Empty
  }

  override val preserveWS: Boolean = false

  override def run(): Unit = {
    curInput = input
    this.initialize.document()
  }

  override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
    state = consumer.progress(state, consumer.elemStart(label, attrs))
    textStack ::= (label -> attrs)
    level += 1
  }

  override def elemEnd(pos: Int, pre: String, label: String) {
    state = consumer.progress(state, consumer.elemEnd(label))
    textStack = textStack.tail
    level -= 1
  }
}

