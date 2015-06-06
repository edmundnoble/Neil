package io.enoble.svg2d.drawables

import com.squareup.javapoet.MethodSpec

import scala.xml.Elem
import scalaz.Free.Trampoline
import scalaz._
import Scalaz._

object Parse {
  def parseAll(x: xml.Elem): Option[Result] = {
    def inner(x: xml.Node): Trampoline[Option[Result]] = {
      val res = x match {
        case x1: Elem =>
          parseDrawable(x1)
        case _ =>
          Some("", "")
      }
      val tramp = x.child.foldLeft(Trampoline.delay(Option(("", "")))) { (acc, x) =>
        for {
          rep <- inner(x)
          prev <- acc
        } yield prev |+| rep
      }
      for {
        t <- tramp
      } yield for {
        r <- res
        ta <- t
      } yield ta |+| r
    }
    inner(x).run
  }
  def parseDrawable = Circle
}
