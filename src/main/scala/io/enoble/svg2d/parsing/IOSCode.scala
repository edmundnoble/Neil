package io.enoble.svg2d.parsing
import scalaz.Free.Trampoline
import scalaz._
import Scalaz._

import IOSCode.{SwiftCode, ObjectiveCCode}

case class IOSCode(swiftGen: Trampoline[SwiftCode], objectiveCGen: Trampoline[ObjectiveCCode])

object IOSCode {

  sealed trait Swift

  sealed trait ObjectiveC

  type SwiftCode = String @@ Swift
  type ObjectiveCCode = String @@ ObjectiveC

  def makeSwift(snippet: String): SwiftCode = Tag.of[Swift](snippet)

  def makeObjC(snippet: String): ObjectiveCCode = Tag.of[ObjectiveC](snippet)

  import Trampoline._

  def swiftAppend(firstSnippet: SwiftCode, secondSnippet: SwiftCode): SwiftCode = {
    val unwrappedFirst = Tag.unwrap(firstSnippet)
    val unwrappedSecond = Tag.unwrap(secondSnippet)
    if (unwrappedFirst.isEmpty) {
      firstSnippet
    } else if (unwrappedSecond.isEmpty) {
      secondSnippet
    } else {
      makeSwift(unwrappedFirst + "\n" + unwrappedSecond)
    }
  }

  def objectiveCAppend(firstSnippet: ObjectiveCCode, secondSnippet: ObjectiveCCode): ObjectiveCCode = {
    val unwrappedFirst = Tag.unwrap(firstSnippet)
    val unwrappedSecond = Tag.unwrap(secondSnippet)
    if (unwrappedFirst.isEmpty) {
      firstSnippet
    } else if (unwrappedSecond.isEmpty) {
      secondSnippet
    } else {
      makeObjC(unwrappedFirst + ";\n" + unwrappedSecond)
    }
  }

  val empty: IOSCode = IOSCode(done(makeSwift("")), done(makeObjC("")))

  trait IOSInstances extends Monoid[IOSCode] {
    override def zero: IOSCode = IOSCode.empty

    override def append(first: IOSCode, f2: => IOSCode): IOSCode = {
      lazy val second = f2
      val newSwiftGen = for {
        firstSwift <- suspend(first.swiftGen)
        secondSwift <- suspend(second.swiftGen)
      } yield swiftAppend(firstSwift, secondSwift)
      val newObjectiveCGen = for {
        firstObjC <- suspend(first.objectiveCGen)
        secondObjC <- suspend(second.objectiveCGen)
      } yield objectiveCAppend(firstObjC, secondObjC)
      IOSCode(newSwiftGen, newObjectiveCGen)
    }
  }
}
