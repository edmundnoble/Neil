package io
package enoble
package svg2d
package utils

import scalaz.State

object Named {
  type Named[A] = State[Map[String, Int], A]

  def freshName(prefix: String): Named[String] = for {
    nameCounts <- State.get[Map[String, Int]]
    count = nameCounts.getOrElse(prefix, 0) + 1
    newName = prefix + count
    _ <- State.put(nameCounts + (prefix -> count))
  } yield newName

  def currentName(prefix: String): Named[String] = for {
    nameCounts <- State.get[Map[String, Int]]
    count = nameCounts.getOrElse(prefix, 0)
    current = prefix + count
  } yield current

}


