package io
package enoble
package svg2d
package utils

object Timed {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      Console.err.println("Wrong number of arguments!")
      sys.exit(1)
    }
    val time = System.nanoTime
    Main.main(args)
    val timeAfter = System.nanoTime
    println(s"Time taken to generate code: ${(timeAfter - time) / 1000000.0} milliseconds")
  }

}
