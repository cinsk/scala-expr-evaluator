package cinsk.hello

import java.io.File
import scopt._

case class OptionConfig(out: File = new File("."),
                        verbose: Boolean = false,
                        version: Boolean = false,
                        debug: Boolean = false,
                        args: Seq[String] = Seq(),
                        kwargs: Map[String, String] = Map())

object Hello {
  val versionString = BuildInfo.version
  val programName = BuildInfo.name

  def showVersionAndExit {
    println(s"$programName version $versionString")
    System.exit(0)
  }

  def main(args: Array[String]) {
    val optParser = new scopt.OptionParser[OptionConfig](programName) {
        head("Short description of the program")
        help("help") text("display this help and exit")
        opt[Unit]("verbose") action { (_, c) =>
          c.copy(verbose = true) } text("verbose is a flag")
        opt[Unit]("version") action { (_, c) =>
          c.copy(version = true) } text("output version information and exit")
        opt[String]("ARG...") unbounded() optional() action { (x, c) =>
          c.copy(args = c.args :+ x) } text("Optional unbounded args")
        note("")
      }

    val c: OptionConfig = optParser.parse(args, OptionConfig()).orNull
    if (c == null) {
      System.err.println("parse error")
      System.exit(1)
    }

    if (c.version)
      showVersionAndExit

    if (c.verbose)
      println("verbose on")

    for (a <- c.args)
      println(s"arg: $a")
  }
}

