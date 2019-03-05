package fuselang

import sys.process._
import java.nio.file.{Files, Paths, Path}

import Errors.HeaderMissing

/**
 * Provides utilities to compile a program and link it with headers required
 * by the CppRunnable backend.
 */
object GenerateExec {
  val headers = List("parser.cpp", "picojson.h")

  val headerLocation = Paths.get("src/main/resources/headers")

  val CXX = s"g++ --std=c++11 -Wall -I$headerLocation"

  /**
   * Generates an executable object [[out]]. Assumes that [[src]] is a valid
   * C++ file. Returns the result of running the compilations commands.
   */
  def generateExec(src: Path, out: String): Int = {
    // Make sure all headers are downloaded.
    for (header <- headers) {
      if (Files.exists(headerLocation.resolve(header)) == false) {
        throw HeaderMissing(header, headerLocation.toString)
      }
    }

    // Generate [[out]]. `!` is defined by sys.process:
    // https://www.scala-lang.org/api/2.12.8/scala/sys/process/index.html
    val cmd = s"$CXX ${src.toString} -o $out"
    println(cmd)
    cmd.!
  }
}
