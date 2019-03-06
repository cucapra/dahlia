package fuselang

import sys.process._
import scala.io.Source
import java.nio.file.{Files, Paths, Path, StandardOpenOption}

import Errors.HeaderMissing

/**
 * Provides utilities to compile a program and link it with headers required
 * by the CppRunnable backend.
 */
object GenerateExec {
  val headers = List("parser.cpp", "picojson.h")

  var headerLocation = Paths.get("src/main/resources/headers")
  val headerFallbackLocation = Paths.get("_headers/")

  // Not the compiler directory, check if the fallback directory has been setup.
  if(Files.exists(headerLocation) == false) {
    // Fallback for headers not setup. Unpack headers from JAR file.
    headerLocation = headerFallbackLocation

    if (Files.exists(headerFallbackLocation) == false)  {
      println(s"Missing required headers for `fuse run`. Unpacking from JAR file into $headerFallbackLocation.")

      val dir = Files.createDirectory(headerFallbackLocation)
      for (header <- headers) {
        val stream = getClass.getResourceAsStream(s"/headers/$header")
        val hdrSource = Source.fromInputStream(stream).toArray.map(_.toByte)
        Files.write(
          dir.resolve(header),
          hdrSource,
          StandardOpenOption.CREATE_NEW,
          StandardOpenOption.WRITE)
      }
    }
  }

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
