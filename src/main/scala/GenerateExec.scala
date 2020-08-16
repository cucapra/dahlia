package fuselang

import sys.process._
import scala.io.Source
import java.nio.file.{Files, Paths, Path, StandardOpenOption}

import common.CompilerError.HeaderMissing

/**
  * Provides utilities to compile a program and link it with headers required
  * by the CppRunnable backend.
  */
object GenerateExec {
  // TODO(rachit): Move this to build.sbt
  val headers = List("parser.cpp", "json.hpp")

  var headerLocation = Paths.get("src/main/resources/headers")
  val headerFallbackLocation = Paths.get("_headers/")


  // Not the compiler directory, check if the fallback directory has been setup.
  if (Files.exists(headerLocation) == false) {
    // Fallback for headers not setup. Unpack headers from JAR file.
    headerLocation = headerFallbackLocation

    if (Files.exists(headerFallbackLocation) == false) {
      scribe.warn(
        s"Missing headers required for `fuse run`." +
          s" Unpacking from JAR file into $headerFallbackLocation."
      )

      val dir = Files.createDirectory(headerFallbackLocation)
      for (header <- headers) {
        val stream = getClass.getResourceAsStream(s"/headers/$header")
        val hdrSource = Source.fromInputStream(stream).toArray.map(_.toByte)
        Files.write(
          dir.resolve(header),
          hdrSource,
          StandardOpenOption.CREATE_NEW,
          StandardOpenOption.WRITE
        )
      }
    }
  }

  /**
    * Generates an executable object [[out]]. Assumes that [[src]] is a valid
    * C++ file. Returns the result of running the compilations commands.
    *
    * The error message returned assumes that CXX would have created some
    * useful errors already.
    */
  def generateExec(
      src: Path,
      out: String,
      compilerOpts: List[String]
  ): Either[String, Int] = {

    // Make sure all headers are downloaded.
    for (header <- headers) {
      if (Files.exists(headerLocation.resolve(header)) == false) {
        throw HeaderMissing(header, headerLocation.toString)
      }
    }

    val CXX =
      Seq("g++", "-g", "--std=c++14", "-Wall", "-I", headerLocation.toString) ++ compilerOpts

    val stderr = new StringBuilder
    val logger = ProcessLogger(l => scribe.info(l), l => stderr ++= (l + "\n"))

    // Generate [[out]]. `!` is defined by sys.process:
    // https://www.scala-lang.org/api/2.12.8/scala/sys/process/index.html
    val cmd = CXX ++ Seq(src.toString, "-o", out)
    scribe.info(cmd.mkString(" "))
    val status = cmd ! logger

    if (status != 0) {
      Left(s"Failed to generate the executable $out.\n${stderr}")
    } else {
      Right(status)
    }
  }
}
