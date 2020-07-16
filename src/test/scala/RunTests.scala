package fuselang

import sys.process._
import scala.concurrent._
import org.scalatest.Tag

import java.nio.file.{Files, Paths, Path}
import scala.jdk.CollectionConverters._

import common._
import Configuration.Config

object SlowTest extends Tag("fuselang.tag.SlowTest")

object AsyncRun {

  def asyncProcRun(
      proc: Seq[String]
  )(implicit ec: ExecutionContext): Future[Either[String, Int]] = {
    val err = new StringBuilder
    val logger = ProcessLogger(_ => (), e => err ++= (e + "\n"))

    Future {
      val status = proc ! logger
      if (status != 0) Left(err.toString)
      else Right(status)
    }
  }

  def compileAndEval(conf: Config, data: Path)(
      implicit ec: ExecutionContext
  ): Future[Either[String, Int]] = {

    val compileToC = Future { Main.runWithConfig(conf) }

    compileToC.flatMap(s =>
      s match {
        case Left(_) => Future.successful(s)
        case Right(status) => {
          assert(status == 0, s"Status after running fuse was $status")
          val toExec = Paths.get(conf.output.get.toString + ".o")
          val runCmd = Seq(s"${toExec}", data.toString)
          toExec.toFile.setExecutable(true)
          asyncProcRun(runCmd)
        }
      }
    )
  }
}

class RunTests extends org.scalatest.AsyncFunSuite {

  // Suppress logging.
  common.Logger.setLogLevel(scribe.Level.Warn)

  val srcFilePattern = """.*\.fuse"""
  val lowerPattern = """.*should-lower.*"""

  // Create a temporary directory for generating C++ files.
  val tmpDir = Files.createDirectories(Paths.get(".").resolve("_test"))

  val shouldRun = Paths.get("src/test/should-run")
  val shouldLower = Paths.get("src/test/should-lower")
  val stream = Files.newDirectoryStream(shouldRun).asScala ++ Files
    .newDirectoryStream(shouldLower)
    .asScala

  for (file <- stream if file.toString.matches(srcFilePattern)) {
    test(file.toString, SlowTest) {
      val dataPath = Paths.get(file.toString + ".data.json")
      val outFile = tmpDir.resolve(file.getFileName.toString + ".cpp")

      // Make sure data file is present
      assert(
        Files.exists(dataPath),
        s"Data file ${dataPath} missing. Run tests require data files."
      )

      val conf = Config(
        srcFile = file.toFile(),
        mode = Configuration.Run,
        backend = Configuration.Cpp,
        output = Some(outFile.toString),
        enableLowering = file.toString.matches(lowerPattern)
      )

      AsyncRun
        .compileAndEval(conf, dataPath)
        .map(_ match {
          case Left(err) => assert(false, err)
          case Right(status) => Files.delete(outFile); assert(status == 0)
        })
    }
  }
}
