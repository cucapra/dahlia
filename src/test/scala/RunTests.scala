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

  def compileAndEval(src: Path, data: Path, out: Path)(
      implicit ec: ExecutionContext
  ): Future[Either[String, Int]] = {

    val conf = Config(
      srcFile = src.toFile(),
      mode = Configuration.Run,
      backend = Configuration.Cpp,
      output = Some(out.toString)
    )

    val compileToC = Future { Main.runWithConfig(conf) }

    compileToC.flatMap(s =>
      s match {
        case Left(_) => Future.successful(s)
        case Right(status) => {
          assert(status == 0, s"Status after running fuse was $status")
          val toExec = Paths.get(out.toString + ".o")
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

  val shouldRun = Paths.get("src/test/should-run")
  val srcFilePattern = """.*\.fuse"""

  // Create a temporary directory for generating C++ files.
  val tmpDir = Files.createDirectories(Paths.get(".").resolve("_test"))

  for (file <- Files.newDirectoryStream(shouldRun).asScala
       if file.toString.matches(srcFilePattern)) {
    test(file.toString, SlowTest) {
      val dataPath = Paths.get(file.toString + ".data.json")
      val outFile = tmpDir.resolve(file.getFileName.toString + ".cpp")

      // Make sure data file is present
      assert(
        Files.exists(dataPath),
        s"Data file ${dataPath} missing. Run tests require data files."
      )

      AsyncRun
        .compileAndEval(file, dataPath, outFile)
        .map(_ match {
          case Left(err) => assert(false, err)
          case Right(status) => Files.delete(outFile); assert(status == 0)
        })
    }
  }
}
