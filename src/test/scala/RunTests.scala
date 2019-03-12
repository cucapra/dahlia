package fuselang

import sys.process._
import scala.concurrent._

import java.nio.file.{Files, Paths, Path}
import collection.JavaConverters._

import org.scalatest.Tag

object SlowTest extends Tag("fuselang.tag.SlowTest")

object AsyncRun {

  def asyncProcRun(proc: Seq[String])(implicit ec: ExecutionContext, logger: ProcessLogger) =
    Future { proc ! logger }

  def compileAndEval
  (srcFile: Path, dataFile: Path, outFile: Path)
  (implicit ec: ExecutionContext, logger: ProcessLogger): Future[Int] = {
    // Assumes that `fuse run` creates outFile.o
    val compileCmd = Seq("./fuse", "run", srcFile.toString, "-o", outFile.toString)

    asyncProcRun(compileCmd).flatMap(status => {
      if (status == 0) {
        val toExec = Paths.get(outFile.toString + ".o")
        val runCmd = Seq(s"${toExec}", dataFile.toString)
        toExec.toFile.setExecutable(true)
        asyncProcRun(runCmd)
      } else {
        Future(status)
      }
    })
  }
}

class RunTests extends org.scalatest.AsyncFunSuite {

  val shouldRun = Paths.get("src/test/should-run")
  val srcFilePattern = """.*\.fuse"""

  // Create a temporary directory for generating C++ files.
  val tmpDir = Files.createDirectories(Paths.get(".").resolve("_test"))

  for (file <- Files.newDirectoryStream(shouldRun).asScala
       if file.toString.matches(srcFilePattern)) {

         test(file.toString, SlowTest) {
           val dataPath = Paths.get(file + ".data.json")
           val outFile = tmpDir.resolve(file.getFileName + ".cpp")

           // Make sure data file is present
           assert(
             Files.exists(dataPath),
             s"Data file ${dataPath} missing. Run tests require data files.")

           val stdout = new StringBuilder
           val stderr = new StringBuilder
           implicit val logger = ProcessLogger(line => stdout ++= (line + "\n"),
                                               err => stderr ++= (err + "\n"))

           AsyncRun.compileAndEval(file, dataPath, outFile).map({ case status =>
             assert(status == 0, stderr.toString)
           })
         }
  }
}
