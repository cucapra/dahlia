package fuselang

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

import org.scalatest.FunSuite

import common._

class FileTypePositive extends FunSuite {

  val shouldCompile = Paths.get("src/test/should-compile")

  for (file <- Files.newDirectoryStream(shouldCompile).asScala) {
    test(file.toString) {
      val prog = new String(Files.readAllBytes(file))
      Compiler.compileStringWithError(prog)
    }
  }

}

class FileTypeNegative extends FunSuite {

  val shouldFail = Paths.get("src/test/should-fail")

  for (file <- Files.newDirectoryStream(shouldFail).asScala) {
    test(file.toString) {
      val prog = new String(Files.readAllBytes(file))
      assertThrows[Errors.TypeError] {
        Compiler.compileStringWithError(prog)
      }
    }
  }

}
