package fuselang

import java.nio.file.{Files, Paths}
import collection.JavaConverters._

import org.scalatest.FunSuite

class FileTypePositive extends FunSuite {

  val shouldCompile = Paths.get("src/test/resources/should-compile")

  for (file <- Files.newDirectoryStream(shouldCompile).asScala) {
    test(file.toString) {
      val prog = new String(Files.readAllBytes(file))
      Compiler.compileStringWithError(prog)
    }
  }

}

class FileTypeNegative extends FunSuite {

  val shouldFail = Paths.get("src/test/resources/should-fail")

  for (file <- Files.newDirectoryStream(shouldFail).asScala) {
    test(file.toString) {
      val prog = new String(Files.readAllBytes(file))
      assertThrows[Errors.TypeError] {
        Compiler.compileStringWithError(prog)
      }
    }
  }

}
