package fuselang

import Utils._
import Syntax._
import org.scalatest.FunSuite

class SimpleTypePositive extends FunSuite {

  test("Adding sized int to static int") {
    typeCheck("decl x: bit<64>; let y = 1; x + y;")
  }

  test("Reassign bound variable") {
    val e1 = typeCheck("let x = 1; x := 2;")
    assert(e1("x").typ === TStaticInt(2))

    val e2 = typeCheck("decl y: bit<64>; let x = 1; x := y;")
    assert(e2("x").typ === TSizedInt(64))
  }

  test("Static index into array") {
    val e1 = typeCheck("decl a: bit<64>[10]; let x = a[1];")
    assert(e1("x").typ === TSizedInt(64))
    assert(e1("a").conBanks(0) === Set(0))
  }

  test("for loop") {
    val e1 = typeCheck("""
      decl a: bit<64>[10];
      for (let i = 0..10) {
        let x = a[i]
      }
    """ )
    assert(e1("a").conBanks(0) === Set(0))

    val e2 = typeCheck("""
      decl a: bit<64>[10 bank 5];
      for (let i = 0..10) unroll 5 {
        let x = a[i]
      }
    """ )
    assert(e2("a").conBanks(0) === 0.until(5).toSet)
  }

  test("refresh banks") {
    typeCheck("""
      decl a: bit<64>[10 bank 5];
      for (let i = 0..10) unroll 5 {
        let x = a[i];
        ---
        let y = a[i];
      }
    """ )
  }

}

class FileTypePositive extends FunSuite {

  import java.nio.file.{Files, Paths}
  import collection.JavaConverters._

  val shouldCompile = Paths.get("src/test/resources/should-compile")

  for (file <- Files.newDirectoryStream(shouldCompile).asScala) {
    test(file.toString) {
      val prog = new String(Files.readAllBytes(file))
      typeCheck(prog)
    }
  }

}
