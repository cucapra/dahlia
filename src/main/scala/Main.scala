package fuselang

import java.nio.file.{Files, Paths}

object Main {
  case class Config(filename: String)

  val parser = new scopt.OptionParser[Config]("fuse"){

    head("fuse", "0.0.1")

    opt[String]('f', "srcFile")
      .required()
      .valueName("<file>")
      .action((x, c) => c.copy(filename = x))
      .text("srcFile containing fuse source code")
  }

  def main(args: Array[String]) = {

    parser.parse(args, Config("")) match {
      case Some(c) => {
        val prog = new String(Files.readAllBytes(Paths.get(c.filename)))
        println(Compiler.compileString(prog))
      }
      case None => {
        sys.exit(1)
      }
    }
  }
}
