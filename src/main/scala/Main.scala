package fuselang

import java.nio.file.Files
import java.io.File

object Main {
  case class Config(srcfile: File)

  val parser = new scopt.OptionParser[Config]("fuse"){

    head("fuse", "0.0.1")

    arg[File]("<srcfile>")
      .required()
      .action((x, c) => c.copy(srcfile = x))
      .text("source code file")
  }

  def main(args: Array[String]) = {

    parser.parse(args, Config(null)) match {
      case Some(c) => {
        val prog = new String(Files.readAllBytes(c.srcfile.toPath()))
        println(Compiler.compileString(prog))
      }
      case None => {
        sys.exit(1)
      }
    }
  }
}
