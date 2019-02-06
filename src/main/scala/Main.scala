package fuselang

import java.nio.file.Files
import java.io.File

import Utils.Config

object Main {

  val parser = new scopt.OptionParser[Config]("fuse"){

    head("fuse", "0.0.1")

    arg[File]("<srcfile>")
      .required()
      .action((x, c) => c.copy(srcFile = x))
      .text("source code file")

    opt[String]('n', "-name")
      .valueName("<kernel>")
      .validate(x =>
          if (x.matches("[A-Za-z0-9_]+")) success
          else failure("Kernel name should only contain alphanumerals and _"))
      .action((x, c) => c.copy(kernelName = x))
      .text("Name of the top level function. Default name is `kernel`.")
  }

  def main(args: Array[String]) = {

    parser.parse(args, Config(null)) match {
      case Some(c) => {
        val prog = new String(Files.readAllBytes(c.srcFile.toPath()))
        println(Compiler.compileString(prog, c))
      }
      case None => {
        sys.exit(1)
      }
    }
  }
}
