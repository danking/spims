  class ArgumentException(message: String)
    extends RuntimeException(message) { }

  class CommandLineParser(options: Seq[Opt]) {
    def parse(argv: Seq[String]) {
      for (i <- 0 until argv.length) {
        var successful = false;
        for (option <- options if !successful) {
          successful = option.maybeParse(argv, i);
        }
        if (!successful) {
          throw new ArgumentException("Argument " + argv(i) + " is not understood.")
        }
      }
    }

    def printHelpMessage() {
      println("SPIMS -- Help")
      for (option <- options) {
        println(option.descriptionString())
      }
      println();
    }
  }

  abstract class Opt(short: Option[String],
                     long: Option[String],
                     description: Option[String]) {
    def descriptionString() = {
      "  " + short.getOrElse("") +
           " " + long.getOrElse("") +
           "\n    " + description.getOrElse("")
    }
    def maybeParse(argv: Seq[String], i: Int): Boolean
  }

  class NoArgOpt(short: Option[String], long: Option[String],
                 description: Option[String],
                 handler: () => Unit)
      extends Opt(short, long, description) {

    def maybeParse(argv: Seq[String], i: Int) = {
      if ((short.isDefined && argv(i) == short.get)
          || (long.isDefined && argv(i) == long.get)) {
        handler()
        true
      } else {
        false
      }
    }

  }

  class OneArgOpt(short: Option[String], long: Option[String],
                  description: Option[String],
                  handler: String => Unit)
      extends Opt(short, long, description) {

    def maybeParse(argv: Seq[String], i: Int) = {
      if ((short.isDefined && argv(i) == short.get)
          || (long.isDefined && argv(i) == long.get)) {
        if (argv.length > i+1) {
          handler(argv(i+1))
          true
        } else {
          throw new ArgumentException("Argument " + argv(i) + " must have a parameter.")
        }
      } else {
        false
      }
    }
  }
