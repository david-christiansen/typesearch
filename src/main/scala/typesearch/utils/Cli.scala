package typesearch.utils

import typesearch.dump.{Dumper}
import java.io.File


object Cli {
  val dumper = new Dumper

  def main(args: Array[String]): Unit = sys exit {
    if (args.isEmpty) println("No arguments provided")
    else args.head match {
      case "dump" => dump(args.tail.toList)
      case "dumpnsearch" => {
        val sigs = dump(args.tail.toList, false)
        Repl.run(ReplUses.dumpnsearch, sigs, "Search")
      }
      case "search" => Repl.run(ReplUses.search, "Search")
      case "qparse" => Repl.run(ReplUses.testQueryParser, "QParse")
      case "tparse" => Repl.run(ReplUses.testTypeParser, "TParse")
      case "edits" => Repl.run(ReplUses.edits, "Edit")
      //case "all" => all(args.tail.toList)
      case other => println("Unknown command:" + other)
    }
    0
  }

  def dump(filepaths: List[String], write: Boolean = true): List[typesearch.model.Signature] = {
    val files = filepaths map (f => new File(f))
    val sources = Locator locate files map (_.getPath)
    dumper.process(sources, write = write)
  }
}

/*
Old Query tester, here for reference when creating new query tester

object TestQueries extends QueryParser with App {
  def test():Unit = {
    print("------Query> ")
    val input = Console.readLine()
    if (input != "q") {
      val q = parse(input, query)
      q match {
        case Success(p,_) => {
          //p.asInstanceOf[Query].findMatching() foreach(x=>println("RESULT: " + x.in.obj.open_!.toString + "#" + x.name))
        }
        case _ => println("Failed to parse")
      }
      test()
    }
  }

  test()
}

*/
