package typesearch.utils

import typesearch.query.{QueryLexer, QueryParser}
import typesearch.dump.Dumper

import java.io.File

object Cli {
  val dumper = new Dumper

  def main(args: Array[String]): Unit = sys exit {
    args.head match {
      case "dump" => dump(args.tail.toList)
      //case "search" => search(args)
      case "qparse" => testQueryParser()
      //case "tparse" => testTypeParser(args)
      case _ => "Unknown command:" + _
    }
    0
  }

  def dump(filepaths: List[String]): Unit = {
    val files = filepaths map (f => new File(f))
    val sources = Locator locate files map (_.getPath)
    dumper.process(sources)
  }

  def testQueryParser(): Unit = {
    import scala.tools.nsc.interpreter.{JLineReader, NoCompletion}
    val reader = new JLineReader(NoCompletion)

    val qp = new QueryParser
    val input = Option(reader.readOneLine("-----PARSER> "))
    input match {
      case None => ()
      case Some(":quit") => ()
      case Some(in) => {
        var scan = new qp.lexical.Scanner(in)
        var lexed = collection.mutable.ListBuffer[qp.lexical.Token]()
        while (!scan.atEnd) {
          lexed += scan.first
          scan = scan.rest
        }
        print("Tokens: ")
        println(lexed toList)
        print("Query: ")
        println(qp.parse(in, qp.query))
        testQueryParser()
      }
    }
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