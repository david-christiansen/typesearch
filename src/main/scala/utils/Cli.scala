package typesearch.utils

import typesearch.query.{QueryLexer, QueryParser}

object Cli {
  
  def main(args: Array[String]): Unit = sys exit {
    args.head match {
      //case "dump" => dump(args)
      //case "search" => search(args)
      case "qparse" => testQueryParser()
      //case "tparse" => testTypeParser(args)
      case _ => "Unknown command:" + _
    }
    0
  }
  
  def testQueryParser():Unit = {
    val qp = new QueryParser
    print("------PARSER> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new qp.lexical.Scanner(input)
      var lexed = collection.mutable.ListBuffer[qp.lexical.Token]()
      while (!scan.atEnd) {
        lexed += scan.first
        scan = scan.rest
      }
      print("Tokens: ")
      println(lexed toList)
      print("Query: ")
      println(qp.parse(input, qp.query))
      testQueryParser()
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