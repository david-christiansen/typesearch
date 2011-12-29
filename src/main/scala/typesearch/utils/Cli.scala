package typesearch.utils

import typesearch.query.{QueryLexer, QueryParser}
import typesearch.dump.{Dumper, TypeParser}

import java.io.File

object Cli {
  val dumper = new Dumper

  def main(args: Array[String]): Unit = sys exit {
    args.head match {
      case "dump" => dump(args.tail.toList)
      case "search" => search()
      case "qparse" => testQueryParser()
      case "tparse" => testTypeParser()
      case "edits" => edits()
      case _ => "Unknown command:" + _
    }
    0
  }

  def dump(filepaths: List[String]): Unit = {
    val files = filepaths map (f => new File(f))
    val sources = Locator locate files map (_.getPath)
    dumper.process(sources)
  }

  //FIXME combine all these console like applications into one thing
  def search(): Unit = {
    import scala.tools.nsc.interpreter.{JLineReader, NoCompletion}
    
    val qp = new QueryParser
    val reader = new JLineReader(NoCompletion)
    val input = Option(reader.readOneLine("-----Search> "))
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
        val query = qp.parse(in, qp.query)
        
        search()
      }
    }
  }

  def edits(): Unit = {
    import typesearch.search.{Searcher, SearchState, Edits}
    import scala.tools.nsc.interpreter.{JLineReader, NoCompletion}
    val reader = new JLineReader(NoCompletion)

    val qp = new QueryParser
    val input = Option(reader.readOneLine("-----PARSER> "))
    input match {
      case None => ()
      case Some(":quit") => ()
      case Some(in) => {
        try{
          val r = Searcher.ParseQ.parseQ(in)
          val ss = new SearchState(r, Edits.defaultEdits:_*)
          ss.results.take(10) foreach(x=>println(x.getOrElse("No More Queries").toString))
        } catch {
          case _ => println("Error parsing")
        }
        edits()
      }
    }
  }

  
  
  def testTypeParser(): Unit = {
    import scala.tools.nsc.interpreter.{JLineReader, NoCompletion}
    val reader = new JLineReader(NoCompletion)

    val tp = new TypeParser
    val input = Option(reader.readOneLine("-----PARSER> "))
    input match {
      case None => ()
      case Some(":quit") => ()
      case Some(in) => {
        var scan = new tp.lexical.Scanner(in)
        var lexed = collection.mutable.ListBuffer[tp.lexical.Token]()
        while (!scan.atEnd) {
          lexed += scan.first
          scan = scan.rest
        }
        print("Tokens: ")
        println(lexed toList)
        print("Query: ")
        println(tp.parse(in))
        testTypeParser()
      }
    }
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