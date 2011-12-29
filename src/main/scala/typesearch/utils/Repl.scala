package typesearch.utils

import typesearch.query.{QueryLexer, QueryParser}
import typesearch.dump.TypeParser
import dispatch.json.Js

object Repl {
  
  import scala.tools.nsc.interpreter.{JLineReader, NoCompletion}
  val reader = new JLineReader(NoCompletion)
  
  def run(f: String => Unit, display: String): Unit = {
    val input = Option(reader.readOneLine("-----" + display + "> "))
    input match {
      case None => ()
      case Some(":quit") => ()
      case Some(in) => {
        f(in)
        run(f, display)
        
      }
    }
  }
  
  def run[A](f: (String, A) => Unit, extra: A, display: String): Unit = {
    val input = Option(reader.readOneLine("-----" + display + "> "))
    input match {
      case None => ()
      case Some(":quit") => ()
      case Some(in) => {
        f(in, extra)
        run(f, extra, display)
        
      }
    }
  }
}

object ReplUses {
  
  def dumpnsearch(in: String, sigs: List[typesearch.model.Signature]): Unit = {
    ()
  }
  
  def search(in: String): Unit = {
    import typesearch.search.{Searcher, SearchState, Edits}
    
    val json = io.Source.fromFile("typesearch.json").getLines.mkString
    val sigs = Js(json)
        try{
          val r = Searcher.ParseQ.parseQ(in)
          val ss = new SearchState(r, Edits.defaultEdits:_*)
          //ss.results.take(50) foreach(x=>println(x.getOrElse("No More Queries").toString))
          
        } catch {
          case _ => println("Error parsing")
        }
  }
  
  def testQueryParser(in: String): Unit = {
    val qp = new QueryParser
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
  }
  
  def edits(in: String): Unit = {
    import typesearch.search.{Searcher, SearchState, Edits}

    val qp = new QueryParser
    try{
      val r = Searcher.ParseQ.parseQ(in)
      val ss = new SearchState(r, Edits.defaultEdits:_*)
      ss.results.take(50) foreach(x=>println(x.getOrElse("No More Queries").toString))
    } catch {
      case _ => println("Error parsing")
    }
  }
  
  def testTypeParser(in: String): Unit = {
    val tp = new TypeParser
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
  }
}