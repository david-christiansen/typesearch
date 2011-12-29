/* scaladoc, a documentation generator for Scala
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 * @author  Geoffrey Washburn
 * 
 * Copyright Thibault Duplessis
 * https://github.com/ornicar/scalex/blob/master/core/src/main/scala/scalex/dump/Locator.scala
 * Dec 28 2011
 */

package typesearch.dump

import scala.tools.nsc._
import java.io._
import java.io.File.pathSeparator
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.FakePos
import Properties.msilLibPath

import akka.serialization.DefaultProtocol._
import sjson.json.JsonSerialization._
import typesearch.model.ModelSerialization._


class Dumper {

  val config = Dumper.Config(Map(
    "StringOps" -> "String"
  ))

  def process(files: List[String], filename: String = "typesearch.json"): Unit = {
    var reporter: ConsoleReporter = null
    val docSettings = new doc.Settings(msg => reporter.error(FakePos("scaladoc"), msg + "\n  scaladoc -help  gives more information"))
    docSettings.debug.value = false
    docSettings.bootclasspath.value = (docSettings.bootclasspath.value :: jarPaths).mkString(":")
    reporter = new ConsoleReporter(docSettings) { override def hasErrors = false }

    log("Creating universe...")
    val universe = new Compiler(reporter, docSettings) universe files

    log("Extracting functions from the model...")
    val sigs = Extractor.extract(universe)
    
    log("Creating json file...")
    val json = tojson(sigs)
    
    log("Writing result to disk...")
    val f = new File(filename)
    val p = new java.io.PrintWriter(f)
    try {
      p.write(json.toString())
    }
    finally { p.close()}
    
    
  }

  private[this] def log(message: String) {
    println("* " + message)
  }

  private[this] def jarPaths: List[String] = {

    def jarPathOfClass(className: String) =
      Class.forName(className).getProtectionDomain.getCodeSource.getLocation.getFile

    List(
      jarPathOfClass("scala.tools.nsc.Interpreter"),
      jarPathOfClass("scala.ScalaObject"))
  }
}

object Dumper {

  case class Config(aliases: Map[String, String])
}