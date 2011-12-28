package typesearch.dump

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable

object Extractor {
  
  val seen = mutable.HashSet.empty[model.MemberEntity]
  
  def extract(docModel: Universe) = {
    val dte = flatten(docModel.rootPackage)
    dte foreach println
    
    println(dte.length)
  }
  
  def flatten(obj: model.Entity): List[model.MemberEntity] = {
    obj match {
      case dte: model.DocTemplateEntity => dte :: dte.members.flatMap(m => seenHelper(m))
      case mem: model.MemberEntity => List(mem)
      case _ => List()
    }
  }
  
  def seenHelper(obj:model.MemberEntity): List[model.MemberEntity] = {
    if (!(seen contains obj)) {seen += obj; flatten(obj)}
    else List()
  }
}