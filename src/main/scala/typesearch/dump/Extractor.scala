package typesearch.dump

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable

import typesearch.model._

object Extractor {
  
  val seen = mutable.HashSet.empty[model.MemberEntity]
  
  var packages = List(): List[Package]
  var typeDefs = List(): List[TypeDef]
  
  def extract(docModel: Universe) = {
    val dte = flatten(docModel.rootPackage)
    dte foreach (item => item match {
      //this wastes space since it just creates objects when it needs it instead of using existing ones
      case p: model.Package => packages = ChildPackage(createPackage(p.toRoot), p.name) :: packages
      case c: model.Class => typeDefs = Class(c.name, List(), TypeVar("NOT A TYPEVAR - FILLER")) :: typeDefs
      case t: model.Trait => typeDefs = Trait(t.name, List(), TypeVar("NOT A TYPEVAR - FILLER")) :: typeDefs
      case o: model.Object => typeDefs = Object(o.name, List(), TypeVar("NOT A TYPEVAR - FILLER")) :: typeDefs
      case d: model.Def => ()
      case v: model.Val => ()
      case _ => ()
    })
    
    typeDefs foreach println
    
    println("dte length: " + dte.length)
    
    println("packages: " + typeDefs.length)
  }
  
  def createPackage(pack: List[model.Package]): Package = {
    pack.head.toString() match {
      case "_root_" => RootPackage
      case p => ChildPackage(createPackage(pack.tail), pack.head.name)
    } 
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