package typesearch.dump

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable
import typesearch.model._
import scala.tools.nsc.doc.model.ValueParam

object Extractor {
  
  val seen = mutable.HashSet.empty[model.MemberEntity]

  var defSigs = List(): List[Signature]
  //ValSig (name: String, returnType: Type, definedOn: TypeDef)
  def extract(docModel: Universe) = {
    val dte = flatten(docModel.rootPackage)
    dte foreach (item => item match {
      case d: model.Def => DefSig(d.name, createArgs(d.valueParams), createType(d.resultType), createTypeDef(d.inTemplate))
      case v: model.Val if v.isVal => ValSig(v.name, createType(v.resultType), createTypeDef(v.inTemplate))
      case v: model.Val if v.isVar => VarSig(v.name, createType(v.resultType), createTypeDef(v.inTemplate))
      case v: model.Val if v.isLazyVal => LazyValSig(v.name, createType(v.resultType), createTypeDef(v.inTemplate))
      case _ => ()
    })
    
    println("dte length: " + dte.length)
  }
  
  //FIXME: Might have to check which type of entity you have
  def createPackage(pack: List[model.DocTemplateEntity]): Package = {
    pack.head.toString() match {
      case "_root_" => RootPackage
      case p => ChildPackage(createPackage(pack.tail), pack.head.name)
    } 
  }
  
  def createTypeDef(dte: model.DocTemplateEntity): TypeDef = {
    //Class (name: String, typeArgs: List[TypeArg], extending: Type, inPackage: Package)
    dte match {
      case c: model.Class => Class(c.name, createTypeArgs(), createTypeVar(), createPackage(c.toRoot))
      case t: model.Trait => Trait(t.name, createTypeArgs(), createTypeVar(), createPackage(t.toRoot))
      case o: model.Object => Object(o.name, createTypeVar(), createPackage(o.toRoot))
    }
  }
  
  //FIXME - undefined
  def createTypeArgs(): List[TypeArg] = {
    //TypeArg (name: String, kind: Kind = TKind())
    List(TypeArg("TYPEARG NAME", createKind()))
  }
  
  //FIXME - undefined
  def createKind(): Kind = {
    TKind()
  }
  
  //FIXME - undefined
  def createTypeVar(): TypeVar = {
    TypeVar("NOT A TYPEVAR - FILLER")
  }
  
  //FIXME - undefined
  def createType(te: model.TypeEntity): Type = {
    TypeVar("NOT A TYPEVAR- FILLER")
  }
  
  //FIXME - undefined
  def createArgs(argsList: List[List[model.ValueParam]]): List[List[(String, Type)]] = {
    List(List(("NOT A NAME", TypeVar("NOT A TYPEVAR - FILLER"))))
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