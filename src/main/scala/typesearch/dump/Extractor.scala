package typesearch.dump

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable
import typesearch.model._
import scala.tools.nsc.doc.model.ValueParam

object Extractor {
  
  val seen = mutable.HashSet.empty[model.MemberEntity]

  var sigs = List(): List[Signature]
  
  def extract(docModel: Universe): List[Signature] = {
    val dte = flatten(docModel.rootPackage)
    dte foreach (item => item match {
      case d: model.Def => sigs = DefSig(d.name, createArgs(d.valueParams), createType(d.resultType), createTypeDef(d.inTemplate)) :: sigs
      case v: model.Val if v.isVal => sigs = ValSig(v.name, createType(v.resultType), createTypeDef(v.inTemplate)) :: sigs
      case v: model.Val if v.isVar => sigs = VarSig(v.name, createType(v.resultType), createTypeDef(v.inTemplate)) :: sigs
      case v: model.Val if v.isLazyVal => sigs = LazyValSig(v.name, createType(v.resultType), createTypeDef(v.inTemplate)) :: sigs
      case _ => ()
    })
    sigs
  }
  
  //FIXME: Might have to check which type of entity you have
  def createPackage(pack: List[model.DocTemplateEntity]): Package = {
    pack.head.toString() match {
      case "_root_" => RootPackage
      case p => ChildPackage(createPackage(pack.tail), pack.head.name)
    } 
  }
  
  def createTypeDef(dte: model.DocTemplateEntity): TypeDef = {
    dte match {
      case c: model.Class => Class(c.name, createTypeArgs(c.typeParams), c.parentType.map(a => createType(a)).getOrElse(AnyT), createPackage(c.toRoot))
      case t: model.Trait => Trait(t.name, createTypeArgs(t.typeParams), t.parentType.map(a => createType(a)).getOrElse(AnyT), createPackage(t.toRoot))
      case o: model.Object => Object(o.name, o.parentType.map(a => createType(a)).getOrElse(AnyT), createPackage(o.toRoot))
    }
  }
  
  def createTypeArgs(tps: List[model.TypeParam]): List[TypeArg] = {
    val res = tps map (tp => TypeArg(tp.name, createKind(tp)))
    res foreach(x => println(x.name + ": " + x.kind))
    res
  }
  
  def createKind(param: model.HigherKinded): Kind = {
    param.typeParams match {
      case Nil => TKind()
      case tps => tps.foldLeft[Kind](TKind()) { 
        (k1: Kind, k2: model.HigherKinded) => 
          ArrKind(createKind(k2), k1) 
        }
    }
  }
  
  //FIXME - undefined
  def createType(te: model.TypeEntity): Type = {
    TypeVar("NOT A TYPEVAR- FILLER")
  }
  
  def createArgs(argLists: List[List[model.ValueParam]]): List[List[(String, Type)]] = {
    argLists map(argList => argList map(arg => (arg.name, createType(arg.resultType))))
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