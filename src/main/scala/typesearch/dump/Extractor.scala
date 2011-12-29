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
    dte map (item => item match {
      case d: model.Def =>
        for (args <- createArgs(d.valueParams); t <- createType(d.resultType))
        yield DefSig(d.name, args, t, createTypeDef(d.inTemplate))
      case v: model.Val if v.isVal =>
        for (t <- createType(v.resultType)) yield ValSig(v.name, t, createTypeDef(v.inTemplate))
      case v: model.Val if v.isVar =>
        for (t <- createType(v.resultType)) yield VarSig(v.name, t, createTypeDef(v.inTemplate))
      case v: model.Val if v.isLazyVal =>
        for (t <- createType(v.resultType)) yield LazyValSig(v.name, t, createTypeDef(v.inTemplate))
      case _ => None
    }) collect { case Some(x) => x }
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
      case c: model.Class =>
        Class(c.name, createTypeArgs(c.typeParams), c.parentType.flatMap(a => createType(a)).getOrElse(AnyT), createPackage(c.toRoot))
      case t: model.Trait =>
        Trait(t.name, createTypeArgs(t.typeParams), t.parentType.flatMap(a => createType(a)).getOrElse(AnyT), createPackage(t.toRoot))
      case o: model.Object =>
        Object(o.name, o.parentType.flatMap(a => createType(a)).getOrElse(AnyT), createPackage(o.toRoot))
    }
  }
  
  def createTypeArgs(tps: List[model.TypeParam]): List[TypeArg] = {
    tps map (tp => TypeArg(tp.name, createKind(tp)))
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
  def createType(te: model.TypeEntity): Option[Type] = {
    val parser = new TypeParser
    parser.parse(te.name.replace("\u21d2","=>")) match {
        case parser.Success(t, _) => Some(t)
        case f@parser.Failure(msg, pos) => {println(f);None}
        case _ => None
      }
  }
  
  def createArgs(argLists: List[List[model.ValueParam]]): Option[List[List[(String, Type)]]] = {
    def processArgList(args: List[model.ValueParam]): Option[List[(String, Type)]] = {
      val parsed = args map (arg => (arg.name, createType(arg.resultType)))
      val collected = parsed collect {case (str, Some(t)) => (str, t)}
      if (collected.length == parsed.length) Some(collected) else None
    }
    
    val parsedLists = argLists map processArgList
    val collected = parsedLists collect { case Some(x) => x}
    if (parsedLists.length == collected.length) Some(collected) else None
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