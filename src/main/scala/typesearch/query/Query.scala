package typesearch.query

import typesearch.model.{Type, Signature}

sealed abstract class MemType

object Def extends MemType {
  override def toString = "def"
}

object Val extends MemType {
  override def toString = "val"
}

object Var extends MemType {
  override def toString = "var"
}

object LazyVal extends MemType {
  override def toString = "lazyval"
}

sealed abstract class QType {
  def query(): List[Type]
}

case class QTuple(elems: List[QType]) extends QType {
  override def toString = elems.mkString("(", ",",")")
  def query = List()
}

case class QFunc(args: List[QType], res: QType) extends QType {
  override def toString = args.mkString("(", ",",")") + ": " + res.toString
  def query() = List()
}

case class QTVar(name: String) extends QType {
  override def toString = name
  //FIXME: Worry about consistent assignments of vars
  def query() = List()
}

case class QTName(name: String) extends QType {
  override def toString = name
  def query() = List()
}

case class QTApp(op: QType, args: List[QType]) extends QType {
  override def toString = op.toString + args.mkString("[",",","]")
  def query() = List()
}

case object QTWildcard extends QType {
  override def toString = "_"
  def query() = List()
}

case class Query( path: Option[QPath],
                  memType: Option[MemType],
                  name: Option[String],
                  args: Option[List[List[QArg]]],
                  resultType: QType) {
  
  override def toString = {
    path.map(_.toString + "#").getOrElse("") +
    memType.getOrElse("").toString + " " +
    name.getOrElse("").toString +
    args.map(a => a.map(_.mkString("(",",",")")).mkString).getOrElse("") + ": " +
    resultType.toString
  }
  
  def findMatching(): List[Signature] = List() 
}

case class QPath(components: List[String]) {
  override def toString = components.mkString(".")
}

case class QArg(name: Option[String], typ: QType) {
  override def toString = name match {
    case Some(n) => n + ": " + typ.toString
    case None => typ.toString
  }

  def query(outer: Int, inner: Int) = List()
}


