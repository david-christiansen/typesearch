package typesearch.model

sealed abstract class Package {
  def name: String
  def path: List[Package]
}
object Package {
  def apply(path: List[String]): Package =
    path.foldLeft[Package](RootPackage)(ChildPackage(_, _))

  def apply(path: String): Package =
    if (path == RootPackage.name) RootPackage
    else path.split("\\.").toList match {
      case root :: rest if root == RootPackage.name => Package(rest)
      case Nil => sys.error("Package must have name")
      case ps => Package(ps)
    }
}

case object RootPackage extends Package {
  final val name = "_root_"
  override def toString = name
  def path = List(this)
}
case class ChildPackage(parent: Package, name: String) extends Package {
  override def toString = path map (_.name) mkString "."
  def path = parent.path :+ this
}

sealed abstract class Variance
case object Invariant extends Variance
case object Covariant extends Variance
case object Contravariant extends Variance

trait Type
case class NamedType(pkg: Package, name: String) extends Type
case class TypeVar(name: String) extends Type
case class TypeApp(t1: Type, args: List[Type]) extends Type
case class TypeConstr(pkg: Package, name: String, args: List[String]) extends Type
case class StructType(members: List[Signature]) extends Type
case class TypeProjection(from: Type, name: String) extends Type
//Base types
case object AnyT extends Type
case object NothingT extends Type
case class Func(t1: Type, t2: Type) extends Type
case class Tuple(ts: List[Type]) extends Type

trait Signature {
  val name: String
  val returnType: Type
}
case class ValSig (name: String, returnType: Type) extends Signature
case class LazyValSig (name: String, returnType: Type) extends Signature
case class DefSig (name: String, args: List[List[(String, Type)]], returnType: Type) extends Signature
case class VarSig (name: String, returnType: Type) extends Signature
