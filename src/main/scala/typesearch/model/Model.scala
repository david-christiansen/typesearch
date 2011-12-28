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

sealed abstract class Kind
case class TKind (lower: Type = NothingT, upper: Type = AnyT) extends Kind
case class ArrKind (k1: Kind, k2: Kind) extends Kind

case class TypeArg (name: String, kind: Kind = TKind())

trait TypeDef {
  val name: String
  val typeArgs: List[TypeArg]
  val extending: Type
}
case class Trait (name: String, typeArgs: List[TypeArg], extending: Type) extends TypeDef
case class Class (name: String, typeArgs: List[TypeArg], extending: Type) extends TypeDef
case class Object (name: String, typeArgs: List[TypeArg], extending: Type) extends TypeDef with Signature {
  val returnType = extending
}


trait HasShape {
  val shape: String
}
trait Type extends HasShape

trait IsAtom extends HasShape {
  val shape = "*"
}

case class NamedType(pkg: Package, name: String) extends Type with IsAtom
case class TypeVar(name: String) extends Type with IsAtom
case class TypeApp(t1: Type, args: List[Type]) extends Type {
  val shape = t1.shape + args.map(_.shape).mkString("[", ",", "]")
}
case class TypeConstr(pkg: Package, name: String, args: List[String]) extends Type with IsAtom
case class StructType(members: List[Signature]) extends Type with IsAtom
case class TypeProjection(from: Type, name: String) extends Type with IsAtom
case class TraitComposition(base: Type, types: List[Type]) extends Type with IsAtom

//Base types
case object AnyT extends Type with IsAtom
case object NothingT extends Type with IsAtom
case object UnitT extends Type with IsAtom
case class Func(t1: Type, t2: Type) extends Type {
  val shape = "*[" + t1.shape + "," + t2.shape + "]"
}
case class Tuple(ts: List[Type]) extends Type {
  val shape = "*" + ts.map(_.shape).mkString("[", ",", "]")
}

trait Signature {
  val name: String
  val returnType: Type
}
case class ValSig (name: String, returnType: Type) extends Signature
case class LazyValSig (name: String, returnType: Type) extends Signature
case class DefSig (name: String, args: List[List[(String, Type)]], returnType: Type) extends Signature
case class VarSig (name: String, returnType: Type) extends Signature
