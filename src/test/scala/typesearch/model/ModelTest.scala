package typesearch.model

object TestModel {

  implicit def str2pack(name: String): Package = Package(name)

  val int: Type = NamedType("scala", "Int")

  val idtype: Type = Func(TypeVar("A"), TypeVar("A"))

  val listOfInt: Type = TypeApp(
      TypeConstr("scala.collection.immutable", "List", List("A")),
      List(int)
  )

  val repo: List[Signature] =
    ValSig("x", int) ::
    ValSig("id", idtype) ::
    ValSig("xs", listOfInt) ::
    DefSig("doIt", List(List("x" -> AnyT)), UnitT) ::
    Nil
}