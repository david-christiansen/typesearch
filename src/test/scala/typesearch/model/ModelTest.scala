package typesearch.model

object TestModel {
  import scala.collection.mutable

  implicit def str2pack(name: String): Package = Package(name)

  val int: Type = NamedType("scala", "Int")

  val idtype: Type = Func(TypeVar("A"), TypeVar("A"))

  val listOfInt: Type = TypeApp(NamedType("scala.collection.immutable", "List"), List(int))

  val testVals: mutable.Map[String, Type] =
    mutable.Map("x" -> int, "id" -> idtype, "numbers" -> listOfInt)

}
