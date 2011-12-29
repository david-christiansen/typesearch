package typesearch.model

import org.scalacheck._
import org.scalacheck.Prop.forAll

trait ModelGenerators {
  import Arbitrary.arbitrary
  import Gen._

  val genRootPackage: Gen[Package] = RootPackage

  def genChildPackage: Gen[Package] =
    for (name <- identifier; parent <- genPackage)
    yield ChildPackage(parent, name)

  val genPackage: Gen[Package] =
    Gen.frequency(5 -> genChildPackage, 1 -> genRootPackage)

  implicit def arbPackage: Arbitrary[Package] = Arbitrary(genPackage)

  implicit def arbType: Arbitrary[Type] = Arbitrary {
      import Arbitrary._
      val genNamedType: Gen[Type] =
        for (pkg <- genPackage; id <- identifier) yield NamedType(pkg, id)

      val genTypeVar: Gen[Type] = for (id <- identifier) yield TypeVar(id)

      val genLeafType = frequency(10 -> genNamedType, 3 -> genTypeVar, 1 -> AnyT, 1 -> NothingT, 2 -> UnitT)

      def genTypeConstr(argCount: Int) = for {
        pack <- arbitrary[Package]
        name <- identifier
        args <- listOfN(argCount, identifier)
      } yield TypeConstr(pack, name, args)

      def genTypeProjection(sz: Int): Gen[Type] = for {
        t <- sizedType(sz - 1)
        id <- identifier
      } yield TypeProjection(t, id)

      def genTypeApp(sz: Int): Gen[Type] = for {
        argCount <- choose(1, sz/2)
        t <- genTypeConstr(argCount)
        args <- listOfN(argCount, sizedType(sz/2))
      } yield TypeApp(t, args)

      def genTraitComposition(sz: Int): Gen[Type] = for {
        howMany <- choose(1, sz/4)
        base <- sizedType(sz - 1)
        traits <- listOfN(howMany, sizedType(sz/howMany))
      } yield TraitComposition(base, traits)

      def genTuple(sz: Int): Gen[Type] = for {
        howMany <- choose(1, sz/4)
        ts <- listOfN(howMany, sizedType(sz / howMany))
      } yield Tuple(ts)

      def genFunc(sz: Int): Gen[Type] = for {
        t1sz <- choose(1, sz)
        t2sz = sz - t1sz
        t1 <- sizedType(t1sz)
        t2 <- sizedType(t2sz)
      } yield Func(t1, t2)

      def genCompoundType(sz: Int) =
        oneOf(
            genTypeProjection(sz),
            genTypeApp(sz),
            genTraitComposition(sz),
            genTuple(sz),
            genFunc(sz)
        )

      def sizedType(sz: Int) =
        if (sz <= 0) genLeafType
        else Gen.frequency(1 -> genLeafType, 3 -> genCompoundType(sz))

      Gen.sized(sz => sizedType(sz))
    }



}

object ModelSerializationSpecification extends Properties("ModelSerialization") with ModelGenerators{
  import akka.serialization.DefaultProtocol._
  import sjson.json.JsonSerialization._
  import ModelSerialization._


  property("serialized/deserialized preserves equality") = forAll { p: Package =>
      p == fromjson[Package](tojson[Package](p))
    }

  property("serialized/deserialized preserves equality") = forAll { t: Type =>
      t == fromjson[Type](tojson[Type](t))
    }
}

object runSpecs extends App {
  ModelSerializationSpecification.check
}