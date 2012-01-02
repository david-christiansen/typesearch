package typesearch.model

object ModelSerialization {
  import akka.serialization._
  import akka.serialization.Serializable.ScalaJSON
  import akka.serialization.JsonSerialization._
  import akka.serialization.DefaultProtocol._
  import dispatch.json.{JsString, JsObject}

  implicit object PackageFormat extends sjson.json.Format[Package] {
    def reads(json: JsValue): Package = json match {
      case JsString(str) => Package(str)
      case _ => throw new RuntimeException("String with path expected")
    }
    def writes(p: Package): JsValue = JsString(p.path.map(_.name) mkString ".")
  }


  object FormatterHelpers {
    implicit def str2JsonStr(str: String) = JsString(str)

    def buildObject(typ: String, props: (String, JsValue)*) =
      JsObject( (JsString("type") -> JsString(typ)) +: props.map {case (str, v) => JsString(str) -> v})
  }

  implicit object TypeFormat extends sjson.json.Format[Type] {
    import FormatterHelpers._

    def reads(json: JsValue): Type = json match {
      case JsObject(obj) => obj("type") match {
        case JsString("NamedType") => {
          val name = fromjson[String](obj("name"))
          val pkg = fromjson[Package](obj("pkg"))
          NamedType(pkg, name)
        }
        case JsString("TypeVar") => {
          val name = fromjson[String](obj("name"))
          TypeVar(name)
        }
        case JsString("TypeApp") => {
          val tOp = fromjson[Type](obj("tOp"))
          val args = fromjson[List[Type]](obj("args"))
          TypeApp(tOp, args)
        }
        case JsString("TypeConstr") => {
          val pkg = fromjson[Package](obj("pkg"))
          val name = fromjson[String](obj("name"))
          val args = fromjson[List[String]](obj("args"))
          TypeConstr(pkg, name, args)
        }
        case JsString("StructType") => {
          //Wtf? Why didn't implicit search find the arg?
          val members = fromjson[List[Signature]](obj("members"))(DefaultProtocol.listFormat[Signature](SignatureFormat))
          StructType(members)
        }
        case JsString("TypeProjection") => {
          val from = fromjson[Type](obj("from"))
          val name = fromjson[String](obj("name"))
          TypeProjection(from, name)
        }
        case JsString("TraitComposition") => {
          val base = fromjson[Type](obj("base"))
          val types = fromjson[List[Type]](obj("types"))
          TraitComposition(base, types)
        }
        case JsString("Wildcard") => Wildcard
        case JsString("AnyT") => AnyT
        case JsString("NothingT") => NothingT
        case JsString("UnitT") => UnitT
        case JsString("Func") => {
          val args = fromjson[List[Type]](obj("args"))
          val result = fromjson[Type](obj("result"))
          Func(args, result)
        }
        case JsString("Tuple") => {
          val ts = fromjson[List[Type]](obj("ts"))
          Tuple(ts)
        }
        case _ => throw new RuntimeException("Not a valid type")
      }
      case _ =>  throw new RuntimeException("Not a valid type")
    }

    def writes(t: Type): JsValue = t match {
      case NamedType(pkg, name) =>
        buildObject("NamedType", "name" -> tojson(name), "pkg" -> tojson(pkg))
      case TypeVar(name) =>
        buildObject("TypeVar", "name" -> tojson(name))
      case TypeApp(tOp, args) =>
        buildObject("TypeApp", "tOp" -> tojson(tOp), "args" -> tojson(args))
      case TypeConstr(pkg, name, args) =>
        buildObject("TypeConstr", "pkg" -> tojson(pkg), "name" -> tojson(name), "args" -> tojson(args))
      case StructType(members) =>
        buildObject("StructType", "members" -> tojson(members))
      case TypeProjection(from, name) =>
        buildObject("TypeProjection", "from" -> tojson(from), "name" -> tojson(name))
      case TraitComposition(base, types) =>
        buildObject("TraitComposition", "base" -> tojson(base), "types" -> tojson(types))
      case Wildcard => buildObject("Wildcard")
      case AnyT => buildObject("AnyT")
      case NothingT => buildObject("NothingT")
      case UnitT => buildObject("UnitT")
      case Func(args, result) =>
        buildObject("Func", "args" -> tojson[List[Type]](args), "result" -> tojson[Type](result))
      case Tuple(ts) => buildObject("Tuple", "ts" -> tojson(ts))
    }

  }

  implicit object KindFormat extends sjson.json.Format[Kind] {
    import FormatterHelpers._
    def reads(json: JsValue): Kind = json match {
      case JsObject(obj) => obj("type") match {
        case JsString("TKind") => TKind(fromjson[Type](obj("lower")), fromjson[Type](obj("upper")))
        case JsString("ArrKind") => ArrKind(reads(obj("k1")), reads(obj("k2")))
      }
    }

    def writes(k: Kind): JsValue = k match {
      case TKind(lower, upper) => buildObject("TKind", "lower" -> tojson(lower), "upper" -> tojson("upper"))
      case ArrKind(k1, k2) => buildObject("ArrKind", "k1" -> writes(k1), "k2" -> writes(k2))
    }
  }

  implicit val typeArgFormat: sjson.json.Format[TypeArg] = asProduct2("name", "kind")(TypeArg)(TypeArg.unapply(_).get)(DefaultProtocol.StringFormat, KindFormat)

  implicit object TypeDefFormat extends sjson.json.Format[TypeDef] {
    import FormatterHelpers._

    def typedef(obj: JsObject): (String, List[TypeArg], Type, Package) = obj match {
      case JsObject(m) =>
        (fromjson[String](m("name")), fromjson[List[TypeArg]](m("typeArgs")), fromjson[Type](m("extends")), fromjson[Package](m("inPackage")))
    }
    def reads(json: JsValue): TypeDef = json match {
      case td@JsObject(obj) => {
        val (name, typeArgs, extending, inPackage) = typedef(td)
        obj("type") match {
          case JsString("Class") => Class(name, typeArgs, extending, inPackage)
          case JsString("Trait") => Trait(name, typeArgs, extending, inPackage)
          case JsString("Object") => Object(name, extending, inPackage) ensuring (typeArgs isEmpty)
        }
      }
    }

    def buildTypeDef(typ: String, td: TypeDef): JsValue =
      buildObject(typ,
          "name" -> tojson(td.name),
          "typeArgs" -> tojson(td.typeArgs),
          "extending" -> tojson(td.extending),
          "inPackage" -> tojson(td.inPackage))

    def writes(td: TypeDef): JsValue = td match {
      case td: Class => buildTypeDef("Class", td)
      case td: Trait => buildTypeDef("Trait", td)
      case td: Object => buildTypeDef("Object", td)
    }
  }

  implicit object SignatureFormat extends sjson.json.Format[Signature] {
    import FormatterHelpers._

    def signature(obj: JsObject): (String, Type, TypeDef) = obj match {
      case JsObject(m) => (fromjson[String](m("name")), fromjson[Type](m("type")), fromjson[TypeDef](m("definedOn")))
    }

    def reads(json: JsValue): Signature = json match {
      case sig@JsObject(obj) =>
        val (name, returnType, definedIn) = signature(sig)
        obj("type") match {
          case JsString("ValSig") => ValSig(name, returnType, definedIn)
          case JsString("VarSig") => VarSig(name, returnType, definedIn)
          case JsString("LazyValSig") => LazyValSig(name, returnType, definedIn)
          case JsString("DefSig") => {
            val args = fromjson[List[List[MethodArg]]](obj("args"))
            DefSig(name, args, returnType, definedIn)
          }
          case _ => throw new RuntimeException("Not a valid signature type")
        }
      case _ => throw new RuntimeException("Not a valid signature")
    }

    def buildSig(typ: String, name: String, returnType: Type, definedOn: TypeDef, args: Option[List[List[MethodArg]]] = None): JsValue =
      args match {
        case Some(a) => buildObject(typ, "name" -> tojson(name), "returnType" -> tojson(returnType), "definedOn" -> tojson(definedOn), "args" -> tojson(a))
        case None => buildObject(typ, "name" -> tojson(name), "returnType" -> tojson(returnType), "definedOn" -> tojson(definedOn))
      }

    def writes(sig: Signature): JsValue = sig match {
      case DefSig(name, args, returnType, definedOn) => buildSig("DefSig", name, returnType, definedOn, Some(args))
      case ValSig(name, returnType, definedOn) => buildSig("ValSig", name, returnType, definedOn)
      case VarSig(name, returnType, definedOn) => buildSig("VarSig", name, returnType, definedOn)
      case LazyValSig(name, returnType, definedOn) => buildSig("LazyValSig", name, returnType, definedOn)
    }
  }
implicit val MethodArgFormat: sjson.json.Format[MethodArg] = asProduct3("name", "typ", "byName")(MethodArg)(MethodArg.unapply(_).get)(DefaultProtocol.StringFormat, TypeFormat, DefaultProtocol.BooleanFormat)
}



