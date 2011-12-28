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


  trait FormatterHelpers {
    implicit def str2JsonStr(str: String) = JsString(str)

    def buildObject(typ: String, props: (String, JsValue)*) =
      JsObject( (JsString("type") -> JsString(typ)) +: props.map {case (str, v) => JsString(str) -> v})
  }

  implicit object TypeFormat extends sjson.json.Format[Type] with FormatterHelpers {
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
        case JsString("AnyT") => AnyT
        case JsString("NothingT") => NothingT
        case JsString("UnitT") => UnitT
        case JsString("Func") => {
          val t1 = fromjson[Type](obj("t1"))
          val t2 = fromjson[Type](obj("t2"))
          Func(t1, t2)
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
      case AnyT => buildObject("AnyT")
      case NothingT => buildObject("NothingT")
      case UnitT => buildObject("UnitT")
      case Func(t1, t2) =>
        buildObject("Func", "t1" -> tojson(t1), "t2" -> tojson(t2))
      case Tuple(ts) => buildObject("Tuple", "ts" -> tojson(ts))
    }

  }

  implicit object SignatureFormat extends sjson.json.Format[Signature] with FormatterHelpers {
    def signature(obj: JsObject): (String, Type) = obj match {
      case JsObject(m) => (fromjson[String](m("name")), fromjson[Type](m("type")))
    }

    def reads(json: JsValue): Signature = json match {
      case sig@JsObject(obj) =>
        val (name, returnType) = signature(sig)
        obj("type") match {
          case JsString("ValSig") => ValSig(name, returnType)
          case JsString("VarSig") => VarSig(name, returnType)
          case JsString("LazyValSig") => LazyValSig(name, returnType)
          case JsString("DefSig") => {
            val args = fromjson[List[List[(String, Type)]]](obj("args"))
            DefSig(name, args, returnType)
          }
          case _ => throw new RuntimeException("Not a valid signature type")
        }
      case _ => throw new RuntimeException("Not a valid signature")
    }

    def buildSig(typ: String, name: String, returnType: Type, args: Option[List[List[(String, Type)]]] = None): JsValue =
      args match {
        case Some(a) => buildObject(typ, "name" -> tojson(name), "returnType" -> tojson(returnType), "args" -> tojson(a))
        case None => buildObject(typ, "name" -> tojson(name), "returnType" -> tojson(returnType))
      }

    def writes(sig: Signature): JsValue = sig match {
      case DefSig(name, args, returnType) => buildSig("DefSig", name, returnType, Some(args))
      case ValSig(name, returnType) => buildSig("ValSig", name, returnType)
      case VarSig(name, returnType) => buildSig("VarSig", name, returnType)
      case LazyValSig(name, returnType) => buildSig("LazyValSig", name, returnType)
    }
  }

}

