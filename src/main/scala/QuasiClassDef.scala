package psp
package ccmacro

trait QuasiDefs[U <: SymbolUniverse] {
  self: MaterializeTrees[U] =>

  import u._, internal._

  trait Creators {
    def tparam(sym: Symbol): Tparam                          = TypeDefWrapper(internal typeDef sym)
    def vparam(sym: Symbol): Vparam                          = ValDefWrapper(internal valDef sym)
    def classDef(sym: Symbol, templ: Template): ProtoClass   = ClassWrapper(internal.classDef(sym, templ))
    def moduleDef(sym: Symbol, templ: Template): ProtoModule = ModuleWrapper(internal.moduleDef(sym, templ))
    def defDef(sym: Symbol): Method                          = DefWrapper(internal.defDef(sym, EmptyTree))
  }

  case class ValDefWrapper(valDef: ValDef) extends Vparam {
    def name = valDef.name
    def tpt  = valDef.tpt
    def tpe  = tpt.tpe match {
      case null => NoType
      case tpe  => tpe
    }
  }
  case class VparamDirect(name: TermName, tpe: Type) extends Vparam {
    def tpt    = convertType(tpe)
    def valDef = q"$name: $tpt"
  }

  case class TypeDefWrapper(typeDef: TypeDef) extends Tparam {
    def name       = typeDef.name
    def boundsTree = typeDef.rhs
    def bounds     = boundsTree.tpe match {
      case tp: TypeBounds => tp
      case _              => TypeBounds(definitions.NothingTpe, definitions.AnyTpe)
    }
  }
  case class TparamDirect(name: TypeName, bounds: TypeBounds) extends Tparam {
    def boundsTree = convertType(bounds)
    def typeDef    = TypeDef(Modifiers(Flag.PARAM), name, Nil, boundsTree)
  }

  case class ModuleWrapper(moduleDef: ModuleDef) extends ProtoModule {
    val ModuleDef(mods, name, Template(parents, _, body)) = moduleDef
  }
  case class DefWrapper(defDef: DefDef) extends Method {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = defDef
  }

  case class ClassWrapper(classDef: ClassDef) extends ProtoClass {
    val ClassDef(mods, name, tparams0, Template(parents, selfType, body)) = classDef

    def tparams  = tparams0 map (tp => TypeDefWrapper(tp))
    def vparamss = List(body.filterType[ValDef](_.mods.isParamAccessor) map ValDefWrapper)
  }

  trait QuasiClassDef {
    def className: TypeName
    def tparams: List[Tparam]
    def vparams: List[Vparam]
    def isEmpty: Tree
    def createEmpty(): Tree

    def primaryNames        = vparams map (_.name)
    def primaryTypes        = vparams map (_.tpe)
    def primaryTypeTrees    = vparams map (_.tpt)
    def parents: List[Tree] = List(tq"AnyRef")
    def selfType: ValDef    = noSelfType
    def stats: List[Tree]   = Nil
  }

  object QuasiClassDef {
    def tparam(name: String)                       = TparamDirect(name, TypeBounds.empty)
    def tparamHi[H: WTag](name: String)            = TparamDirect(name, TypeBounds.upper(wtag[H]))
    def tparamLoHi[L: WTag, H: WTag](name: String) = TparamDirect(name, TypeBounds(wtag[L], wtag[H]))
    def vparam[T: WTag](name: String)              = VparamDirect(name, wtag[T])

    def apply(cdef: ClassDef): QuasiClassDef = {
      val ClassDef(mods, name, tparams, Template(parents, self, stats)) = cdef
      val tparams1 = tparams map (tp => tparam(tp.name.decoded))
      val vparams1 = stats.filterType[ValDef](_.mods.isParamAccessor) map ValDefWrapper

      apply(name.decoded)(tparams1: _*)(vparams1: _*)
    }

    def apply(name: String)(tps: Tparam*)(vps: Vparam*): QuasiClassDef = new QuasiClassDef {
      def className: TypeName   = TypeName(name)
      def tparams: List[Tparam] = tps.toList
      def vparams: List[Vparam] = vps.toList
      def isEmpty: Tree         = q"false"
      def createEmpty(): Tree   = q"new $className(..${vparams map (_.tpe.zeroOf)})"
    }
  }

  object QuasiCaseClass {
    def apply(name: String): QuasiCaseClassBuilder = new QuasiCaseClassBuilder(name)
    def apply(tp: Type): QuasiCaseClassBuilder = {
      val sym = tp.typeSymbol
      val builder = apply(sym.name.decoded)
      sym.info.typeParams foreach builder.addTp
      sym.primaryConstructor.paramss.flatten foreach builder.addVp
      builder
    }
    def apply[T: WTag] : QuasiCaseClassBuilder = apply(wtag[T])

    trait Case2[A, B, C <: Product2[A, B]] extends ((A, B) => C) {
      def apply(p1: A, p2: B): C
    }

    class QuasiCaseClassBuilder(name: String) {
      var tps: List[Tparam] = Nil
      var vps: List[Vparam] = Nil

      def addTp(name: String, bounds: TypeBounds): this.type = addTp(TparamDirect(name, bounds))
      def addTp(sym: Symbol): this.type                      = addTp(TypeDefWrapper(typeDef(sym)))
      def addTp(tp: Tparam): this.type = { tps = tps :+ tp ; this }

      def addVp(sym: Symbol): this.type = addVp(ValDefWrapper(valDef(sym)))
      def addVp(p: Vparam): this.type   = { vps = vps :+ p ; this }

      def vparam[T: WTag](name: String)              = addVp(VparamDirect(name, wtag[T]))
      def tparam(name: String)                       = addTp(TparamDirect(name, TypeBounds.empty))
      def tparamHi[H: WTag](name: String)            = addTp(TparamDirect(name, TypeBounds.upper(wtag[H])))
      def tparamLoHi[L: WTag, H: WTag](name: String) = addTp(TparamDirect(name, TypeBounds(wtag[L], wtag[H])))

      def result: QuasiCaseClass = new QuasiCaseClass(QuasiClassDef(name)(tps: _*)(vps: _*))
    }
  }

  class QuasiCaseClass(val quasi: QuasiClassDef) {
    type Tparams = List[TypeDef]
    type Vparams = List[ValDef]

    def CanEqualLogic(param: TermName): Tree = q"$param.isInstanceOf[$ClassType]"
    def ToStringLogic: Tree = q"""List(..$primaryNames).mkString($ClassNameString + "(", ", ", ")")"""
    def HashCodeLogic: Tree = {
      val stats = primaryNames map (n => q"scala.runtime.Statics.mix(acc, $n)")
      val length: Int = primaryNames.length
      q"""{
        var acc: Int = 0xcafebabe // oops it's hardcoded
        ..$stats
        scala.runtime.Statics.finalizeHash(acc, $length)
      }"""
    }

    def EqualsLogic(param: TermName): Tree = {
      def sameFields = primaryNames map (n => q"$n == $param.$n")
      def canEqual   = q"$param canEqual this"
      def condition  = sameFields :+ canEqual reduceLeft ((a, b) => q"$a && $b")

      val cond1 = q"this eq $param.asInstanceOf[AnyRef]"
      val cond2 = q"""
        $param match {
          case that: $ClassType => $condition
          case _                => false
        }
      """

      q"$cond1 || $cond2"
    }

    def ClassNameString: String = quasi.className.decoded
    def ClassName: TypeName     = quasi.className
    def ObjectName: TermName    = ClassName.toTermName
    def NoClassName: TermName   = TermName(s"No$ClassName")
    def ClassType: Tree         = tq"$ClassName[..$classTypeParams]"

    def classTypeParams: Tparams = quasi.tparams map (_.typeDef)
    // def classParamss: Vparamss = (quasi.vparams map (_.valDef)) :: Nil

    def primaryAccessorNames = (1 to primaryArity).toList map (i => TermName(s"_$i"))
    def primaryAccessors     = (primaryAccessorNames, primaryNames).zipped map ((name, p) => q"def $name = $p")

    def newClassType: Tree       = q"new $ClassName(..$primaryNames)"
    def objectParent: Tree       = tq"(..$primaryTypeTrees) => $ClassType"
    def classParent: Tree        = tq"$ProductN[..$primaryTypeTrees]"
    def classParents: List[Tree] = quasi.parents :+ classParent
    def ProductN: TypeName       = TypeName("Product" + primaryArity)

    def primaryMods                  = Modifiers(Flag.PARAM | Flag.PARAMACCESSOR)
    def copyParamMods                = Modifiers(Flag.PARAM)
    def primaryIteratorType          = convertType(mkIteratorType(primaryLub))
    def primaryLub: Type             = lub(quasi.primaryTypes)
    def primaryArity: Int            = primaryParams.length
    def primaryParams: Vparams       = quasi.vparams map (vp => q"val ${vp.name}: ${vp.tpt}")
    def primaryTypeTrees             = quasi.primaryTypeTrees
    def primaryNames: List[TermName] = quasi.primaryNames
    def primaryWithDefaults: Vparams = quasi.vparams map (vp => q"val ${vp.name}: ${vp.tpt} = ${vp.name}")

    def cdef = q"""
      class $ClassName[..$classTypeParams](..$primaryParams) extends ..$classParents {
        ..$primaryAccessors

        def get = this
        def isEmpty = ${quasi.isEmpty}
        def copy(..$primaryWithDefaults) = $ObjectName(..$primaryNames)

        override def productPrefix: String                 = $ClassNameString
        override def productIterator: $primaryIteratorType = Iterator(..$primaryNames)
        override def canEqual(that: Any): Boolean          = ${CanEqualLogic("that")}
        override def toString(): String                    = $ToStringLogic
        override def hashCode(): Int                       = $HashCodeLogic
        override def equals(that: Any): Boolean            = ${EqualsLogic("that")}
      }"""

    def mdef = q"""
      object $ObjectName extends $objectParent {
        private[this] val $NoClassName: $ClassType                  = ${quasi.createEmpty()}
        override final def toString                                 = $ClassNameString
        def apply[..$classTypeParams](..$primaryParams): $ClassName = $newClassType
        def unapply[..$classTypeParams](x: $ClassType): $ClassType  = if (x eq null) $NoClassName else x
        private def readResolve(): Object                           = $ObjectName
      }"""

    def defns[A](f: (ClassDef, ModuleDef) => Tree): Tree = {
      val expr = f(cdef, mdef)
      q"""{ $cdef ; $mdef ; $expr }"""
    }

    def code(): String = showCode(cdef) + "\n" + showCode(mdef)
    def show1()        = println(code())
    def show2()        = println(Text.pp(showRaw(cdef)))
  }
}
