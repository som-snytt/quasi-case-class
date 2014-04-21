package psp
package ccmacro

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror

trait UTypes[U <: SymbolUniverse] {
  self: MaterializeTrees[U] =>

  val u: U
  import u._
  import definitions.ObjectClass

  type TTag[T] = TypeTag[T]
  type WTag[A] = WeakTypeTag[A]

  def ttag[A: TTag] : Type = implicitly[TTag[A]].tpe
  def wtag[A: WTag] : Type = implicitly[WTag[A]].tpe

  implicit class ReplListOps[A](val xs: Traversable[A]) {
    def > : Unit = xs foreach println
  }

  implicit final class ExprOps[A](val expr: Expr[A]) {
    def trees[T: CTag] : List[T] = expr.tree collect { case t: T => t }

    def memberDefs = trees[MemberDef]
    def moduleDefs = trees[ModuleDef]
    def classDefs  = trees[ClassDef]
    def implDefs   = trees[ImplDef]

    def casify0(): QuasiCaseClass = (classDefs, implDefs) match {
      case (cdef :: Nil, mdef :: Nil) => new QuasiCaseClass(QuasiClassDef(typecheck(cdef)))
      case (cdef :: Nil, Nil)         => new QuasiCaseClass(QuasiClassDef(typecheck(cdef)))
      case _                          => null
    }

    def casify(): String = casify0() match {
      case null => ""
      case qcc  => qcc.code
    }
  }

  implicit final class TypeOps(val tpe: Type) {
    import definitions._

    def isNothing = tpe.typeSymbol == NothingClass
    def isAny     = tpe.typeSymbol == AnyClass

    def sigOf(m: Symbol): String  = m defStringSeenAs (m typeSignatureIn tpe)
    def dsigOf(m: Symbol): String = m.debugFlagString match { case "" => sigOf(m) ; case s => s + " " + sigOf(m) }

    // Have to do members and decls or you miss any deferred methods
    // which have a concrete implementation earlier in the chain (they
    // still must be reimplemented)
    // https://issues.scala-lang.org/browse/SI-8548
    def declsAndMembers: List[Symbol] = (tpe.decls.toList ++ tpe.members.toList).distinct filterNot (ObjectClass isSubClass _.owner)

    def deferredMembers = declsAndMembers filter (_.isDeferred)

    def deferredSigs = deferredMembers map dsigOf

    def memberSigs = declsAndMembers map dsigOf

    def zeroOf: Tree = if (tpe eq null) Literal(Constant(null)) else tpe.typeSymbol match {
      case s if s == definitions.BooleanClass => Literal(Constant(false))
      case s if s.isPrimitiveValueClass       => Literal(Constant(0))
      case _                                  => Literal(Constant(null))
    }
  }
  implicit final class SymbolOps(val sym: Symbol) {
    def isPackageDefined: Boolean = sym.isDefinedInPackage || sym.moduleClass.isDefinedInPackage
  }

  implicit def liftClassDef(cdef: ClassDef): ProtoClass    = ClassWrapper(cdef)
  implicit def liftModuleDef(mdef: ModuleDef): ProtoModule = ModuleWrapper(mdef)

  implicit final class TypedListFilterOps[A: TTag](val xs: List[A]) {
    // lifted from the compiler. (I wrote it.)
    def propagateKnownTypes(from: Type, to: Symbol): Type = {
      def tparams  = to.typeParams
      val tvars    = tparams map (p => TypeVar(p))
      val tvarType = appliedType(to, tvars: _*)
      val bases    = from.baseClasses filter (to.baseClasses contains _)

      bases foreach { bc =>
        val tps1 = (from baseType bc).typeArgs
        val tps2 = (tvarType baseType bc).typeArgs
        if (tps1.size != tps2.size)
          devWarning(s"Unequally sized type arg lists in propagateKnownTypes($from, $to): ($tps1, $tps2)")

        (tps1, tps2).zipped foreach (_ =:= _)
      }
      val resArgs = tparams zip tvars map {
        case (_, tvar) if tvar.instValid => tvar.constr.inst
        case (tparam, _)                 => tparam.tpeHK
      }
      appliedType(to, resArgs: _*)
    }

    def tpartition[B : CTag : TTag] : (List[B], List[A]) = {
      val elemTag  = implicitly[u.TypeTag[A]]
      val testTag  = implicitly[u.TypeTag[B]]
      val elemTpe  = elemTag.tpe
      val testTpe  = testTag.tpe

      val classTag  = implicitly[CTag[B]]
      val className = classTag.runtimeClass.getName
      val classSym  = (currentMirror staticClass className).asInstanceOf[ClassSymbol]
      val classTpe  = propagateKnownTypes(elemTpe, classSym)

      println(sm"""
        |In a collection with elements of type    $elemTpe
        |            a runtime check for class    $className
        |      allows us to infer elements are    $classTpe
        |        which means we can filter for    $testTpe
        |                   only if this holds    $classTpe <:< $testTpe
        |                and that evaluates to    ${classTpe <:< testTpe}
        |""")

      val ys = xs.tfilter[B]
      val zs = xs filterNot ys.contains
      (ys, zs)
    }
  }
}

trait ConvertTypes[U <: SymbolUniverse] {
  self: MaterializeTrees[U] =>

  import u._, internal._, treeBuild._
  import definitions.{ ObjectClass, AnyTpe, NothingTpe }

  def mkIteratorType[A: WTag] : Type = mkIteratorType(wtag[A])
  def mkIteratorType(tp: Type): Type = appliedType(wtag[Iterator[_]].typeConstructor, tp :: Nil)

  type Params  = List[Symbol]
  type Paramss = List[Params]

  val RootClass                           = rootMirror.RootClass
  def ScalaPackageObject                  = definitions.getMember(definitions.ScalaPackage, TermName("package"))
  def defaultModules: Set[Symbol]         = Set(definitions.PredefModule, definitions.ScalaPackage, ScalaPackageObject)
  def isPredefOrScalaPackage: Set[Symbol] = defaultModules ++ defaultModules.map(_.moduleClass)

  object FlatMethodType {
    def loop(method: Symbol, tparams: Params, vparamss: Paramss, restpe: Type): Option[(Symbol, Params, Paramss, Type)] = restpe match {
      case mt @ MethodType(vparams, restpe) => loop(method orElse mt.typeSymbol, tparams, vparams :: vparamss, restpe)
      case mt @ NullaryMethodType(restpe)   => Some((method orElse mt.typeSymbol, tparams, Nil, restpe))
      case _                                => Some((method, tparams, vparamss, restpe))
    }
    def unapply(tp: Type): Option[(Symbol, Params, Paramss, Type)] = tp match {
      case PolyType(tparams, mt @ (_: MethodType | _: NullaryMethodType)) => loop(NoSymbol, tparams, Nil, mt)
      case _: MethodType | _: NullaryMethodType                           => loop(NoSymbol, Nil, Nil, tp)
      case _                                                              => None
    }
  }

  private implicit class MissingAnyRefOps[A](val lhs: A) {
    def showing[B](msg: String)(f: A => B): B = printResult(s"$msg: $lhs ($shortClass)")(f(lhs))
    def debug_s: String          = s"$lhs ($shortClass)"
    def shortClass: String       = lhs.getClass.getName split "[.$]" filterNot (_ forall (_.isDigit)) last
  }

  private def mkParents(parents: List[Type]): List[Tree] = convertTypes(parents filterNot (_.typeSymbol == ObjectClass))

  def convertTypes(tps: List[Type]): List[Tree] = tps map convertType

  private def symbolModifiers(sym: Symbol): Modifiers            = Modifiers(sym.flags, tpnme.EMPTY, Nil)

  private def newClassDef(sym: Symbol) = sym match {
    case sym: ClassSymbol => classDef(sym, template(sym.info))
  }

  private def template(info: Type): Template = info match {
    case PolyType(tparams, restpe)            => template(restpe)
    case ClassInfoType(parents, decls, clazz) => Template(mkParents(parents), noSelfType, memberDefs(decls))
    case RefinedType(parents, decls)          => Template(mkParents(parents), noSelfType, memberDefs(decls))
  }
  private def memberDefs(decls: Scope): List[MemberDef] = decls.toList map memberDef
  private def memberDef(sym: Symbol): MemberDef = sym.name match {
    case n: TermName if sym.isMethod      => defDef(sym, EmptyTree)
    case n: TermName if sym.isParameter   => valDef(sym)
    case n: TermName if sym.isStaticOwner => moduleDef(sym, template(sym.info))
    case n: TypeName if sym.isClass       => classDef(sym, template(sym.info))
    case n: TypeName if sym.isAbstract    => typeDef(sym, convertType(sym.info))
    case n: TypeName                      => typeDef(sym, convertType(sym.typeSignature))
  }
  private def dropPrefix(pre: Type): Boolean = pre match {
    case NoPrefix | NoType  => true
    case _                  => isPredefOrScalaPackage(pre.typeSymbol)
  }

  private def typeRef(pre: Type, sym: Symbol): Tree = (
    if (dropPrefix(pre)) Ident(sym)
    else mkAttributedSelect(convertType(pre), sym)
  )
  def convertValueParam(pre: Type, param: Symbol): ValDef = printResult(s"convertValueParam($pre, $param)")(
    ValDef(Modifiers(param.flags), param.name.toTermName, convertType(param.tpe_*.asSeenFrom(pre, param.enclClass)), EmptyTree)
  )

  def convertTypeParams(pre: Type, tparams: List[Symbol]): List[TypeDef] = tparams map (convertTypeParam(pre, _))
  def convertTypeParam(pre: Type, tparam: Symbol): TypeDef =
    TypeDef(Modifiers(tparam.flags), tparam.name.toTypeName, tparam.typeParams map (convertTypeParam(pre, _)), convertType(tparam.info.asSeenFrom(pre, tparam.enclClass)))

  def convertDefDef(pre: Type, m: Symbol, tparams: List[Symbol], vparamss: List[List[Symbol]], restpe: Type): DefDef = printResult(s"convertDefDef($pre, $m, $tparams, $vparamss, $restpe") {
    val mods        = Modifiers(m.flags)
    val name        = m.name.toTermName
    val tparamTrees = convertTypeParams(pre, tparams)
    val vparamTrees = mmap(vparamss)(convertValueParam(pre, _))
    val tpt         = convertType(restpe)

    DefDef(mods, name, tparamTrees, vparamTrees, tpt, EmptyTree)
  }

  def convertType(pre: Type, sym: Symbol): Tree = sym match {
    case m: MethodSymbol => defDef(sym, EmptyTree)
    case _               => convertType(pre, sym typeSignatureIn pre)
  }
  def convertType(tp: Type): Tree = convertType(NoPrefix, tp)
  def convertType(pre: Type, tp: Type): Tree = {
    def recur(tp: Type): Tree = convertType(pre, tp)

    val result = tp match {
      case _ if tp ne tp.dealias                                        => convertType(tp.dealias)
      case SingleType(pre, sym) if sym.isPackage                        => Ident(sym)
      case TypeRef(ThisType(preSym), sym, Nil) if preSym.isPackageClass => Ident(sym)
      case TypeRef(pre, sym, Nil) if pre.typeSymbol.isPackageClass      => Select(convertType(pre), sym)
      case TypeRef(_, sym, Nil) if sym.isPackageDefined                 => mkAttributedRef(sym)
      case TypeRef(_, RootClass, _)                                     => Ident(RootClass)
      case NoType | NoPrefix                                            => EmptyTree
      case FlatMethodType(method, tparams, vparamss, restpe)            => convertDefDef(pre, method, tparams, vparamss, restpe)
      case PolyType(tparams, info @ ClassInfoType(_, _, clazz))         => ClassDef(Modifiers(clazz.flags), clazz.name.toTypeName, convertTypeParams(pre, tparams), template(info))
      case PolyType(tparams, info)                                      => recur(info)
      case SingleType(pre, sym)                                         => SingletonTypeTree(Ident(sym))
      case ThisType(sym)                                                => This(sym.name.toTypeName)
      case TypeBounds(lo, hi) if lo.isNothing && hi.isAny               => TypeBoundsTree(EmptyTree, EmptyTree)
      case TypeBounds(lo, hi) if lo.isNothing                           => TypeBoundsTree(EmptyTree, recur(hi))
      case TypeBounds(lo, hi) if hi.isAny                               => TypeBoundsTree(recur(lo), EmptyTree)
      case TypeBounds(lo, hi)                                           => TypeBoundsTree(recur(lo), recur(hi))
      case RefinedType(_, _)                                            => CompoundTypeTree(template(tp))
      case ExistentialType(qparams, restpe)                             => ExistentialTypeTree(recur(restpe), qparams map memberDef)
      case TypeRef(NoPrefix, sym, Nil) if tp ne tp.dealias              => memberDef(sym)
      case TypeRef(pre, sym, Nil)                                       => convertType(pre, sym)
      case TypeRef(pre, sym, args)                                      => AppliedTypeTree(recur(tp.typeConstructor), args map recur)
      case ClassInfoType(parents, decls, clazz)                         => mkAttributedRef(clazz)
    }
    result
  }

  def apply[A: WTag] : Tree = convertType(wtag[A])
  def widen(tp: Type): Tree = convertType(tp.widen)
  def widen[A: WTag] : Tree = widen(wtag[A])
}
