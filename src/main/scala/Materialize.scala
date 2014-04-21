package psp
package ccmacro

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

trait ToolboxOps[U <: SymbolUniverse] {
  self: MaterializeTrees[U] =>

  import u._
  val toolbox: ToolBox[u.type]

  def typecheck[A <: Tree](tree: A): A   = (toolbox typecheck tree).asInstanceOf[A]
  def untypecheck[A <: Tree](tree: A): A = (toolbox untypecheck tree).asInstanceOf[A]
  def define(tree: ImplDef): Symbol      = toolbox define tree
  def eval(t: Tree): Any                 = toolbox eval t
  def evalAs[A](t: Tree): A              = eval(t).asInstanceOf[A]
}


trait MaterializeTrees[U <: SymbolUniverse] extends UTypes[U] with ConvertTypes[U] with QuasiDefs[U] with api.ProtoDefs[U] with ToolboxOps[U] {
  import u._
  import internal.{ typeDef, defDef }
  import definitions._

  implicit def exprToTree(expr: Expr[_]): Tree = expr.tree

  def lit(x: Any): Tree = Literal(Constant(x))
  def symOf[A: WTag]    = weakTypeOf[A].typeSymbol
  def nameOf[A: WTag]   = symOf[A].name.decode

  sealed trait MaterializeInput
  object MaterializeInput {
    implicit def tupleToDefDefImpl(x: (String, Tree)): DefDefImpl       = DefDefImpl(x._1, x._2)
    implicit def tupleToDefaultForType(x: (Type, Tree)): DefaultForType = DefaultForType(x._1, x._2)
  }
  case class DefDefImpl(name: String, impl: Tree) extends MaterializeInput
  case class DefaultForType(tpe: Type, impl: Tree) extends MaterializeInput
  case class SuperArgs(values: Tree*) extends MaterializeInput

  def zeroOf(tpe: Type): Tree = tpe.typeSymbol match {
    case sym if sym == BooleanClass => q"false"
    case sym if sym == UnitClass    => q"()"
    case _ if NullTpe <:< tpe       => q"null"
    case _                          => q"throw new Exception"
  }

  def materializeClassDef(className: TypeName, classType: Type, defdefs: Map[Name, Tree], defaults: Map[Type, Tree], superArgs: List[Tree]): ClassDef = {
    val clazz = classType.typeSymbol
    var impls: Map[Name, Tree] = defdefs

    def hasImplInObject(m: Symbol): Boolean = ObjectTpe member m.name match {
      case NoSymbol => false
      case sym      => (sym.info =:= m.info) && { println("same: " + ((sym, m, sym.info, m.info))) ; true }
    }
    def needsImpl(m: Symbol): Boolean = m.isDeferred && !hasImplInObject(m)
    def emptyInit = DefDef(
      NoMods,
      nme.CONSTRUCTOR,
      Nil,
      List(Nil),
      TypeTree(),
      Block(List(Apply(gen.mkSuperInitCall, superArgs)), Literal(Constant(())))
    )
    def makeDef(m: Symbol): Tree = {
      val resultType = (m typeSignatureIn classType).finalResultType
      val rhs        = impls get m.name match {
        case Some(expr) => impls = impls - m.name ; expr
        case _          => defaults.getOrElse(resultType, zeroOf(resultType))
      }
      val proto          = defDef(m, rhs)
      val mods           = (proto.mods &~ Flag.DEFERRED) | Flag.OVERRIDE
      val vparamss       = u.mmap(proto.vparamss) { vd =>
        val sym = vd.symbol
        printResult(s"$sym: ${sym.info} typeargs=${sym.info.typeArgs.map(_.getClass)}")(
          ValDef(Modifiers(sym.flags), sym.name.toTermName, convertType(sym.info), EmptyTree)
        )
      }

      DefDef(mods, proto.name, proto.tparams, vparamss, TypeTree(resultType), rhs)
    }

    val ddefs0 = classType.declsAndMembers filter (m => m.isDeferred || (impls contains m.name)) map makeDef
    val ddefs1 = impls.keys.toList map (n => DefDef(NoMods, n.toTermName, Nil, Nil, TypeTree(), impls(n)))

    val ddefs       = (emptyInit :: ddefs0 ::: ddefs1) filterNot (_ eq EmptyTree)
    val parentTypes = if (clazz.isTrait || clazz.isInterface) List(ObjectTpe, classType) else List(classType)
    val parents     = parentTypes map (tp => convertType(tp))
    val templ       = Template(parents, noSelfType, ddefs)
    val mods        = Modifiers(clazz.flags, tpnme.EMPTY, Nil) &~ (Flag.ABSTRACT | Flag.TRAIT) & Flag.FINAL
    val tparams     = clazz.info.typeParams map typeDef

    ClassDef(mods, className, Nil, templ)
  }

  def materialize[A: WTag](name: String)(impls: MaterializeInput*): ClassDef = {
    val map1      = impls collect { case DefDefImpl(name, impl) => (TermName(name): Name, impl) } toMap
    val map2      = impls collect { case DefaultForType(tpe, impl) => (tpe, impl) } toMap
    val superArgs = impls collectFirst { case SuperArgs(args @ _*) => args.toList } getOrElse Nil
    val classType = weakTypeOf[A]

    materializeClassDef(TypeName(name), classType, map1, map2, superArgs)
  }

  def materializeInstance[A: WTag](impls: MaterializeInput*): A = {
    val tpe = weakTypeOf[A]
    val origName = tpe.typeSymbol.name.decode
    val name = TypeName(s"Synthetic_$origName")
    val cdef = materialize[A](name.toString)(impls: _*)
    println(s"Created synthetic $tpe instance with implementation:\n$cdef\n")
    val sym  = define(cdef)
    evalAs[A](q"new $sym")
  }

  def materializeProduct[A1: WTag, A2: WTag, A3: WTag, A4: WTag](v1: A1, v2: A2, v3: A3, v4: A4): Product4[A1, A2, A3, A4] = {
    val args = List(v1, v2, v3, v4).zipWithIndex map { case (v, i) => (s"_${i+1}" -> lit(v)): MaterializeInput }
    materializeInstance[Product4[A1, A2, A3, A4]](args: _*)
  }

  def adaptInterface[A: WTag]: ClassDef = materialize(nameOf[A] + "Adapter")()
}

object Materializer extends {
  val u: rus.type = rus
} with MaterializeTrees[InternalStaticUniverse] {
  val toolbox = currentMirror.mkToolBox().asInstanceOf[ToolBox[rus.type]]
}
