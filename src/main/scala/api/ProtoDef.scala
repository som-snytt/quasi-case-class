package psp
package ccmacro
package api

trait ProtoDefs[U <: SymbolUniverse] {
  val u: U
  import u._

  type TypeParams  = List[Tparam]
  type ValueParams = List[Vparam]

  trait Named {
    def name: Name
  }

  trait Method extends Named {
    def name: TermName
  }

  trait Vparam extends Named {
    def name: TermName
    def tpt: Tree
    def tpe: Type
    def valDef: Tree
  }
  trait Tparam extends Named {
    def name: TypeName
    def boundsTree: Tree
    def bounds: TypeBounds
    def typeDef: TypeDef
  }

  // trait ProtoPair {
  //   def cdef: ProtoClass
  //   def mdef: ProtoModule
  // }

  trait ProtoClass extends ProtoImpl {
    def name: TypeName
    def selfType: Tree
    def tparams: TypeParams
    def vparamss: List[ValueParams]
  }
  trait ProtoModule extends ProtoImpl {
    def name: TermName
  }
  trait ProtoImpl extends Named {
    def mods: Modifiers
    def name: Name
    def parents: List[Tree]
    def body: List[Tree]
  }

  // trait ProtoBuilder {
  //   def protoPair: ProtoPair
  //   def protoOps: List[ProtoOp]
  // }

  case class ProtoBuilder(protoPair: ProtoPair, ops: List[ProtoOp]) {
    def map(f: ProtoPair => ProtoPair): ProtoBuilder = ProtoBuilder(protoPair, ops :+ ProtoOp(f))
    def result(): ProtoPair = ops.foldLeft(protoPair) { case (res, ProtoOp(f)) => f(res) }
  }
  case class ProtoOp(f: ProtoPair => ProtoPair)
  case class ProtoPair(cdef: ProtoClass, mdef: ProtoModule)

  // trait ProtoBuilder {
  //   def protoPair: ProtoPair
  //   def protoClass: ProtoClass
  //   def plusCopy(): ProtoBuilder
  //   def plusEquals(): ProtoBuilder
  //   def plusUnapply(generateIsEmpty: TermName => Tree): ProtoBuilder
  //   def plusToString(generateToString: List[TermName] => Tree): ProtoBuilder
  //   def result(): Companions
  // }
}
