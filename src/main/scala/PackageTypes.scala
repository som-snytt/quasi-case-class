package psp

trait PackageTypes {
  type jPath                  = java.nio.file.Path
  type CTag[T]                = scala.reflect.ClassTag[T]
  type CBF[-From, -Elem, +To] = scala.collection.generic.CanBuildFrom[From, Elem, To]

  type GlobalUniverse         = scala.tools.nsc.Global
  type SymbolUniverse         = scala.reflect.internal.SymbolTable
  type MacroUniverse          = scala.reflect.macros.Universe
  type JavaUniverse           = scala.reflect.api.JavaUniverse
  type ApiUniverse            = scala.reflect.api.Universe
  type StaticUniverse         = scala.reflect.runtime.universe.type
  type InternalStaticUniverse = StaticUniverse with SymbolUniverse
}
