package psp

import scala.reflect.runtime.currentMirror
import scala.reflect.internal.util.Collections

package object ccmacro extends PackageTypes {

  def printResult[A](msg: String)(res: A): A = { println(s"$msg: $res") ; res }

  val ru: StaticUniverse          = scala.reflect.runtime.universe
  val rus: InternalStaticUniverse = ru.asInstanceOf[InternalStaticUniverse]

  implicit final class AnyExtensions[A](val lhs: A) extends AnyVal {
    def toRef: AnyRef = lhs.asInstanceOf[AnyRef]
  }

  implicit final class ListFilterOps[A](val xs: List[A]) extends AnyVal {
    def tfilter[B: CTag] = new CFilterOps[A, B](xs) apply (x => x)
    def cfilter[B]       = new CFilterOps[A, B](xs)
    def cfilterFlat[B]   = new CFilterFlatOps[A, B](xs)

    def cspan[B: CTag] : (List[B], List[A]) = {
      val filter = implicitly[TypeFilter[B]]
      val (ys, zs) = xs span filter.isDefinedAt
      (ys map filter, zs)
    }

    def filterType[B: CTag](p: B => Boolean): List[B] = xs collect { case x: B if p(x) => x }
  }
}
