package psp.ccmacro
package tests

import org.junit._
import org.junit.Assert._

class CaseTest {
  import Materializer._,u._

  val rationalExpected = """
class Rational(val n: Int, val d: Int) extends AnyRef with Product2[Int, Int] {
  def _1 = n;
  def _2 = d;
  def get = this;
  def isEmpty = false;
  def copy(n: Int = n, d: Int = d) = Rational(n, d);
  override def productPrefix: String = "Rational";
  override def productIterator: Iterator[Int] = Iterator(n, d);
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Rational];
  override def toString(): String = List(n, d).mkString("Rational".+("("), ", ", ")");
  override def hashCode(): Int = {
    var acc: Int = -889275714;
    acc = scala.runtime.Statics.mix(acc, n);
    acc = scala.runtime.Statics.mix(acc, d);
    scala.runtime.Statics.finalizeHash(acc, 2)
  };
  override def equals(that: Any): Boolean = this.eq(that.asInstanceOf[AnyRef]).||(that match {
    case (that @ ((_): Rational)) => n.==(that.n).&&(d.==(that.d)).&&(that.canEqual(this))
    case _ => false
  })
}
object Rational extends _root_.scala.Function2[Int, Int, Rational] {
  private[this] val NoRational: Rational = new Rational(0, 0);
  final override def toString = "Rational";
  def apply(n: Int, d: Int): Rational = new Rational(n, d);
  def unapply(x: Rational): Rational = if (x.eq(null))
    NoRational
  else
    x;
  private def readResolve(): Object = Rational
}""".trim

  @Test
  def casifyRational(): Unit = {
    val expr   = reify { class Rational(n: Int, d: Int) }
    val result = expr.casify

    assertEquals(result.toString, rationalExpected)
  }
}
