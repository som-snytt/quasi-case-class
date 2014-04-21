package psp
package ccmacro
package baseline

// Object of study
package p1 {
  case class Rational(n: Int, d: Int)
}

// Faithful source - produces identical bytecode
package p2 {
  import scala.runtime.{ AbstractFunction2, ScalaRunTime, Statics }

  class Rational(val n: Int, val d: Int) extends Product with Serializable {
    def copy(n: Int = n, d: Int = d): Rational = new Rational(n, d)
    override def productPrefix: String = "Rational"
    def productArity: Int = 2
    def productElement(index: Int): Any = index match {
      case 0 => Rational.this.n
      case 1 => Rational.this.d
      case _ => throw new IndexOutOfBoundsException(index.toString())
    }
    override def productIterator: Iterator[Any] = ScalaRunTime.typedProductIterator[Any](this)
    def canEqual(that: Any): Boolean = that.isInstanceOf[Rational]
    override def hashCode(): Int = {
      var acc: Int = 0xcafebabe // oops it's hardcoded
      acc = Statics.mix(acc, n)
      acc = Statics.mix(acc, d)
      Statics.finalizeHash(acc, 2)
    }
    override def toString(): String = ScalaRunTime._toString(this)
    override def equals(x: Any): Boolean = (this eq x.asInstanceOf[Object]) || {
      (x match {
        case _: Rational => true
        case _           => false
        }) && {
        val that = x.asInstanceOf[Rational]
        (n == that.n) && (d == that.d) && (that canEqual this)
      }
    }
  }

  object Rational extends AbstractFunction2[Int, Int, Rational] with Serializable {
    override final def toString(): String = "Rational"
    def apply(n: Int, d: Int): Rational = new Rational(n, d)
    def unapply(x: Rational): Option[(Int, Int)] = if (x eq null) None else Some((x.n, x.d))
    private def readResolve(): Object = Rational
  }
}

// Improved source
package p3 {
  import scala.runtime.{ AbstractFunction2, Statics }

  class Rational(val n: Int, val d: Int) extends Product2[Int, Int] {
    def _1 = n
    def _2 = d
    def get = this
    def isEmpty = d == 0
    def copy(n: Int = n, d: Int = d): Rational = new Rational(n, d)

    override def productPrefix: String = "Rational"
    override def productIterator: Iterator[Int] = Iterator(n, d)
    def canEqual(that: Any): Boolean = that.isInstanceOf[Rational]
    override def hashCode(): Int = {
      var acc: Int = 0xcafebabe // oops it's hardcoded
      acc = Statics.mix(acc, n)
      acc = Statics.mix(acc, d)
      Statics.finalizeHash(acc, 2)
    }
    override def toString(): String = {
      val sb = new StringBuilder
      sb append n
      sb append "/"
      sb append d
      sb.toString
    }
    override def equals(x: Any): Boolean = (this eq x.asInstanceOf[Object]) || {
      x match {
        case that: Rational => (n == that.n) && (d == that.d) && (that canEqual this)
        case _              => false
      }
    }
  }

  object Rational extends AbstractFunction2[Int, Int, Rational] with Serializable {
    private[this] val NoRational = new Rational(0, 0)
    override final val toString = "Rational"
    def apply(n: Int, d: Int): Rational = new Rational(n, d)
    def unapply(x: Rational): Rational = if (x eq null) NoRational else x
    private def readResolve(): Object = Rational
  }
}
