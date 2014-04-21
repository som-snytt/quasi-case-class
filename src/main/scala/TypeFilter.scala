package psp
package ccmacro

class IdentityTypeFilter[A](implicit tag: CTag[A]) extends TypeFilter[A] {
  def apply(x: Any)       = x.asInstanceOf[A]
  def isDefinedAt(x: Any) = (x != null) && (tag.runtimeClass isAssignableFrom x.getClass)
}
class FilteredTypeFilter[A](typeFilter: TypeFilter[A], p: A => Boolean) extends TypeFilter[A] {
  def apply(x: Any)       = typeFilter(x)
  def isDefinedAt(x: Any) = (typeFilter isDefinedAt x) && p(apply(x))
}
class MappedTypeFilter[A, B](typeFilter: TypeFilter[A], f: A => B) extends TypeFilter[B] {
  def apply(x: Any)       = f(typeFilter(x))
  def isDefinedAt(x: Any) = typeFilter isDefinedAt x
}
trait TypeFilter[A] extends PartialFunction[Any, A] {
  def isDefinedAt(x: Any): Boolean
  def apply(x: Any): A

  def filter(p: A => Boolean): TypeFilter[A]     = withFilter(p)
  def withFilter(p: A => Boolean): TypeFilter[A] = new FilteredTypeFilter[A](this, p)
  def map[B](f: A => B): TypeFilter[B]           = new MappedTypeFilter[A, B](this, f)
}
object TypeFilter {
  implicit def apply[A: CTag] = new IdentityTypeFilter[A]
}

class CFilterOps[A, B](val xs: List[A]) extends AnyVal {
  def apply[C](f: B => C)(implicit tf: TypeFilter[B]): List[C] = xs collect (tf map f)
}
class CFilterFlatOps[A, B](val xs: List[A]) extends AnyVal {
  def apply[C](f: B => Traversable[C])(implicit tag: CTag[B]): List[C] = (xs collect { case x: B => f(x) }).flatten
}
