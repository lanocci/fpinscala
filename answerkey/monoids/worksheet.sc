trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

val intAddition: Monoid[Int] = new Monoid[Int]{
  def op(a1: Int, a2: Int): Int = a1 + a2
  def zero: Int = 0
}

val intMultiplication: Monoid[Int] = new Monoid[Int]{
  override def op(a1: Int, a2: Int): Int = a1 * a2
  override def zero: Int = 1
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean]{
  override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  override def zero: Boolean = false
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean) = a1 && a2
  override def zero = true
}

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  override def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
  override def zero = None
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
  override def zero: A => A = (a: A) => a
}
