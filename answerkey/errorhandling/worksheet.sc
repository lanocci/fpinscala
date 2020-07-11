sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(this)
  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = this.flatMap(a => if(f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] = {
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

val abs0: Option[Double] => Option[Double] = lift(math.abs)

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double
def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
}

def Try[A](a : => A): Option[A] = {
  try Some(a)
  catch { case e: Exception => None}
}

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a.flatMap(av => b.map(bv => f(av, bv)))
}

def sequence[A](as: List[Option[A]]): Option[List[A]] = {
  as.foldRight[Option[List[A]]](Some(Nil))((h, t) => map2(h, t)(_ :: _))
}

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
  as.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case l: Left[E] => l
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case l: Left[E] => l
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r: Right[A] => r
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(av) => b.map(bv => f(av, bv))
    case l: Left[E] => l
  }
  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for { av <- this; bv <- b } yield f(av, bv)

  //def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight[Either[E, List[A]]](Right(Nil))((h, t) => h.flatMap(hv => t.map(tv => hv :: tv)))

  //def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).flatMap(r => t.map(tv => r :: tv)))
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def Try[A](a: => A): Either[Exception, A] = {
  try Right(a)
  catch { case e: Exception => Left(e)}
}