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
  as.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))
}
