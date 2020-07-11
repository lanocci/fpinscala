trait Stream[+A] {

  //def toListRecursive: List[A] = this match {
  //  case Empty => Nil
  //  case Cons(h: () => A, t: () => Stream[A]) => h() :: t.toListRecursive
  //}

  //def toList: List[A] = {
  //  @scala.annotation.tailrec
  //  def go(s: Stream[A], acc: List[A]): List[A] = {
  //    s match {
  //      case Empty => acc
  //      case Cons(h: () => A, t: () => Stream[A]) => go(t, h() :: acc)
  //    }
  //  }
  //  go(this, Nil).reverse
  //}

  //def toListFaster: List[A] = {
  //  var buf = collection.mutable.ListBuffer[A]
  //  @scala.annotation.tailrec
  //  def go(s: Stream[A]): List[A] = s match {
  //    case Cons(h, t) =>
  //      buf += h()
  //      go(t())
  //    case _ => buf.toList
  //  }
  //  go(this)
  //}

  def take(n: Int): Stream[A] = this match{
    case Cons(_, t) if n > 0 => t().take(n - 1)
    case Cons(h, _) if n == 0 => Stream.cons[A](h(), Stream.empty[A])
    case Empty => Stream.empty[A]
  }
  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(hd, _), 1) => Some(hd(), (Stream.empty[A], 0))
    case (Cons(hd, tl), n) if n > 0 => Some(hd(), (tl(), n -1))
   case (Empty, _) => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B ={
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty)((a, b)  => if(p(a)) Stream.cons(a, b) else Stream.empty)

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(hd, tl) => Some((f(hd()), tl()))
    case Empty => None
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty)((a, b) => if(f(a)) Stream.cons(a, b) else b)

  def append[B >: A](ss: => Stream[B]): Stream[B] = foldRight(ss)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def zipWith[B, C](s2: Stream[B])(f: ((A, B)) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s2) {
    case (Empty, Empty) => None
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  def startsWith(s: Stream[A]): Boolean = {
    this.zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h1, h2) => h1 == h2
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some(s, s.drop(1))
  }.append(Stream.empty)

  def hasSubsequence(s: Stream[A]): Boolean = this.tails.exists(_.startsWith(s))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

//def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
def constant[A](a: A): Stream[A] = {
  lazy val tail = Cons(() => a, () => tail)
  tail
}

def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

//def fibs(prev: Int = 0, current: Int = 0): Stream[Int] = Stream.cons(0, fibs(current, current + prev))
def fibs: Stream[Int] = {
  def go(f0: Int, f1: Int): Stream[Int] = {
    Stream.cons(f0, go(f1, f1 + f0))
  }
  go(0, 1)
}

// 初期状態z: S
// 状態を表す S を使って f関数に渡し、次の状態と今回の結果を返す
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => Stream.empty[A]
  }
}

def fibsViaUnfold: Stream[Int] = {
  unfold((0, 1)){case (f0, f1) => Some(f0, (f1, f0 + f1))}
}

def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))

def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(c => Some(c, c))

def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

