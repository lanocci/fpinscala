def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
  l match {
    case Nil => z
    case h :: t => f(h, foldRight(t, z)(f))
  }
}

@annotation.tailrec
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
  l match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }
}

def foldLeftViaFoldRight[A, B](as: List[A], outerIdent: B)(combiner: (B, A) => B): B = {

  // 評価を遅延させて、関数をネストすることで適用順序を逆転させるために
  // 各操作を恒等写像(identity function) でラップするための型
  type BtoB = B => B

  // 内部のfoldRightで使うための恒等写像のインスタンスを定義
  def innerIdent: BtoB = (b: B) => b

  // リストの各要素に対して新しい遅延評価関数を生成する
  // 評価されたときにcombinerを使った評価が走る
  // 各関数はひとつ前の関数の引数となる
  def combinerDelayer: (A, BtoB) => BtoB =
    (a: A, delayFunc: BtoB) => (b:B) => delayFunc(combiner(b, a))

  def go: BtoB = foldRight(as, innerIdent)(combinerDelayer)

  go(outerIdent)

}

def foldLeftViaFoldRightAbbreviated[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b:B) => b)((a, g) => b => g(f(b, a)))(z)

// def append[A](l: List[A], as: List[A]) = foldRight(l, as)((acc, h) => acc :: h)

def addOneToEachElement(l: List[Int]): List[Int] = {
  //l match {
  //  case Nil => Nil
  //  case h :: t => (h + 1) +: addOneToEachElement(t)
  //}
  foldRight(l, Nil: List[Int])((h, t) => h + 1 :: t)
}

val list1 = List(1, 2, 3)
addOneToEachElement(list1)


def toStringList(l: List[Double]): List[String] =
  foldRight(l, Nil: List[String])((h, t) => h.toString :: t)
toStringList(List(1.0, 2.0, 4.0))

def map[A, B](as: List[A])(f: A => B) =
  foldRight(as, Nil: List[B])((h, t) => f(h) :: t)

def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A])((h, t) => if(f(h)) h :: t else t)

//def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
//  concat(map(as)(f))

//def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
//  flatMap[A, A](as)(a => if(f(a)) List(a) else Nil)

def addLists(l: List[Int], r: List[Int]): List[Int] = {
  l.flatMap(li => r.map(ri => li + ri))
}

def addPairsInList(l: List[Int], r: List[Int]): List[Int] = {
  (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (li :: lt, ri :: rt) =>  (li + ri) :: addPairsInList(lt, rt)
  }
}

def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = {
  (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (li :: lt, ri :: rt) => f(li, ri) :: zipWith[A](lt, rt)(f)
  }
}

//@annotation.tailrec
//def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
//  (sup, sub) match {
//    case (_ :: _, Nil) => true
//    case (Nil, _ :: _) => false
//    case (suph :: supt, subh :: subt) if suph == subh => hasSubsequence(supt, subt)
//  }
//}

@annotation.tailrec
def startsWith[A](l: List[A], prefix: List[A]): Boolean = {
  (l, prefix) match {
    case (_, Nil) => true
    case (h :: t, prefh :: preft) if h == prefh => startsWith(t, preft)
    case (_, _) => false
  }
}

@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  sup match {
    case Nil => false
    case _ if startsWith(sup, sub) => true
    case _ :: t => hasSubsequence(t, sub)
  }
}

val listA = List(1, 2, 3)
val listB = List(4)

hasSubsequence(listA, listB)

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](tree: Tree[A]): Int = {
  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }
}

def maximum(tree: Tree[Int]): Int = tree match {
  case Leaf(l) => l
  case Branch(l, r) => maximum(l).max(maximum(r))
}

def depth[A](tree: Tree[A]): Int = tree match {
  case Leaf(_) => 0
  case Branch(l, r) => depth(l).max(depth(r)) + 1
}

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
  case Leaf(a: A) => Leaf(f(a))
  case Branch(l: Tree[A], r: Tree[A]) => Branch(map[A, B](l)(f), map[A, B](r)(f))
}

def fold[A, B](tree: Tree[A])(fl: A => B)(fb: (B, B) => B): B = {
  case Leaf(a: A) => fl(a)
  case Branch(l: Tree[A], r: Tree[A]) => fb(fold[A, B](l)(fl)(fb),fold[A, B](r)(fl)(fb))
}

def sizeViaFold[A](tree: Tree[A]): Int = {
  fold[A, Int](tree)(_ => 1)(_ + _ + 1)
}

def maximumViaFold(tree: Tree[Int]): Int = fold[Int, Int](tree)(l => l)(_.max(_))

def depthViaFold[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 0)(_.max(_) + 1)

def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
