trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG) = {
    val (i, r) = this.nextInt
    (if(i < 0) -(i + 1) else i, r)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DFEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt