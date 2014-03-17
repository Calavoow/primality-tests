import randomized.Primality

object Main {

  import randomized.Primality._

  def main(args: Array[String]) {
    for(i<- 4 to 1000) {
      val s = Primality.test(i, 2, solovayStrassenTest)
      val m = Primality.test(i, 2, millerRabinTest)
      (s,m) match {
        case (ProbablyPrime, Composite) => print("S")
        case (Composite, ProbablyPrime) => print("M")
        case _ => print(".")
      }
    }
  }
}