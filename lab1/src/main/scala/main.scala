import randomized.Primality

object Main {

  import randomized.Primality.{ProbablyPrime, Composite}

  def main(args: Array[String]) {
    for(i<- 4 to 4000) {
      val s = Primality.solovayStrassen(i)
      val m = Primality.millerRabin(i)
      (s,m) match {
        case (ProbablyPrime, Composite(i)) => println("Strassen was wrong..")
        case (Composite(i), ProbablyPrime) => println("Miller was wrong..")
        case _ =>
      }
    }
  }
}