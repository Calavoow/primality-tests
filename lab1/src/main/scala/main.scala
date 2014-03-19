import randomized.Primality._

object Main {

  import randomized._

  def main(args: Array[String]) {
    for(i<- 3 to Math.pow(10, 9).toInt) {
      val s = Primality.test(i, 3, millerRabinTest)
      s match {
        case ProbablyPrime => println(s"$i: PRIME")
        case Composite => println(s"$i: Composite")
      }
    }
  }
}
