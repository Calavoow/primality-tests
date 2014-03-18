import randomized.Primality._

object Main {

  import randomized._

  def main(args: Array[String]) {
    Testing.accuracyTest
//    for(i<- 4 to 1000) {
//      val s = Primality.test(i, 2, solovayStrassenTest)
//      val m = Primality.test(i, 2, millerRabinTest)
//      val a = AKS(i)
//      (s,m,a) match {
//        case (ProbablyPrime, _, Composite) => print("F")
//        case (_, ProbablyPrime, Composite) => print("F")
//        case (Composite, _, Prime) => print("E")
//        case (_, Composite, Prime) => print("E")
//        case _ => print(".")
//      }
//    }
  }
}
