import randomized.Primality

object Main{

  def main(args: Array[String]){
    println("Hello world!")
    for(i<- 1 to 50) println(Primality.solovayStrassen(4909))
  }
}