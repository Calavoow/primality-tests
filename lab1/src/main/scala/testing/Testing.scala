import java.io.{File, PrintWriter}
import randomized.AKS
import randomized.Primality._
import scala.io.Source

object Testing {

  // list of primes
  val primes: List[Int] = (Source.fromFile("src/main/resources/primesBelow1000.txt").getLines() map {
    _.toInt
  }).toList

  // list of composites
  val composites: List[Int] = (3 to 7919).toList filter {
    i => i % 2 != 0 && !primes.contains(i)
  }

  def accuracyTest() = {
    val runs = 1000
    println(s"Testing accuracy $runs times for all ${composites.size} composite numbers between 3 and 7919")
    val solovayResults = outcomesAlgo(solovayStrassenTest, runs)
    val millerResults = outcomesAlgo(millerRabinTest, runs)

    // Write results to CSV
    val resultWriter = new PrintWriter(new File("src/main/resources/accuracyResults.csv"))

    resultWriter.println("solovay primes, miller primes")
    (solovayResults zip millerResults).map{
      (tuple) => tuple.productIterator.mkString(",")
    }.foreach( resultWriter.println )

    resultWriter.close()
  }

  def runTimeTest() = {
    val logInput = for (i <- 0.5 to 6.0 by 0.1) yield {
      val result = Math.round(Math.pow(10, i)).toInt // They need to be whole numbers
      // Make sure they are not even.
      if (result % 2 == 0) result + 1
      else result
    }

    def calcRunTime(primeTester: Int => Outcome, runs: Int): Map[Int, (Double, Double)] = {
      // Can you do this better? Create a Map with the input to the results.
      (logInput zip logInput
        .map(timeAlgo(_, primeTester, runs))
        .map {
        (runTimes) => (mean(runTimes), sample_stddev(runTimes))
      }
        ).toMap
    }

    val runs = 100
    println(s"Testing runtime $runs times for logarithmically scaled inputs between 10^(0.5) to 10^6")
    val solovayResults = calcRunTime(solovayStrassenTest, runs)
    println(s"Solovay runTimes: $solovayResults")
    val millerResults = calcRunTime(millerRabinTest, runs)
    println(s"Miller runTimes: $millerResults")
    val aksResults = calcRunTime(AKS.apply _, runs)
    println(s"AKS runTimes: $aksResults")


    // Write results to CSV
    val resultWriter = new PrintWriter(new File("src/main/resources/runtimeResults.csv"))
    resultWriter.println("input,solovay mean,solovay stddev,miller mean, miller stddev,AKS mean, AKS stddev")
    for (input <- logInput) {
      val resultLine = input :: solovayResults(input).productIterator.toList :::
        millerResults(input).productIterator.toList :::
        aksResults(input).productIterator.toList
      resultWriter.println(resultLine.mkString(","))
    }
    resultWriter.close()
  }

  def mean[T](items: Traversable[T])(implicit num: Numeric[T]) = {
    num.toDouble(items.sum) / items.size
  }

  def sample_stddev[T](items: Traversable[T])(implicit num: Numeric[T]) = {
    val meanCalc = mean(items)
    Math.sqrt(
      (1 / (items.size.toDouble - 1)) * items.foldLeft(0.0) {
        (sum: Double, item) => sum + Math.pow(num.toDouble(item) - meanCalc, 2)
      }
    )
  }

  /**
   * Checks how often an algorithm returns probablyPrime as opposed to Composite.
   *
   * This is not suitable for testing AKS, because it does not have an error (it return either Composite or Prime).
   */
  def outcomesAlgo(algo: Int => Outcome, times: Int): Seq[Int] = {
    for (i <- 1 to times) yield {
      composites
        .map( algo )
        .foldLeft(0) {
          case (primeTimes, ProbablyPrime) => primeTimes + 1
          case (primeTimes, _) => primeTimes
        }
    }
  }

  /**
   * Returns the runtimes in nanoseconds of the algorithm for the given input, tested times number.
   */
  def timeAlgo(input: Int, algo: Int => Outcome, times: Int): Seq[Long] = {
    for (i <- 1 to times) yield {
      val startTime = System.nanoTime
      algo(input)
      System.nanoTime - startTime
    }
  }

}
