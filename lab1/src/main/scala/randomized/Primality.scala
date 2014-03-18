package randomized

import scala.util.Random
import scala.annotation.tailrec
import scala.Console

object Logger {
  val WARNING = 0
  val INFO = 1
  val DEBUG = 2

  val level = WARNING

  def info(s: String) = if(level>=INFO) println(Console.WHITE + s + Console.RESET)
  def debug(s: String) = if(level>=DEBUG) println(s)
}

object AKS {

  import Primality._

  def apply(n: Int): Outcome = {
    test_int_powers(n).getOrElse {
      val r = find_smallest_order(n)
      Logger.debug(s"Found smallest order $r")

      test_smallest_order(n, r).orElse({
        // step 3 and 4
        test_poly(n, r)
      }).getOrElse({
        // step 6
        Prime
      })
    }
  }

  def log2(n: Int): Double = Math.log(n)/Math.log(2)

  def test_smallest_order(n: Int, r: Int): Option[Outcome] = {
    Range(r, 1, -1).find {
      i =>
        val d = gcd(i, n)
        d > 1 && d < n
    }.flatMap {
      _ =>
      // If it found something, return Composite. Step 3
        Logger.info("Smallest order test says: Composite!")
        Some(Composite)
    }.orElse {
      if (n < r) {
        // Step 4. Should this be part of the smallest order test?
        Logger.info("Smallest order test says: prime!")
        Some(Prime)
      } else {
        None
      }
    }
  }

  def test_poly(n: Int, r: Int): Option[Outcome] = {
    val max = (Math.sqrt(totient(r)) * log2(n)).toInt
    val divider = Polynomial((-1 +: Seq.fill(r-1)(0) :+ 1).toArray)

    for(a <- 1 until max) {
      val left = Polynomial(Array(a, 1)).pow(n) // (x+a)^n // optimize!
      val right = Polynomial((a +: Seq.fill(n-1)(0) :+ 1).toArray) // (x^n + a)
      if(left.remainder(divider).mod(n).subtract(right.remainder(divider)) != Polynomial(Array(0))) {
        Logger.info("Poly test says: composite!")
        return Some(Composite)
      }
    }

    None
  }

  def totient(n: Int): Int = {
    (1 until n).count(gcd(n, _) == 1)
  }

  def find_smallest_order(n: Int): Int = {
    @tailrec
    def find_r(r: Int): Int = {
      if(gcd(n,r) == 1 && multiplicative_order(n, r) > Math.pow(log2(n), 2)) r
      else find_r(r+1)
    }

    find_r(2)
  }

  /**
   * TODO guard against infinite looping with maxk
   */
  def multiplicative_order(n: Int, r: Int): Int = {
    assert(gcd(n,r) == 1, "Multiplicative order is only defined if gcd(n,r) == 1")

    @tailrec
    def calc(k: Int): Int = {
      val done = powMod(n, k, r).toInt
      if(done == 1 || done == 0) k
      else calc(k+1)
    }

    calc(1)
  }

  /**
   * Test
   * @param n
   * @return
   */
  def test_int_powers(n: Int): Option[Outcome] = {
    for(b <- 2 to log2(n).toInt) {
      val a = Math.pow(n, 1.0/b)
      if(a.isValidInt) {
        Logger.info("Int powers test says: composite!")
        Logger.debug(s"Found $a^$b == $n")
        return Some(Composite)
      }
    }

    None
  }
}

object Primality {

  sealed trait Outcome {
    /**
     * You might want to do multiple tests.
     * This operator helps you combine test results for singled sided error outcomes on primality tests,
     */
    def &(right: => Outcome) = this match {
      case ProbablyPrime => right
      case Composite => this
      case Prime => this
    }
  }

  case object ProbablyPrime extends Outcome
  case object Composite extends Outcome
  case object Prime extends Outcome

  /**
   * Try k iterations of a given test with appropriately ranged
   * random values
   */
  def test(n: Int, k: Int = 100, f: Int => Outcome): Outcome = {
    assert(k>0, "You should atleast run the primality test once...")

    // see if we can find a certificate of compositeness
    for(i <- Range(0, k)) {
      f(n) match {
        case c@Composite => return c
        case _ =>
      }
    }

    // if the loop did not find any counter examples
    Primality.ProbablyPrime
  }

  def aksTest(n: Int): Outcome = AKS(n)

  /**
   * Using the Solovay-Strassen algorithm, try to find out whether n is a Prime or a Composite.
   */
  def solovayStrassenTest(n : Int) : Outcome = {
    if(n % 2 == 0) Composite
    else {
      // TODO: fails for n < 3
      val a = Random.nextInt(n-2) + 2 // a in [2,n-1]
      if(gcd(a,n) != 1) Composite
      else{
        val unroundedPow = powMod(a, (n-1)/2, n)
        assert(!unroundedPow.isNaN, "Input too large")
        val pow = unroundedPow.toInt

        val jac = (jacobi(a,n) + n) % n // Make sure to modulo it to a positive number (in case it's -1)
        if(jac == pow) ProbablyPrime
        else Composite
      }
    }
  }

  /**
   * Test if randomly chosen a can be used to find a
   * proof that n is composite
   */
  def millerRabinTest(n: Int): Outcome = {
    if(n % 2 == 0) Composite
    else {
      val a = Random.nextInt(n-2) + 2 // random in range [2, n-2]

      // factor n as 2^s*d
      val (s, d) = millerRabinFactors(n-1)
      var x = powMod(a, d, n).toInt

      if(x == 1 || x == n - 1) {
        ProbablyPrime
      } else {
        // try s - 1 times
        for(i <- Range(1, s)) {
          x = powMod(x, 2, n).toInt

          // we might find proof
          if(x == 1) return Composite
          // we might run out of tries
          else if(x == n-1) return ProbablyPrime
          // else try next
        }

        Composite
      }
    }
  }

  def millerRabinFactors(n: Int): (Int, Int) = {
    @tailrec
    def rec_factor(k: Int, s: Int): (Int,Int) = {
      // if k is uneven we're done
      if(k % 2 == 1) (s, k)
      // else we factor out a 2, and recurse
      else rec_factor(k/2, s+1)
    }

    rec_factor(n,0)
  }

  /**
   * Calculate the greatest common divisor.
   *
   * @param a
   * @param b
   * @return
   */
  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

  /**
   * Raise a number to the power, using module every multiplication.
   *
   * This can be used to prevent numbers from becoming too big (NaN for doubles).
   * Example: b**p % m becomes powMod(b,p,m)
   *
   * @param base The base of the power
   * @param pow The power to raise the base to
   * @param mod The modulus to use.
   * @return The number raised to the power, with the given modulus.
   */
  def powMod(base: Double, pow: Double, mod: Int) : Double = {
    @tailrec
    def powMod(base: Double, pow: Double, mod: Int, accum: Double) : Double = {
      pow match {
        case 0 => accum
        case _ => powMod(base, pow-1, mod, (base * accum) % mod)
      }
    }

    powMod(base, pow, mod, 1)
  }

  /**
   * Calculate the Jacobi symbol.
   *    ing time of this algorithm is O(k log3n), where k is the number of different values of a that we test; thus this is an efficient, polynomial-time algorithm. FFT-based multiplication can push the running time down to O(k log2n log log n log log log n) = Õ(k log2n).
   * TODO: Maybe find a better source?
   * http://cryptocode.wordpress.com/2008/08/16/jacobi-symbol/
   *
   * @param a
   * @param n
   * @return
   */
  def jacobi(a: Int, n:Int) : Int = {
    assert(n%2 == 1, "Jacobi is undefined for even n")

    if(a==0){
      if(n==1) 1
      else 0
    } else if(a==2) {
      n%8 match{
        case 1 | 7 => 1
        case 3 | 5 => -1
      }
    } else if( a>= n ) jacobi(a%n, n)
    else if( a%2 == 0 ) jacobi(2,n) * jacobi(a/2, n)
    else {
      if( a%4 == 3 && n%4 == 3) -jacobi(n,a)
      else jacobi(n,a)
    }
  }
}
