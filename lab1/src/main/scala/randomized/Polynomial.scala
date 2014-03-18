package randomized

object Polynomial {
  def apply(coefs: Array[BigInt]): Polynomial = new Polynomial(coefs)

  def apply(coefs: Array[Int]): Polynomial = {
    Polynomial(coefs.map {
      e => BigInt(e)
    })
  }

  def apply(c: BigInt, exp: Int): Polynomial = {
    val coefs = Array.fill(exp + 1)(BigInt(0))
    coefs(exp) = c

    Polynomial(coefs)
  }
}

class Polynomial(c: Array[BigInt]) {

  val coefs = c.reverse.dropWhile { c => c == BigInt(0) }.reverse

  def pow(n: Int) = {
    (1 until n).foldLeft(this) {
      (r, _) => r.multiply(this)
    }
  }

  /**
   * Polynomial multiplication
   *
   * Implementation mirrors org.apache.commons.math3.analysis.polynomials
   */
  def multiply(that: Polynomial) = {
    val newcoefs = Array.fill(coefs.length + that.coefs.length - 1)(BigInt(0))

    for (
      i <- 0 until newcoefs.length;
      j <- Math.max(0, i + 1 - that.coefs.length) until Math.min(coefs.length, i + 1)
    ) {
      newcoefs(i) += coefs(j) * that.coefs(i - j)
    }

    new Polynomial(newcoefs.toArray)
  }

  /**
   * Polynomial subtraction
   *
   * Implementation mirrors org.apache.commons.math3.analysis.polynomials
   */
  def subtract(that: Polynomial) = {
    val lowlength = Math.min(coefs.length, that.coefs.length)
    val highlength = Math.max(coefs.length, that.coefs.length)

    val newcoefs = Array.fill(highlength)(BigInt(0))
    for (i <- 0 until lowlength) {
      newcoefs(i) = coefs(i) - that.coefs(i)
    }

    if (coefs.length < that.coefs.length) {
      for (i <- lowlength until highlength) {
        newcoefs(i) = -that.coefs(i)
      }
    } else {
      for (i <- lowlength until highlength) {
        newcoefs(i) = -this.coefs(i)
      }
    }

    Polynomial(newcoefs)
  }

  /**
   * Termwise modulo
   */
  def mod(n: Int) = new Polynomial(this.coefs.map {
    c => c % n
  })

  def degree = this.coefs.length - 1

  /**
   * Polynomial long division
   */
  def remainder(that: Polynomial): Polynomial = {
    val diff = this.degree - that.degree
    if (diff < 0) this
    else {
      val divisor = that.multiply(Polynomial(coefs(this.degree) / that.coefs(that.degree), diff))
      this.subtract(divisor).remainder(that)
    }
  }

  override def toString = {
    coefs.zipWithIndex.foldLeft("") {
      case (r, (c, i)) if c != BigInt(0) => r + s" + $c x^$i"
      case (r, _) => r
    }
  }

  override def equals(o: Any) = o match {
    case that: Polynomial => this.degree == that.degree && coefs.zip(that.coefs).forall {
      case (l, r) => l == r
    }
    case _ => false
  }

}
