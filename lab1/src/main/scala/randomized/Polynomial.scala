package randomized

import scala.annotation.tailrec

object Polynomial {
  def apply(coefs: Array[Long]): Polynomial = new Polynomial(coefs)

  def apply(c: Long, exp: Int): Polynomial = {
    val coefs = Array.fill(exp + 1)(0L)
    coefs(exp) = c

    Polynomial(coefs)
  }
}

class Polynomial(c: Array[Long]) {

  val coefs = c.take(c.lastIndexWhere({ _ != 0 })+1)

  /**
   * Exponential by squaring (O(log(n)) runtime
   * With modulo mod every iteration
   */
  def pow_mod(n: Long, mod: Int): Polynomial = {
    assert(n>0, "negative powers are not supported")

    n match {
      case 0 => Polynomial(Array(0))
      case 1 => this
      case _ if n % 2 == 0 => this.multiply(this, mod).pow_mod(n/2, mod)
      case _ => this.multiply(this.multiply(this, mod).pow_mod((n-1)/2, mod), mod)
    }
  }

  /**
   * Polynomial multiplication
   *
   * Implementation mirrors org.apache.commons.math3.analysis.polynomials
   */
  def multiply(that: Polynomial, mod: Long) = {
    val newcoefs = Array.fill(coefs.length + that.coefs.length - 1)(0L)

    for (
      i <- 0 until newcoefs.length;
      j <- Math.max(0, i + 1 - that.coefs.length) until Math.min(coefs.length, i + 1)
    ) {
      newcoefs(i) = (newcoefs(i) + (coefs(j) * that.coefs(i - j)) % mod) % mod
    }

    new Polynomial(newcoefs)
  }

  /**
   * Polynomial subtraction
   *
   * Implementation mirrors org.apache.commons.math3.analysis.polynomials
   */
  def subtract(that: Polynomial) = {
    val lowlength = Math.min(coefs.length, that.coefs.length)
    val highlength = Math.max(coefs.length, that.coefs.length)

    val newcoefs = Array.fill(highlength)(0L)
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
  def mod(n: Long) = new Polynomial(this.coefs.map {
    c => c % n
  })

  def degree = this.coefs.length - 1

  override def toString = {
    coefs.zipWithIndex.foldLeft("") {
      case (r, (c, i)) if c != 0L => r + s" + $c x^$i"
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
