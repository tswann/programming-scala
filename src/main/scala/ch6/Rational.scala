package ch6
/*
 * Rational Number class -
 * Example from Ch6 Programming in Scala
 */
class Rational(n: Int, d: Int) {

  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  // auxiliary constructor
  def this(n: Int) = this(n, 1)

  override def toString = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def add(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def lessThan(that: Rational) = {
    this.numer * that.denom < that.numer * this.denom
  }

  def max(that: Rational) = {
    if (this.lessThan(that)) that else this
  }
}
