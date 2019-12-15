class Rational(n: Int, d: Int) extends Ordered[Rational] {
  require(d != 0)
  private val g = greatestCommonDenominator(n, d)
  val numerator = n / g
  val denominator = d / g

  def this(n: Int) = this(n, 1)

  def +(that: Rational) = new Rational(
    numerator * that.denominator + that.numerator * denominator,
    denominator * that.denominator
  )

  def +(that: Int) = new Rational(
    denominator * that + numerator,
    denominator
  )

  def *(that: Rational) = new Rational(
    this.numerator * that.numerator,
    this.denominator * that.denominator
  )

  private def greatestCommonDenominator(a: Int, b: Int): Int =
    if (b == 0) a else greatestCommonDenominator(b, a % b)

  def compare(that: Rational) = {

    import that.{numerator => numer, denominator => denom}

    (this.numerator * denom) - (numer * this.denominator)
  }

  override def toString(): String = s"$numerator/$denominator"

}
