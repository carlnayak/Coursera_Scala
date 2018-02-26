
object Test {

  def sqrt(x: Double) = {
    def abs(x:Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double): Double = if (isGoodEnough(guess)) guess else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = abs(guess * guess - x)/ x < 0.0001

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  val t = sqrt(1e7)



  def factorialOuter(string: String, x:Int) = {
    def factorial(x: Int): Int = if (x == 0) 1 else x * factorial(x - 1)
    factorial(x)
  }

  factorialOuter("test", 8)

  def factorial(x:Int): Int = {
    def factorialTR(s: Int, x: Int): Int = if (x == 0) s else factorialTR(s * x, x - 1)
    factorialTR(1, x)
  }

  factorial(8)
}
