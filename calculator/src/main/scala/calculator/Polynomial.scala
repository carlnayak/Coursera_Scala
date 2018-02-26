package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal{
      val bVal = b()
      (bVal * bVal) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) {
        Set()
      } else {
        val sqRt = Math.sqrt(delta())
        if (sqRt == 0){
          Set((-1 * b()) / (2 * a()))
        } else {
          Set((-1 * b() + sqRt) / (2 * a()), (-1 * b() - sqRt) / (2 * a()))
        }
      }
    }
  }
}
