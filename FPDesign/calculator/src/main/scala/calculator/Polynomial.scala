package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      if(delta() < 0 || delta().isNaN) Set.empty
      else{
        val root = Math.sqrt(delta())
        Set(
          (-b() + root) / (2 * a()),
          (-b() - root) / (2 * a())
        )
      }
    }
  }
}
