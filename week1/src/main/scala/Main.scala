/**
  * Created by jyothi on 15/9/17.
  */
object Main extends App {

  def abs(x: Double) = if(x > 0) x else -x

  def sqrtIter(guess: Double, x: Double): Double = {
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    abs(x - guess * guess) / x < 0.00001
  }

  def improve(guess: Double, x: Double): Double = {
    (guess + x / guess) / 2
  }

  def sqrt(x: Double): Double = {
    val initialGuess = 1
    sqrtIter(initialGuess, x)
  }

  /*println(sqrt(2))

  println(sqrt(4))

  println(sqrt(0.001))

  println(sqrt(0.1e-20))

  println(sqrt(1.0e20))

  println(sqrt(1.0e50))*/

  def factorial(x: Long): Long = {
    if(x == 0) 1
    else x * factorial(x - 1)
  }

  def factorialTR(x: Long): Long = {
    val acc = 1
    def mult(x: Long, acc: Long): Long = {
      if(x == 0) acc
      else mult(x - 1, x * acc)
    }
    mult(x, acc)
  }

  println(factorial(5))

  println(factorialTR(5))

}
