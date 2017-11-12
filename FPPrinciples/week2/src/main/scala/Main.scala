import scala.util.control.TailCalls.TailRec

/**
  * Created by jyothi on 16/9/17.
  */
object Main extends App {

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  //println(sum((x:Int) => x * x * x)(1, 3))

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  //println(product((x:Int) => x)(1, 3))

  def fact(n: Int): Int = {
    if(n == 0) 1
    else n * fact(n - 1)
  }

  def factInTermsOfProduct(n: Int): Int = product(x => x)(1, n)

  //println(fact(5))
  //println(factInTermsOfProduct(5))

  /**
    * for product and sum with tail recursion
    * @param f
    * @param a
    * @param b
    * @return
    */
  def combinedTR(f:(Int, Int) => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a, acc))
    }
    loop(a, if(f(0, 1) == 0) 1 else 0)
  }

  //println(combinedTR((x, y) => x + y)(1, 3)) //sum
  //println(combinedTR((x, y) => x * y)(1, 3)) //product

  //product and sum with same function
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, initial: Int)(a: Int, b: Int): Int = {
    if(a > b) initial
    else combine(f(a), mapReduce(f, combine, initial)(a + 1, b))
  }

  println(mapReduce(x => x, (x, y) => x * y, 1)(1, 3))
  println(mapReduce(x => x, (x, y) => x + y, 0)(1, 3))

}
