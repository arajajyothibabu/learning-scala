/**
  * Created by jyothi on 30/9/17.
  */
object Main extends App {

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }

  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

  println(removeAt(1, List('a', 'b', 'c', 'd'))) // List(a, c, d)

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if(x > y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => y * y :: squareList(ys)
    }

  def squareListWithMap(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  /*def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => (x :: xs1.takeWhile(y => y.equals(x))) :: pack(xs1.dropWhile(y => y.equals(x)))
  }*/

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  println(pack(List("a", "a", "a", "b", "c", "c", "a"))) //List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

  def encode[T](strings: List[T]) = pack(strings).map(x => (x.head, x.length))

  println(encode(List("a", "a", "a", "b", "c", "c", "a"))) //List(("a", 3), ("b", 1), ("c", 2), ("a", 1))

}
