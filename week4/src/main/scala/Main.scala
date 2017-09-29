/**
  * Created by jyothi on 21/9/17.
  */
object Main extends App {

  def show(e: Expr): String = e match {
    case Number(e) => e.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
  }

  //println(show(Sum(Number(1), Number(2))))

  import scala.collection.immutable.List
  def insert(x: Int, xs: scala.collection.immutable.List[Int]): scala.collection.immutable.List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x <= y) x :: xs else y :: insert(x, ys)
  }

}
