/**
  * Created by jyothi on 3/12/17.
  */
object Main extends App {

  val problem = new Pouring(Vector(4, 7))

  problem.moves

  //problem.pathSets.take(3).toList.mkString(" - ")

  println(problem.solution(6))

}
