package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val open = '('
      val close = ')'
      def checkWithStack(chars: List[Char], stack: List[Char]): Boolean = {
        if(chars.isEmpty && stack.isEmpty) true
        else if(chars.isEmpty && stack.nonEmpty) false
        else {
          if(chars.head.equals(open)){
            checkWithStack(chars.tail, stack :+ open)
          }else if(chars.head.equals(close)){
            if(stack.nonEmpty && stack.last.equals(open)){
              checkWithStack(chars.tail, stack.take(stack.size - 1))
            }else{
              checkWithStack(chars.tail, stack :+ open)
            }
          }else{
            checkWithStack(chars.tail, stack)
          }
        }
      }
      checkWithStack(chars, List.empty)
    }
  
  /**
    * Exercise 3
    * 1) Optimal Substructure
    *  To count total number solutions, we can divide all set solutions in two sets.
    *  1) Solutions that do not contain mth coin (or Sm).
    *  2) Solutions that contain at least one Sm.
    *  Let count(money) be the function to count the number of solutions, then it can be written as sum of count(money, coins.last) and count(money - coins.head, coins).
    *
    *  Therefore, the problem has optimal substructure property as the problem can be solved using solutions to subproblems.
    *
    * 2) Overlapping Subproblems
    *  Following is a simple recursive implementation of the Coin Change problem. The implementation simply follows the recursive structure mentioned above.
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0 || coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

  }
