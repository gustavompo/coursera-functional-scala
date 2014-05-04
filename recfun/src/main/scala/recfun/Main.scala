package recfun
import common._

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
    if(c == 0 || r == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(parenthesesCount:Int, chars: List[Char]): Boolean = {
      if(parenthesesCount < 0) false
      else if(chars.isEmpty) parenthesesCount == 0
      else {
	      val currentCount =  if (chars.head == '(') (parenthesesCount + 1) else if (chars.head == ')') (parenthesesCount - 1) else parenthesesCount
	      isBalanced(currentCount, chars.tail)
      }
    }
    isBalanced(0, chars);
  } 
  
  def countChange(money:Int, coins:List[Int]) :Int = {
    def countChangeInner(money:Int, coins:List[Int]) :Int = {
      if( money == 0 ) 1
      else if (money < 0) 0
      else if( coins.isEmpty ) 0
      else countChangeInner(money , coins.tail) + countChangeInner(money - coins.head, coins)
    }
    if(money == 0 || coins.isEmpty) 0
    else countChangeInner(money, coins);
  }
}
