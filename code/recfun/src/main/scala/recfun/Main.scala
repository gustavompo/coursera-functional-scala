package recfun

import scala.annotation.tailrec

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
      if( c == 0 || c == r ) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def b(c:List[Char], pCount:Int): Boolean = {
        if(pCount < 0) false
        else if (c.isEmpty) pCount == 0
        else {
          val p = if (c.head == '(')  1 else if (c.head == ')') -1 else 0
          b(c.tail, pCount + p )
        }
      }
      b(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(mon: Int, cs: List[Int]): Int = {
      if(mon == 0) return 0

      def countIt(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (coins.isEmpty || (money < 0) ) 0
        else countIt(money - coins.head, coins) + countIt(money, coins.tail)
      }
      countIt(mon, cs)
    }
  }
