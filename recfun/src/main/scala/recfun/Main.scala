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
    if   (c == 0 || r == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(current: List[Char], open: Int = 0, close: Int = 0): Boolean = {
      if (current.isEmpty) open == close
      else if (current.head == '(') balanceIter(current.tail, open+1, close)
      else if (current.head == ')') open > close && balanceIter(current.tail, open, close+1)
      else balanceIter(current.tail, open, close)
    }
    balanceIter(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && !coins.isEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}
