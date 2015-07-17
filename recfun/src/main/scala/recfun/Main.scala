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
   * pascal(0, 0) = 1
   * pascal(0, 1) = pascal(0, 1 - 1) = 1
   * pascal(1, 1) = pascal(1 - 1, 1 - 1) = 1
   * pascal(0, 2) = pascal(0, 2 - 1) = pascal(0, 1 - 1) = 1
   * pascal(1, 2) = pascal(1 - 1, 2 - 1) + pascal(1, 2 - 1) = 1 + 1
   *
   * generalize
   * base condition
   * pascal(0, 0) => 1
   * pascal(0, r) => 1
   * pascal(r, r) => 1
   * normalize
   * pascal(c, r) => pascal(c - 1, r - 1) + pascal(c, r - 1)
   * c
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, 0) => 1
    case (0, _) => 1
    case (_, _) if c == r => 1
    case (column, row) => pascal(column - 1, row - 1) + pascal(column, row - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
