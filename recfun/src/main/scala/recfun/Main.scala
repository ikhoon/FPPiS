package recfun
import common._

import scala.swing.event.WindowOpened

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
   * base condition
   * pascal(0, 0) => 1
   * pascal(0, r) => 1
   * pascal(r, r) => 1
   * generalize
   * pascal(c, r) => pascal(c - 1, r - 1) + pascal(c, r - 1)
   * c
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, 0) => 1
    case (0, _) => 1
    case (_, _) if c == r => 1
    case (col, row) => pascal(col - 1, row - 1) + pascal(col, row - 1)
  }


  /**
   * Exercise 2
   * chars = "just((a)()) parentheses"
   * if opened < 0
   *   opened
   * else
   *   opened++ if chars.head == "("
   *   opened-- if chars.head == ")"
   *   opened if chars.head is not parentheses
   *
   */
  def balance(chars: List[Char]): Boolean = {
    def inspect(opened: Int, chars: List[Char]) : Int =
      if (opened < 0) opened
      else
        chars match {
          case '(' :: rest => inspect(opened + 1, rest)
          case ')' :: rest => inspect(opened - 1, rest)
          case _ :: rest => inspect(opened, rest)
          case _ => opened
        }
      inspect(0, chars) == 0
  }

  /**
   * Exercise 3
   * base case
   * countChange(0, coins) => 0
   * countChange(money, Nil) => 0
   *
   * base case
   * count(0, coins) => 1
   * count(-money, coins) => 0
   * generalize
   * count(money, coins) => count(money - coins.head, coins) + count(money - coins.tail.head, coins.tail)
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0 || coins.isEmpty) 0
    def count(money: Int, coins: List[Int]): Int = {
      (money, coins) match {
        case (0, _) => 1
        case (m, _) if m < 0 => 0
        case (_, c :: Nil) => count(money - c, coins)
        case (_, c :: cs) => count(money - c, coins) + count(money, cs)
      }
    }
    count(money, coins)
  }
}
