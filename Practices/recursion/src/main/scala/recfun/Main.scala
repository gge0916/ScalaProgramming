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
      if (c > r) 0 // Out of the triangle
      else if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def helper(chars: List[Char], openCount: Int): Boolean = {
        if (chars.isEmpty) {
          openCount == 0
        } else {
          val head = chars.head
          val newOpenCount =
            if (head == '(') openCount + 1
            else if (head == ')') openCount - 1
            else openCount
          // ')' should never be more than '('
          if (newOpenCount < 0) false
          else helper(chars.tail, newOpenCount)
        }
      }
      helper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def helper(money: Int, sortedCoins: List[Int]): Int = {
        if (money == 0) {
          1
        }
        else if (sortedCoins.isEmpty) {
          if (money == 0) 1
          else 0
        } else {
          val first = sortedCoins.head
          if (money < first) {
            0
          } else {
            helper(money - first, sortedCoins) + helper(money, sortedCoins.tail)
          }
        }

      }
      val sortedCoins = coins.sorted
      helper(money, sortedCoins)
    }
  }
