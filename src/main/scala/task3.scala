import scala.math.abs
import scala.annotation.tailrec

// task 1

@main def task1(): Unit = {

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if isGoodEnough(guess) then guess
      else sqrtIter(improve(guess))

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0001

    sqrtIter(1)
  }


  println(sqrt(0.001)) // Результат: 0.03162278245070105
  println(sqrt(0.1e-20)) // Результат: 3.1622776601683795E-11
  println(sqrt(1.0e20)) // Результат: 1.0000000076341822E10
  println(sqrt(1.0e50)) // Результат: 1.0000000000000002E25
}

// task 2

@main def task2(): Unit = {

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  println(pascal(0,2))
  println(pascal(1,2))
  println(pascal(1,3))

}

// task 3

@main def task3(): Unit = {


  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceIter(lst: List[Char], acc: Int): Boolean = {
      if (lst.isEmpty) (acc == 0)
      else {
        val head = lst.head
        if (head == '(') balanceIter(lst.tail, acc + 1)
        else if(head == ')') {
          if(acc <= 0) false
          else balanceIter(lst.tail, acc - 1)
        }
        else balanceIter(lst.tail, acc)
      }
    }

    balanceIter(chars, 0)
  }


  val str = "(если (ноль? x) максимум (/ 1 x))"
  val lst: List[Char] = str.toList
  println(balance(lst))


  val str1 = "Я сказал ему (что это (еще) не сделано). (Но он не слушал)"
  val lst1: List[Char] = str1.toList
  println(balance(lst1))


  val str2 = ":-)"
  val lst2: List[Char] = str2.toList
  println(balance(lst2))

  val str3 = "())("
  val lst3: List[Char] = str3.toList
  println(balance(lst3))


}

// task 4


@main def task4(): Unit = {

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }


  val coins = List(1, 2)
  println(countChange(4, coins))


}