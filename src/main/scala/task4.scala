import scala.BigInt
import scala.math.*
import scala.util.*


@main def task4_1(): Unit = {
  //----TASK 1

  def weightChips(potato: BigInt, waterPotato: Double, waterChips: Double): String = {
    (potato.toDouble * (1 - waterPotato) / (1 - waterChips)).formatted("%.2f")
  }

  //----TASK1b
  val weightChipsC = ((potato: BigInt, waterPotato: Double, waterChips: Double) =>
    (potato.toDouble * (1 - waterPotato) / (1 - waterChips)).formatted("%.2f")).curried

  //----TASK1c
  def weightChipsC1: BigInt => Double => Double => String = potato => waterPotato => waterChips =>
    (potato.toDouble * (1 - waterPotato) / (1 - waterChips)).formatted("%.2f")

  //----TASK2a
  def strToColDigits: String => Int = str => str.count(_.isDigit)

  def strToSumDigits: String => Int = str => str.filter(_.isDigit).map(_.asDigit).sum

  def strToColAlpha: String => Int = str => str.count(_.isLetter)

  ///----TASK2b
  def compareString(f: String => Int): (String, String) => Boolean =
    (s1: String, s2: String) => f(s1) == f(s2)


  ///----TASK3

  // Функция для разбора строки на числа и операции
  def parseExpression(expression: String): (Double, String, Double) = {
    val regex = "(-?\\d+(\\.\\d+)?) ([+\\-*/]) (-?\\d+(\\.\\d+)?)".r
    if (regex matches (expression)) {
      val a = expression.split(" ")
      (a(0).toDouble, a(1), a(2).toDouble)
    }
    else throw throw new IllegalArgumentException("Invalid expression format")
  }

  // Функция высшего порядка и частичная функция для выполнения операции
  val performOperation: PartialFunction[String, (Double, Double) => Double] = {
    case "+" => (x, y) => x + y
    case "-" => (x, y) => x - y
    case "*" => (x, y) => x * y
    case "/" => (x, y) => if (y != 0) x / y else throw new ArithmeticException("/ by zero")

  }

  // Функция для вычисления выражения
  def calculateExpression(exp: String): Double = {
    val (num1, op, num2) = parseExpression(exp)
    performOperation(op)(num1, num2)

  }


  println("TASK1a")
  println(weightChips(90, 0.9, 0.1)) // Вывод: 10.00
  println(weightChips(100, 0.85, 0.1)) // Вывод: 16.67
  println(weightChips(10000000000L, 0.9, 0.05)) // Вывод: 1052631578.95
  println("TASK1b")
  println(weightChipsC(90)) // Вывод: 10.00
  println(weightChipsC(100)(0.85)) // Вывод: 16.67
  println(weightChipsC(10000000000L)(0.9)(0.05)) // Вывод: 1052631578.95
  println("TASK1c")
  println(weightChipsC1(90)) // Вывод: 10.00
  println(weightChipsC1(100)(0.85)) // Вывод: 16.67
  println(weightChipsC1(10000000000L)(0.9)(0.05)) // Вывод: 1052631578.95
  println("TASK2a")
  println(strToColDigits("123asd12jf4)"))
  println(strToSumDigits("123asd12jf4)"))
  println(strToColAlpha("123asd12jf4)"))
  println("TASK2bc")
  println(compareString(strToColDigits)("sd12", "f5sf"))
  println(compareString(strToSumDigits)("12sd", "f3sf"))
  println(compareString(strToColAlpha)("fsr5g43f", "fsrgg43f"))
  println("TASK3")

  try {

    println(calculateExpression("-1 + 2")) // Вывод: 3.0
    println(calculateExpression("1.5 - 3")) // Вывод: -1.5
    println(calculateExpression("-5.2 / 2")) // Вывод: -2.6
    println(calculateExpression("6 * 2.5")) // Вывод: 15.0
    println(calculateExpression("7 / 0")) // ArithmeticException: Division by /
  }
  catch {
    case a: ArithmeticException => println(s"ArithmeticException: ${a.getMessage}")
    case b: IllegalArgumentException => println(s"IllegalArgumentException: ${b.getMessage}")
    case c: NumberFormatException => println(s"NumberFormatException: ${c.getMessage}")
  }

}
