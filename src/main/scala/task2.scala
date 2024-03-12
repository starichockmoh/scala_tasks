import scala.util._
import scala.io.StdIn.readLine

import scala.util.matching.Regex

@main def task1_1(): Unit = {

  def performOperation(input: String): Double = {
    val parts = input.split(" ")
    if (parts.length != 3) {
      return throw new IllegalArgumentException("неправильный формат")
    }
    val num1 = Try(parts(0).toDouble)
    val num2 = Try(parts(2).toDouble)

    (num1, num2) match {
      case (Success(n1), Success(n2)) =>
        parts(1) match {
          case "+" => n1 + n2
          case "-" => n1 - n2
          case "*" => n1 * n2
          case "/" => if (n2 != 0) n1 / n2
          else throw (new ArithmeticException("деление на ноль"))
          case _ => throw new IllegalArgumentException("Неподдерживаемая операция")
        }
      case _ => throw new NumberFormatException("Введены некорректные значения")
    }
  }

  println("Введите выражение: ")
  val input = readLine()

  try {
    println(performOperation(input))
  }
  catch {
    case a: ArithmeticException => println(s"ArithmeticException: ${a.getMessage}")
    case b: IllegalArgumentException => println(s"IllegalArgumentException: ${b.getMessage}")
    case c: NumberFormatException => println(s"NumberFormatException: ${c.getMessage}")
  }
}


@main def task2_1(): Unit = {
  def sumDigitsInString(str: String): Int = {
    var sum = 0
    for (char <- str if char.isDigit) {
      sum += char.asDigit
    }
    sum
  }

  val input = readLine("Введите строку: ")
  val sum = sumDigitsInString(input)
  println(s"Сумма цифр в строке: $sum")

}


@main def task2_2(): Unit = {
  def charMult(str: String): Long = {
    var sum = 1L
    for (char <- str if char.isLetter) {
      sum *= char.toInt
    }
    sum
  }

  val input = readLine("Введите строку: ")
  val sum = charMult(input)
  println(s"Произведение кодов всех букв в строке: $sum")
}


@main def task2_3(): Unit = {
  val n = readLine("Введите число: ").toInt

  for (i <- 1 to n) {
    for (j <- i to 1 by -1) {
      print(j + " ")
    }
    println()
  }

}


@main def task2_4(): Unit = {
  val secretNumber = Random.nextInt(100) + 1
  var guessed = false

  while (!guessed) {
    val userGuess = readLine("Угадайте число от 1 до 100: ").toInt

    if (userGuess == secretNumber) {
      println("Поздравляем! Вы угадали число.")
      guessed = true
    } else if (userGuess < secretNumber) {
      println("Ваше число меньше загаданного.")
    } else {
      println("Ваше число больше загаданного.")
    }
  }
}


@main def task3_1(): Unit = {
  val carNumberPattern: Regex = "[А-Яа-я][0-9]{3}[А-Яа-я]{2}".r
  val input = readLine("Введите номер: ")
  if (carNumberPattern.matches(input)) println("Да")
  else println("Нет")
}


@main def task3_2(): Unit = {
  val hasUppercase = ".*[A-Z].*".r
  val hasLowercase = ".*[a-z].*".r
  val hasDigit = ".*[0-9].*".r
  val hasSpecialChar = ".*[!@#$%^&*()].*".r
  val hasEightChars = ".{8,}".r
  val input = readLine("Введите пароль: ")

  val boolPass = hasUppercase.matches(input) && hasLowercase.matches(input) &&
    hasDigit.matches(input) && hasSpecialChar.matches(input) && hasEightChars.matches(input)

  if (boolPass) println("Да")
  else println("Нет")
}


@main def task3_3(): Unit = {
  val carNumberPattern: Regex = "\\d{2}\\.\\d{2}\\.\\d{4}".r
  val input = readLine("Введите дату: ")
  if (carNumberPattern.matches(input)) println("Да")
  else println("Нет")
}
