import scala.annotation.tailrec
import scala.collection.immutable
import scala.math.*
import scala.util.*


@main def task5_1(): Unit = {
  //----TASK 1
  def insertionSort[T](list: List[T])(using ord: Ordering[T]): List[T] = list match {

    case Nil => Nil
    case head :: tail => insert(head, insertionSort(tail))
  }

  def insert[T](elem: T, sortedList: List[T])(using ord: Ordering[T]): List[T] = sortedList match {
    case Nil => List(elem)
    case head :: tail =>
      if (ord.lt(elem, head)) elem :: sortedList
      else head :: insert(elem, tail)
  }

  //----TASK2
  def quickSort[T](list: List[T])(using ord: Ordering[T]): List[T] = {
    list match {
      case Nil => Nil
      case pivot :: tail =>
        val (less, greater) = tail.partition(ord.lt(_, pivot))
        quickSort(less) ::: pivot :: quickSort(greater)
    }
  }

  def kthStatistic[T](k: Int, list: List[T])(using ord: Ordering[T]): Option[T] = {
    val sortedList = quickSort(list)
    if (k > 0 && k <= sortedList.length) Some(sortedList(k - 1))
    else None
  }

  //----TASK3
  def isPrime(n: Int): Boolean = {
    @tailrec
    def check(i: Int): Boolean = if (i == 1) true
    else if (n % i != 0) check(i - 1)
    else false

    n > 1 && check(n / 2)
  }

  def primeFrom(n: Int): LazyList[Int] = {
    if (isPrime(n)) n #:: primeFrom(n + 1)
    else primeFrom(n + 1)
  }

  //----TASK4
  type Point = (Int, Int)
  type Ship = List[Point]
  type Field = Vector[Vector[Boolean]]
  class ShipExists(s: String) extends Exception(s) {}

  def addShip(field: Field, ship: Ship): Field = {
    ship.foldLeft(field) { (updatedField, point) =>
      val (x, y) = point
      if (updatedField(x)(y)) {
        throw new ShipExists(s"There is a ship already in ($x, $y)!")
      }
      updatedField.updated(x, updatedField(x).updated(y, true))
    }
  }


  println("TASK1")
  val intList = List(3, 1, 4, 1, 5, 9)
  val doubleList = List(2.718, 3.14, 1.414)
  val stringList = List("apple", "banana", "cherry")
  println(insertionSort(intList)) // Вывод: List(1, 1, 3, 4, 5, 9)
  println(insertionSort(doubleList)) // Вывод: List(1.414, 2.718, 3.14)
  println(insertionSort(stringList)) // Вывод: List(apple, banana, cherry)
  println("TASK2")
  println(kthStatistic(4, List(3, 8, 1, 12, 23)))
  println(kthStatistic(3, List(4, 7, 6, 5, 12, 9, 5)))
  println(kthStatistic(5, List(4, 7, 6, 5, 12, 9, 5)))
  println(kthStatistic(1, intList))
  println(kthStatistic(2, doubleList))
  println(kthStatistic[String](3, stringList))
  println("TASK3")
  val primes = primeFrom(1).takeWhile(_ <= 100).force
  println(primes)

  println("TASK4")
  try {
    val initialField: Field = Vector.fill(10, 10)(false)
    val ship1: Ship = List((3, 3), (5, 5), (5, 6), (5, 7))
    val ship2: Ship = List((3, 3), (6, 9))
    var updatedField = addShip(initialField, ship1)
    updatedField.foreach(row => println(row.mkString(" ")))
    updatedField = addShip(updatedField, ship2)

    // Вывод поля
    updatedField.foreach(row => println(row.mkString(" ")))
  }
  catch {
    case e: ShipExists => println(e.getMessage)
  }


}
