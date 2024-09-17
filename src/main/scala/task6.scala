import scala.collection.immutable
import scala.util.*


@main def task6(): Unit = {
  //----TASK 1
  def pack[T](xs: List[T]): List[List[T]] = {
    xs.foldRight(List.empty[List[T]]) { (x, acc) =>
      acc match {
        case Nil => List(List(x))
        case head :: tail if head.head == x => (x :: head) :: tail
        case _ => List(x) :: acc
      }
    }
  }

  //----TASK2
  def encode1[T](xs: List[T]): List[(T, Int)] = {
    xs.foldRight(List.empty[(T, Int)]) { (x, acc) =>
      acc match {
        case Nil => List((x, 1))
        case (y, n) :: tail if x == y => (y, n + 1) :: tail
        case _ => (x, 1) :: acc
      }
    }
  }

  //----TASK3
  def encode2[T](xs: List[T]): List[(T, Int)] = {
    xs.groupBy(identity).mapValues(_.size).toList
  }


  println("TASK1")
  val input1 = List('a', 'a', 'a', 'b', 'b', 'a')
  val result1 = pack(input1)
  println(result1) // Вывод: List(List('a', 'a', 'a'), List('b', 'b'), List('a'))

  println("TASK2")
  val input2 = List('a', 'a', 'a', 'b', 'b', 'a')
  val result2 = encode1(input2)
  println(result2) // Вывод: List(('a', 3), ('b', 2), ('a', 1))

  println("TASK3")
  val input3 = List('a', 'a', 'a', 'b', 'b', 'a')
  val result3 = encode2(input3)
  println(result3) // Вывод: List(('a', 4), ('b', 2))


}
