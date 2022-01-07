package net.yoshinorin.fpinscala.datastructures

import net.yoshinorin.fpinscala.datastructures.List._
import org.scalatest.wordspec.AnyWordSpec

// testOnly net.yoshinorin.fpinscala.datastructures.ListSpec
class ListSpec extends AnyWordSpec {

  "List" should {

    "tail" in {
      // tail
      println("\n-------tail")
      println(tail(List(1, 2, 3, 4)))
      println(tail(List()))
      println(tail(List("A", "B", "C")))
    }

    "setHead" in {
      // setHead
      println("\n-------setHead")
      println(setHead(1, List(2, 3, 4, 5)))
      println(setHead(1, List()))
      println(setHead("A", List("B", "C", "D")))
    }

    "drop" in {
      // drop
      println("\n-------drop")
      println(drop(List(1, 2, 3, 4), 2))
      println(drop(List()))
      println(drop(List(), 2))
      println(drop(List("A", "B", "C"), 2))
      println(drop(List("A", "B", "C"), 4))
    }

    "dropWhile" in {
      // dropWhile
      println("\n-------dropWhile")
      //println(dropWhile(List(1, 3, 2, 3, 5), (x: Int) => x == 3))  //
      println(dropWhile(List(1, 3, 2, 3, 5))(x => x == 3))
    }

    "length" in {
      // length
      println("\n-------length")
      println(length(List("A", "B", "C", "D")))
    }

    "foldLeft - sum" in {
      // foldLeft - sum
      println("\n-------foldleft sum")
      println(sum3(List(2, 3, 45)))
      println(product3(List(2, 2, 3)))
    }

    "foldLeft reverse" in {
      println("\n-------foldleft reverse")
      println(reverse(List(2, 3, 4, 5, 6)))
    }

    "foldLeft - incrementOne" in {
      println("\n-------foldRight incrementOne")
      println(incrementOne(List(2, 4, 6, 8)))
    }

    "foldRight - Double To String" in {
      println("\n-------foldRight DoubleToString")
      println(net.yoshinorin.fpinscala.datastructures.List.toString(List(2.0, 2.1, 2.2)))
    }

    "map" in {
      println("\n-------map")
      println(map(List(1, 2, 3, 4))(x => x.toString()))
      println(map(List(1, 2, 3, 4))(x => x * 2))
    }

    "filter" in {
      println("\n-------filter")
      println(filter(List(1, 2, 2, 4, 5, 6, 2))(x => x == 2))
    }

    "flatMap" in {
      println("\n-------flatMap")
      println(flatMap(List(List(1, 2, 3), List(4, 5, 6)))(x => x))
      println(flatMap(List(1, 2, 3))(x => List(x, x)))
    }

  }

}
