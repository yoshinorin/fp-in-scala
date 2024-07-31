package net.yoshinorin.fpinscala.datastructures

import net.yoshinorin.fpinscala.datastructures.Tree._
import org.scalatest.wordspec.AnyWordSpec

// testOnly net.yoshinorin.fpinscala.datastructures.TreeSpec
class TreeSpec extends AnyWordSpec {

  "Tree" should {

    "size" in {
      println("\n-------size")
      println(size(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))))
    }

    "maximam" in {
      println("\n-------maximam")
      println(maximam(Branch(Leaf(1), Branch(Leaf(6), Leaf(3)))))
      /*
      Branch(Leaf(1),Branch(Leaf(6),Leaf(3)))
      Leaf(1)
      Branch(Leaf(6),Leaf(3))
      Leaf(6)
      Leaf(3)
       */
    }

    "depth" in {
      println("\n-------depth")
      println(depth(Branch(Leaf(1), Branch(Leaf(6), Leaf(3)))))
    }

    "map" in {
      println("\n-------map")
      println(map(Branch(Leaf(1), Branch(Leaf(6), Leaf(3))))(t => t.toString()))
      println(map(Branch(Leaf(1), Branch(Leaf(6), Leaf(3))))(t => t * 2))
    }

    "fold" in {
      println("\n-------fold")
      println(fold(Branch(Leaf(1), Branch(Leaf(6), Leaf(3))))(x => x * 2)(_ * _))
    }

  }

}
