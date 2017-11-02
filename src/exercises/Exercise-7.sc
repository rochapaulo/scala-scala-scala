import datastructures._

def sum(ints: List[Int]): Int =
  ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

def product(doubles: List[Double]): Double =
  doubles match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }


/**
  * Implement a second version of sum and product
  * using foldRight.
  *
  * Can product implemented using foldRight immediately halt
  * the recursion and return 0.0 if it encounters a 0.0?
  * Why or why not?
  *
  */
def sum2(ints: List[Int]): Int = ???

def product2(doubles: List[Double]): Double = ???


