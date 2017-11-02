import datastructures._

/**
  * Implement a function init, which returns a List
  * consisting of all but the last element of a List
  *
  * Given: List(1, 2, 3, 4), init will return List(1, 2, 3)
  *
  */
def init[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case Cons(h, tail) => {
    tail match {
      case Nil => Nil
      case Cons(_, _) => Cons(h, init(tail))
    }
  }
}

init(List())
init(List(1))
init(List(1,2))
init(List(1,2,3))
init(List(1,2,3,4,5))