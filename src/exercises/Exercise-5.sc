import datastructures._

/**
  * Implement the function setHead for replacing the first element
  * of a List with a different value
  *
  */
def setHead[T](xs: List[T], value: T): List[T] = xs match {
  case Nil => Nil
  case Cons(_, tail) => Cons(value, tail)
}


setHead(List(), 1)
setHead(List(9, -10, 1,2,3,4), -9)
setHead(setHead(List(1,2,3,4), 0), 10)