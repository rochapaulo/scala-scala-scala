import datastructures._

def tail[T](xs: List[T]) = xs match {
  case Nil => Nil
  case Cons(_, tail) => tail
}

/**
  * Generalize tail to the function drop, which removes the first
  * n elements from List
  *
  */
def drop[T](xs: List[T], n: Int): List[T] = {
  if (n > 0) drop(tail(xs), n - 1) else xs
}


drop(List(), 10)
drop(List(1), 1)
drop(List(1), 2)
drop(List(1,2,3,4,5,6), 10)
drop(List(1,2,3,4,5,6), 3)
drop(List(1,2,3,4), 2)