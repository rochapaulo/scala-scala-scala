import datastructures._

def tail[T](xs: List[T]) = xs match {
  case Nil => Nil
  case Cons(_, tail) => tail
}

/**
  * Implement dropWhile, which removes elements from the List
  * prefix as long as they match a predicate. Again, notice these
  * functions take time proportional only to the number of elements
  * being dropped - we do not need to make a copy of the entire List.
  *
  */
def dropWhile[T](xs: List[T])(p: T => Boolean): List[T] = xs match {
  case Nil => Nil
  case Cons(head, tail) => if (p(head)) dropWhile(tail)(p) else xs
}

dropWhile(List(1,2,3,4,5))(x => x < 4)
dropWhile(List(1,2,3,4,5))(x => x <= 4)
dropWhile(List(1,2,3,4,5))(x => x < 20)
dropWhile(List(1,2,3,4,5))(x => x < 0)