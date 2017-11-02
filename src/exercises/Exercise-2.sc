import datastructures._

/**
  * Implement the function tail for "removing" the first element of
  * a List. Notice the function takes constant time. What are different
  * choices you could make in your implementation if List is Nil?
  *
  */
def tail[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case Cons(_, t) => t
}

tail(List())
tail(List(1))
tail(List(1,2))
tail(List(1,2,3,4))