import datastructures._

/**
  * foldRight is not tail-recursive and will StackOverflow for large lists.
  * Convince yourself that this is the case, then write another general
  * list-recursion function, foldLeft that is tail-recursive.
  *
  * foldLeft is defined as a method of List in the Scala standard library,
  * and it' curried similarly for better type inference, so you can write
  * myList.foldLeft(0.0)(_ + _)
  *
  */
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???