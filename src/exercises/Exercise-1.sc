import datastructures._

def sum(l: List[Int]): Int = {
  def calc(l: List[Int], o: Int): Int = l match {
    case datastructures.Nil => o
    case Cons(h ,t) => calc(t, o + h)
  }
  calc(l, 0)
}

/**
  * What will the result of the following match expression be?
  * Answer: 3 (x + y). It matches the third expression.
  *
  */
val x = datastructures.List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case datastructures.Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}