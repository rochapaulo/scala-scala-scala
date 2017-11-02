package generators

case class ListGenerator[T](elements: Generator[T]) extends Generator[List[T]] {

  private val _booleans = new Booleans

  private val emptyList = Single(Nil)

  private val nonEmptyList: Generator[List[T]] = for {
    head <- elements
    tail <- lists
  } yield (head :: tail)

  private def lists = for {
    isEmpty <- _booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list


  override def generate = lists.generate

}
