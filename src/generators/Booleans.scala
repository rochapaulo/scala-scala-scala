package generators

class Booleans extends Generator[Boolean] {

  val integers = new Integers
  override def generate = integers.generate > 0

}
