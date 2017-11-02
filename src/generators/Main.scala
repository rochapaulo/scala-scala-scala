package generators

object Main extends App {

  override def main(args: Array[String]): Unit = {

    println("Generators ....")

    val integers = new Integers
    val booleans = new Booleans

    println(integers.generate)
    println(booleans.generate)

    val booleans_v2 = integers.map(_ > 0)

    println(s"boolean_v2 = ${booleans_v2.generate}")

    val booleans_v3 = for { x <- integers } yield x > 0

    println(s"boolean_v3 = ${booleans_v3.generate}")

    println(s"(Int, Boolean) = ${pairs(integers, booleans).generate}")
    println(s"(Int, Boolean) = ${pairs(integers, booleans_v2).generate}")
    println(s"(Int, Boolean) = ${pairs(integers, booleans_v3).generate}")


    println("---- List Generator ----")
    val listGenerator = ListGenerator(new Integers)

    val xs = listGenerator.generate
    println(xs)

    val ys = listGenerator.generate
    println(ys)

  }


  def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
    x => u.map { y => (x, y) }
  }

  def pairs_v2[T, U](t: Generator[T], u: Generator[U]) = {
    for {
      x <- t
      y <- u
    } yield (x, y)
  }

}
