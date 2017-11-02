package generators

import java.util.Random

class Integers extends Generator[Int] {

  val random = new Random()
  override def generate = random.nextInt()

}
