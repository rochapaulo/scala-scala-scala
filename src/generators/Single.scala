package generators

case class Single[T](v: T) extends Generator[T] {

  override def generate = v

}
