package fpinscala.testing

object test extends App {
  val something = LazyList.cons[String]({"abc"}, { LazyList.empty})
  val list: List[String] = something.toList
}