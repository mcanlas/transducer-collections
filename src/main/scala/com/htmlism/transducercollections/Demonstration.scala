package com.htmlism.transducercollections

object Demonstration extends App {
  val numbers = List.fill(20)(util.Random.nextInt(10))

  println("Here are 20 random numbers")
  println(numbers.mkString(", "))

  val transducer: Transformer[Int, String] =
    Filter[Int](_ % 2 == 0) andThen
      Mapper(n => n.toString * n) andThen
      FlatMapper(x => List(x, x))

  val foldable = new Foldable(numbers)

  println("\nHere is the foldable, rendered to standard out")
  println(foldable.render)

  println("\nHere is the foldable, with a transducer applied manually")
  println(foldable
    // notice how the destination type is annotated
    .fold[List[String]](Nil, transducer appliedTo accToList)
    .mkString(", "))

  println("\nHere is the foldable, with a transducer applied behind the scenes")
  println(foldable
    .filter(_ % 2 == 1)
    .map(_ + 200)
    .render
    .mkString("; "))

  // { (acc, x) => acc :+ x } )
  def accToList[A](acc: List[A], x: A) = acc :+ x
}
