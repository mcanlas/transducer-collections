package com.htmlism.transducercollections

object Demonstration extends App {
  val numbers = List.fill(20)(util.Random.nextInt(10))

  println(numbers.mkString(", "))

  val transducer =
    Filter[Int](_ % 2 == 0) andThen
      Mapper(n => n.toString * n) andThen
      FlatMapper(x => List(x, x))

  val foldable = new Foldable(numbers)

  println("filtered")

  println(foldable
    .fold[List[String]](Nil, transducer appliedTo { (acc, x) => acc :+ x } )
    .mkString(", "))

  println(foldable
    .filter(_ % 2 == 1)
    .map(_ + 200)
    .render
    .mkString("; "))

  println(foldable.render)
}
