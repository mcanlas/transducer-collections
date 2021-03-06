package com.htmlism.transducercollections

object Demonstration extends App {
  val numbers = List.fill(20)(util.Random.nextInt(10))

  println("Here are 20 random numbers")
  println(numbers.mkString(", "))

  val transducer: EasyTransducer[Int, String] =
    Filter[Int](_ % 2 == 0) andThen
      Mapper(n => n.toString * n) andThen
      FlatMapper(x => List(x, x))

  val stream = StreamLike(numbers)

  println("\nHere is the stream, rendered to standard out")
  println(stream.toList)

  println("\nHere is the stream, with no transducer")
  println(stream
    // notice how the destination type is explicit
    .fold(Nil, { (acc: List[Int], x) => acc :+ x } )
    .mkString(", "))

  println("\nHere is the stream, with a transducer applied manually")
  println(stream
    // notice how the destination type is explicit
    .fold(Nil, transducer wrappedAround { (acc: List[String], x) => acc :+ x })
    .mkString(", "))

  println("\nHere is the stream, with a transducer applied behind the scenes")
  println(stream
    .filter(_ % 2 == 1)
    .map(_ + 200)
    .toList
    .mkString("; "))
}
