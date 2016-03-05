package com.htmlism.transducercollections

/**
  * Any structure that describes the transformation of data via the manipulation of reducers.
  *
  * @tparam A The origin type
  * @tparam B The destination type
  */

trait Transducer[A, B] {
  /**
    * Given a source reducer, this emits another reducer.
    *
    * This is the core logic of the transducer.
    *
    * Could be viewed as a function
    *
    *    type Transducer[A, B] = (Reducer[X, B] => Reducer[X, A]) forSome { type X }
    *
    * May look backwards, but reducers are always talking about the destination. So if we want to change the source A
    * to the same destination B, then we are changing the overall reduction from original Bs to new As.
    *
    * @param red The given reducer
    * @tparam X The collection type
    * @return A reducer
    */

  def apply[X](red: Reducer[X, B]): Reducer[X, A]

  /**
    * A convenience method for chaining transducers using infix notation.
    *
    * @param that
    * @tparam C
    * @return
    */

  def andThen[C](that: Transducer[B, C]): Transducer[A, C] = new Composite(this, that)
}

/**
  * Convenience class to make transducers less intimidating.
  *
  * @tparam A The origin type
  * @tparam B The destination type
  */

trait EasyTransducer[A, B] extends Transducer[A, B] {
  def wrapping[X](red: Reducer[X, B]): Reducer[X, A] = apply[X](red)

  /**
    * Exists only to make the return type more specific.
    *
    * @param that
    * @tparam C
    * @return
    */

  override def andThen[C](that: Transducer[B, C]): EasyTransducer[A, C] = new Composite(this, that)
}

/**
  * Describes two transducers in sequence.
  *
  * @param left
  * @param right
  * @tparam A The origin type
  * @tparam B The destination type
  * @tparam C
  */

class Composite[A, B, C](left: Transducer[A, B], right: Transducer[B, C]) extends EasyTransducer[A, C] {
  def apply[X](red: Reducer[X, C]) = left(right(red))
}

/**
  * Describes filtering.
  *
  * @param f
  * @tparam A The origin type
  */

class Filter[A](f: A => Boolean) extends EasyTransducer[A, A] {
  def apply[X](red: Reducer[X, A]) =
    (acc, x) =>
      if (f(x))
        red(acc, x)
      else
        acc
}

/**
  * Describes mapping
  *
  * @param f
  * @tparam A The origin type
  * @tparam B The destination type
  */

class Mapper[A, B](f: A => B) extends EasyTransducer[A, B]  {
  def apply[X](red: Reducer[X, B]) =
    (acc, x) => red(acc, f(x))
}

/**
  * Describes flat mapping.
  *
  * A limitation of this transducer is that it is hard-coded to reduce over traversables. Ideally, the collection or
  * monad could be injected so that one could transduce any reduction.
  *
  * @param f
  * @tparam A The origin type
  * @tparam B The destination type
  */

class FlatMapper[A, B](f: A => TraversableOnce[B]) extends Transducer[A, B]  {
  def apply[X](red: Reducer[X, B]) =
    (acc, x) => f(x).foldLeft(acc)(red)
}

/**
  * A companion factory.
  */

object Filter {
  def apply[A](f: A => Boolean): Filter[A] = new Filter(f)
}

/**
  * A companion factory.
  */

object Mapper {
  def apply[A, B](f: A => B): Mapper[A, B] = new Mapper(f)
}

/**
  * A companion factory.
  */

object FlatMapper {
  def apply[A, B](f: A => TraversableOnce[B]): FlatMapper[A, B] = new FlatMapper(f)
}

