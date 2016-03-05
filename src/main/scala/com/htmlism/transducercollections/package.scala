package com.htmlism

package object transducercollections {
  /**
    * A more concise way of expressing a function that reduces
    *
    * Type parameter order differs from https://github.com/knutwalker/transducers-scala
    *
    * @tparam X The accumulation type
    * @tparam A The element type
    */

  type Reducer[X, A] = (X, A) => X

  type TransducerType[A, B] = (Reducer[X, B] => Reducer[X, A]) forSome { type X }
}

