package com.htmlism.transducercollections

/**
  * Describes not modifying the incoming transducer.
  *
  * @tparam A The element being iterated upon
  */

class Echo[A] extends EasyTransducer[A, A] {
  def apply[X](red: Reducer[X, A]): Reducer[X, A] = red
}

