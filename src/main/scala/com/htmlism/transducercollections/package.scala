package com.htmlism

package object transducercollections {
  type Reducer[R, A] = (R, A) => R
}
