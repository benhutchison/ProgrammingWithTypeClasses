package com.github.benhutchison

package object typeclasses {

  def assertEqual[T](expected: T, actual: T) {
    if (expected != actual)
      throw new AssertionError(s"Expected: ${expected}   \nActual: ${actual}")
  }
}