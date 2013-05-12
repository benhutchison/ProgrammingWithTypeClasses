package com.github.benhutchison.typeclasses

object HeterogeneousCollectionExample extends App {
  
  trait Show[T] {
    def show(value: T): String
  }
  
  implicit def showString = new Show[String] {
	  def show(s: String) = s
  }
  
  implicit def showInt = new Show[Int] {
    def show(i: Int) = i.toString
  }
  
  class Showable[T](value: T)(implicit instance: Show[T]) {
    def show = instance.show(value)
  }
  
  //the underscore in Seq[Showable[_]] denotes an existential type
  
  val showables: Seq[Showable[_]] = Seq(new Showable("a string"), new Showable(42))
  
  println(showables.map(_.show))
  

}