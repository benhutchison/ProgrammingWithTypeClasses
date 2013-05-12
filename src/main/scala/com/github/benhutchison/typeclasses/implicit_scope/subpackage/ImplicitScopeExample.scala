package com.github.benhutchison.typeclasses.implicit_scope
package subpackage

import com.github.benhutchison.typeclasses.OrderingExample

object ImplicitScopeExample extends App {

  //We've learned about the default implicit scope provided by companion
  //objects.
  // But which companion objects does the compiler search in?

  //The answer is all "parts" of a type. eg for Ordering[SuburbRating]
  //the compiler will search the companions of Ordering and
  //OrderingExample.SuburbRating. This is sometimes called 'Provider Scope'

  //But what about if neither of these is under our control,
  // eg if they're both defined by someone else's library?

  //Scala also allows clients to define ambient implicits that are
  //in scope for an entire package, or sub-tree of packages

  //uses hashCode ordering defined in parent package 
  
  println(OrderingExample.myRatings.sorted.map(
    (r) => (r, r.hashCode())).mkString("\n"))

}
