package com.github.benhutchison.typeclasses

import com.github.benhutchison.typeclasses.OrderingExample.SuburbRating

package object implicit_scope {

  implicit val clientOrdering: Ordering[SuburbRating] =
    Ordering.by(_.hashCode())

}
