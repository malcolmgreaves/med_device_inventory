import scalaz._

package object meddev {

  /** A non-negative integer type representing an index. */
  type Index = Int @@ Index.T
  object Index {

    /** Size constructor: throws Exception if input is negative. */
    def apply(x: Int): Index = {
      if (x >= 0) Tag[Int, T](x)
      else
        throw new IllegalArgumentException(
          s"Index must be non-negative, not $x")
    }

    /** Size value accessor. */
    def apply(x: Index): Int = Tag.unwrap(x)

    /** The Size phantom type. */
    sealed trait T
  }
//  type Index = Int
//  object Index {
//
//    /** Size constructor: throws Exception if input is negative. */
//    def apply(x: Int): Int = {
//      if (x >= 0) x
//      else
//        throw new IllegalArgumentException(
//          s"Index must be non-negative, not $x")
//    }
//  }

}
