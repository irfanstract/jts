package cbsq.meta.asm.jvm


















object jblss {
   
opaque type JbltOpdStackState[+E]
   <: AnyRef with Matchable
   = IndexedSeq[E]

object JbltOpdStackState {

   extension [E](s: JbltOpdStackState[E] ) {

      def fromRightLeftwards: IndexedSeq[E] = s.reverse

      def fromLeftRightwards: IndexedSeq[E] = s

   }

   extension [E](s0: JbltOpdStackState[E] ) {

      /**
       * 
       * always on the right-hand end
       * 
       */
      def pushed[E1 >: E](v: E1): JbltOpdStackState[E1] = {
         (s0: IndexedSeq[E]) :+ v
      }

      /**
       * 
       * always on the right-hand end
       * 
       */
      def pushedAll[E1 >: E](v: Seq[E1]): JbltOpdStackState[E1] = {
         (s0: IndexedSeq[E]) :++ v
      }

   }

   extension [E](s0: JbltOpdStackState[E] ) {

      /**
       * 
       * always from the right-hand end
       * 
       */
      def poppedNImpl(n: Int): (JbltOpdStackState[E], IndexedSeq[E]) = {
         s0 splitAt(s0.length + -n )
      }

   }

   export jblssExtraOps.{poppedN, poppedOne}

   object byFromLeftRightwards {
      
      def apply[E](values: IndexedSeq[E]): JbltOpdStackState[E] = {
         values
      }

      def unapply[E](e: JbltOpdStackState[E]) = {
         Some(e : IndexedSeq[E])
      }

   }

   lazy val empty = {
      byFromLeftRightwards(IndexedSeq() )
   }
   
}

}

object jblssExtraOps {

   export jblss.JbltOpdStackState
   
   /**
    * 
    * extracted out, to avoid (unexpected) compiler-done dealiasing
    * 
    */
   extension [E](s0: JbltOpdStackState[E] ) {

      /**
       * 
       * always from the right-hand end
       * 
       */
      def poppedN(n: Int): (JbltOpdStackState[E], IndexedSeq[E]) = {
         s0 poppedNImpl(n)
      }

      /**
       * 
       * always from the right-hand end
       * 
       */
      def poppedOne(): (JbltOpdStackState[E], E) = {
         s0.poppedN(1) match {

            case (s1, Seq(poppedV)) =>
               (s1, poppedV)

            /* replace the bugs-only `MatchError` with `NoSuchElementException` */
            case (_, Seq() ) =>
               throw java.util.NoSuchElementException()

         }
      }

   }

}

export jblss.JbltOpdStackState

// sealed
// case class FqnStronumericPair[+E <: String | BigInt](elems: IndexedSeq[E])
// {
// 
//    override
//    def toString(): String = {
//       elems.mkString(".")
//    }
// 
//    def :+[E1 >: E <: String | BigInt](newElem: E1): FqnStronumericPair[E1] = {
//       FqnStronumericPair[E1](elems = elems :+ newElem)
//    }
// 
// }
type FqnStronumericPair[_] = (
   // Tuple.Append[String *: EmptyTuple, Int]
   (String, Int)
)










































































