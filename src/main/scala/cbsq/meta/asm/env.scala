package cbsq.meta.asm














@annotation.experimental
def ??? = {
   throw NotImplementedError()
}

object utilityImplicits {
   
   extension (o: java.io.Writer) {

      def asPrintWriter() : java.io.PrintWriter = {
         new java.io.PrintWriter(o, true)
      }

   }

   extension [CC[A] <: collection.SeqOps[A, CC, CC[A]], E](s: CC[E]) {

      def unfolding[Count](v0: Count)(f: (Count, E) => Count): CC[(E, Count)] = {
         val s1 = s.to(LazyList)
         Iterator.unfold[(E, Count), (Count, LazyList[E])]((v0, s1))({
            case (state0, presentlyItem +: remainingItems) =>
               val state1 = f(state0, presentlyItem)
               Some(((presentlyItem, state1): (E, Count), (state1, remainingItems)))
         })
         .to(s.iterableFactory )
         // ???
      }
      
   }

}

export utilityImplicits._

@deprecated
def stdOutWriter(o: java.io.OutputStream) : java.io.Writer = {
   new java.io.OutputStreamWriter(o)
}

export cbsq.meta.util.PwEmitter












export cbsq.meta.asm.jvm.ow


















