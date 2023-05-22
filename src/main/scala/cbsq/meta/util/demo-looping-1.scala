package cbsq.meta.util








object Looping1 {
   
   def apply(): Range = {
      var currentVal: Int = 0
      while ((try { currentVal } finally { currentVal += 1 } ) < 5) {
         import java.lang.ref.Reference.{reachabilityFence }
         reachabilityFence(currentVal )
         reachabilityFence(Seq )
         reachabilityFence(Set )
      }
      Range(0, currentVal )
   }
   
}



