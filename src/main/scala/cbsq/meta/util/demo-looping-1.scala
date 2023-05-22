package cbsq.meta.util








object Looping1 {
   
   def apply(): Range = {
      var currentVal: Int = 0
      
      while (((currentVal, currentVal += 1 )._1 ) < 5) {
         import java.lang.ref.Reference.{reachabilityFence }
         reachabilityFence(currentVal )
         reachabilityFence(Seq )
         reachabilityFence(Set )
      }

      Range(0, currentVal )
   }
   
   def apply(arg: String ): Range = {
      var currentVal: Int = 0

      while (((currentVal, currentVal = ((1.8 * currentVal ).floor + 1 ).toInt )._1 ) < 35) {
         import java.lang.ref.Reference.{reachabilityFence }
         reachabilityFence(currentVal )
         reachabilityFence(Seq )
         reachabilityFence(Set )
      }

      Range(0, currentVal )
   }
   
}



