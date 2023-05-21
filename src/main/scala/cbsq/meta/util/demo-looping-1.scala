package cbsq.meta.util








object Looping1 {
   
   def run(): Unit = {
   for (i <- Range(0, 5) ) {
      import java.lang.ref.Reference.reachabilityFence
      reachabilityFence(Seq )
      reachabilityFence(Set )
      reachabilityFence(i )
   }
   }
   
}



