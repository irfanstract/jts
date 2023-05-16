package cbsq.meta.asm.jvmc















object jPairsOwClassTags {
   
   val ofTuples = (
      /* a fictituous type `java/lang/Tuple` being variadic */
      ow.Type.getObjectType("java/lang/Tuple")
   )
   
}



object jFuturesOwClassTags {

   val ofFutures = (
      // ow.Type.getObjectType("java/util/concurrent/Future")
      ow.Type.getType(classOf[java.util.concurrent.Future[?] ] )
   )
   
   val ofCompletionStages = (
      ow.Type.getType(classOf[java.util.concurrent.CompletionStage[?] ] )
   )
   
   val ofFlowPublisher = (
      ow.Type.getType(classOf[java.util.concurrent.Flow.Publisher[?] ] )
   )
   
}


extension [A <: OwtBase](sig0: A) {

   /**
    * 
    * `A` become `Future&lt;A>`
    * 
    */
   def asMonoFuturified : OwtMapped[A] = {
      import language.unsafeNulls
      sig0 monoAppliedToGenericType jFuturesOwClassTags.ofFutures
   }
   
   /**
    * 
    * `A` become `CompletionStage&lt;A>`
    * 
    */
   def asMonoFutureMonadified : OwtMapped[A] = {
      import language.unsafeNulls
      sig0 monoAppliedToGenericType jFuturesOwClassTags.ofCompletionStages
   }
   
   /**
    * 
    * `(...)A` become `(...)CompletionStage&lt;A>`
    * 
    */
   def asReturnValueFutureMonadified : OwtMapped[A] = {
      sig0 withMappedReturnType([A <: OwtBase] => (t: A) => t.asMonoFutureMonadified )
   }
   
   /**
    * 
    * `A` become `Flow.Publisher&lt;A>`
    * 
    */
   def asMonoFlewified : OwtMapped[A] = {
      import language.unsafeNulls
      sig0 monoAppliedToGenericType jFuturesOwClassTags.ofFlowPublisher
   }
   
}



























































