package cbsq.meta.asm.jvmc












type OwtBase
   >: org.objectweb.asm.Type | Esig
   <: org.objectweb.asm.Type | Esig

type Eoft[A] <: OwtBase = A match {
   case org.objectweb.asm.Type =>
        org.objectweb.asm.Type
   case Esig =>
        Esig
}

extension [A <: OwtBase](sig0: A) {
   
   /**
    * 
    * `A1.class monoAppliedToGenericType A2.class`
    * would yield `A1&lt;? extends A2>.class`
    * 
    * 
    * @param variance override the default variance (which's `+`)
    * 
    */
   def monoAppliedToGenericType(
      sv: org.objectweb.asm.Type ,

      variance: '+' | '-' | '=' = '+' ,

   ): Eoft[A] = {
      sig0 match {

         case sig0 : org.objectweb.asm.Type =>
            /**
             * oh, and JVM *types* don't maintain type-arguments, so
             * we'll have to ignore `sig0` XD
             */
            sv
            
         case sig0 : Esig =>
            val Esig(sig) = sig0

            val scv = (
               new org.objectweb.asm.signature.SignatureWriter()
            )
            
            scv visitClassType(sv.getInternalName() )
            (scv visitTypeArgument(variance ) )
               .visitXSig(sig )
            scv.visitEnd()
            
            Esig(scv.toString() )
            
      }
   }

   def withMappedReturnType(applyTx: [A1 <: OwtBase] => A1 => Eoft[A1] ): Eoft[A] = {
      sig0 match {

         case type1 : org.objectweb.asm.Type =>
            /**
             * oh, and JVM *types* don't maintain type-arguments, so
             * we'll have to ignore `sig0` XD
             */
            type1.getSort() match {
               
               case ow.Type.METHOD =>
                  val returnType = (
                     type1.getReturnType()
                  )
                  val argTypes = (
                     type1.getArgumentTypes()
                     .toIndexedSeq
                  )
                  org.objectweb.asm.Type.getMethodType((
                     applyTx[org.objectweb.asm.Type](returnType)
                  ), argTypes.toArray : _* )
                  
            }
            
         case sig0 : Esig =>
            val analysis = (
               sig0.analyse()
            )
            import analysis.sig
            
            import analysis.pt0
            
            import analysis.rt0
            
            val rt1 = applyTx[Esig](rt0 )
            
            Esig(s"${pt0.value.dropRight(1) }${rt1.value }" )
            
      }
   }

}

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
   def asMonoFuturified : Eoft[A] = {
      sig0 monoAppliedToGenericType jFuturesOwClassTags.ofFutures
   }
   
   /**
    * 
    * `A` become `CompletionStage&lt;A>`
    * 
    */
   def asMonoFutureMonadified : Eoft[A] = {
      sig0 monoAppliedToGenericType jFuturesOwClassTags.ofCompletionStages
   }
   
   /**
    * 
    * `(...)A` become `(...)CompletionStage&lt;A>`
    * 
    */
   def asReturnValueFutureMonadified : Eoft[A] = {
      sig0 withMappedReturnType([A <: OwtBase] => (t: A) => t.asMonoFutureMonadified )
   }
   
   /**
    * 
    * `A` become `Flow.Publisher&lt;A>`
    * 
    */
   def asMonoFlewified : Eoft[A] = {
      sig0 monoAppliedToGenericType jFuturesOwClassTags.ofFlowPublisher
   }
   
}

extension (dest: org.objectweb.asm.ClassVisitor) {

   /**
    * 
    * this `ClassVisitor`
    * adds, for each method, an "async" variant of it (eg returning `Future` or `CompletionStage`)
    * 
    * important since
    * the W3C's threading-model *require explicit async-and-await syntax*
    *
    */
   def asMakingAsyncMonadifiedVariants() : org.objectweb.asm.ClassVisitor = {
      new org.objectweb.asm.ClassVisitor(ow.Opcodes.ASM9, dest) {

         
         override
         def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): ow.MethodVisitor = {
            try {
               cv.visitMethod(access, name           , {
                  descriptor
               }, {
                  signature
               }, exceptions)
            } finally {
               val asyncifiedNd = {
                  ow.Type.getType(descriptor)
                  .asReturnValueFutureMonadified
                  .getDescriptor()
               }
               val asyncifiedSignature = (
                  Option(signature)
                  .map(signature => {
                     Esig(signature)
                     .asReturnValueFutureMonadified
                     .value
                  })
                  .orNull 
               )
               name match {

                  case "<clinit>" =>

                  case "<init>" =>
                     cv
                     .visitMethod(access | ow.Opcodes.ACC_STATIC | ow.Opcodes.ACC_NATIVE, "$asyncNew", (
                        asyncifiedNd
                     ), asyncifiedSignature, Array.empty )
                     .visitEnd()

                  case "clone" | "equals" | "toString" | "hashCode" =>

                  case _ =>
                     cv
                     .visitMethod(access | ow.Opcodes.ACC_NATIVE, name + "Asynchronously", (
                        asyncifiedNd
                     ), asyncifiedSignature, Array.empty )
                     .visitEnd()
                           
               }
            }
         }
         
      }
   }

}










