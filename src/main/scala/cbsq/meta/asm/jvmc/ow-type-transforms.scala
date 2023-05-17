package cbsq.meta.asm.jvmc















type OwtBase
   >: org.objectweb.asm.Type | Esig
   <: org.objectweb.asm.Type | Esig



type OwtMapped[A <: OwtBase] <: OwtBase = 
   A match {
      case org.objectweb.asm.Type =>
         org.objectweb.asm.Type
      case Esig =>
         Esig
   }



@deprecated("naming")
type Eoft[A <: OwtBase] = OwtMapped[A]
   


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

      variance: cbsq.meta.asm.jvm.VarianceChar = '+' ,

   ): OwtMapped[A] = {
      sig0 match {

         case sig0 : org.objectweb.asm.Type =>
            /**
             * oh, and JVM *types* don't maintain type-arguments, so
             * we'll have to ignore `sig0` XD
             */
            sv
            
         case sig0 : Esig =>
            import language.unsafeNulls
            
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

   def withMappedReturnType(applyTx: [A1 <: OwtBase] => A1 => OwtMapped[A1] ): OwtMapped[A] = {
      sig0 match {

         case type1 : org.objectweb.asm.Type =>
            /**
             * oh, and JVM *types* don't maintain type-arguments, so
             * we'll have to ignore `sig0` XD
             */
            type1.getSort() match {
               
               case ow.Type.METHOD =>
                  import language.unsafeNulls
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




































