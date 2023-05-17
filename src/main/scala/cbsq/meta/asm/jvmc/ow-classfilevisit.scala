package cbsq.meta.asm.jvmc


















trait Wsn {

   type Derived <: Wsn

   def withNewFullName(
      ownName            : ow.Type ,
      superclassName     : ow.Type,
      superInterfaces    : IndexedSeq[ow.Type]
   ): Derived

   /**
    * 
    * ```
    * 
    * // TS Type Defs
    * equals&lt;That>(thatOne: That ): boolean ;
    * 
    * // Scala
    * def equals[That](thatOne: That ): Boolean ;
    * 
    * ```
    * 
    */
   type NativeSig

   def translateIntoNativeSig(sig: WSourceLevelSig ) : NativeSig

   def withAddedMethodByNativeSig(sig: NativeSig): Derived

   /**
    * 
    */
   @deprecated
   def withAddedMethod(sig: WMNs) : Derived

   /**
    * argument to `withAddedMethod`
    */
   type WMNs

   val distilledFormPwEmitter: WsnPwEmitter

}

type WSourceLevelSig
   >: cbsq.meta.asm.jvm.MethodDescriptorImpl1
   <: cbsq.meta.asm.jvm.MethodDescriptorImpl1

export cbsq.meta.util.{PwEmitter as WsnPwEmitter }











































