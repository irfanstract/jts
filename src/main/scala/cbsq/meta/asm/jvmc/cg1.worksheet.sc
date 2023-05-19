


import cbsq.meta.asm.jvm.MethodDescriptorImpl1
import cbsq.meta.asm.jvm.JbltOpdStackState
import cbsq.meta.asm.jvm.FqnStronumericPair


import cbsq.meta.asm.jvmc.Jblt


import org.objectweb.asm



val s2 = (
   Jblt.OpdState[FqnStronumericPair[?] ](
      opdStack = JbltOpdStackState.empty,
      storage = IndexedSeq() ,
      lastItemgenState = ("lclval", 2) ,
   )
)

val s21 = (
   s2
   .afterLdcOpaque
   .afterLdcOpaque
   .afterPopoff
)
val s22 = (
   s21
   .afterLdcOpaque
   .afterLdcOpaque
   .afterLdcOpaque
)
val s23 = (
   s22
   .afterPopoff
   .afterPopoff
   .afterLdcOpaque
)



trait Em {

   @annotation.experimental
   def apply(): Any 

   def applyAlt() = {
      this match {
      case c =>
         import scala.language.unsafeNulls
         import java.lang.invoke.{MethodHandles, MethodType}
         MethodHandles.lookup()
         .bind(c, "apply", MethodType.fromMethodDescriptorString("()Ljava/lang/Object;", null ) )
         .invokeWithArguments()
      }
   }

}



import cbsq.meta.asm.jvmc.wsnImplCtx1
import cbsq.meta.asm.jvmc.InOpdCtx


val ciw = wsnImplCtx1()
val mockupOpc = {
   new InOpdCtx {
         val operandStackPrefix      : String = "lval$2"
         val returnValueStackPrefix  : String = "lval$3"
   }
}
((new Em {

@annotation.experimental
def apply() = {
   ciw.eRpkImpl.toXJsString21(

      opc = { import asm.Opcodes.* ; INVOKEVIRTUAL } ,
      odst = {
         MethodDescriptorImpl1(access = {
            import asm.Opcodes.*
            ACC_PUBLIC   
         } , name = "apply", descriptor0 = MethodDescriptorImpl1.Bds.apply(descriptor = "()V", signature0 = null) )
      } ,
      opdState0 = s23 ,

      documentOriginalSrc = true ,
      async = false ,

   )(using mockupOpc) match {
      case o =>
         (o.transliteratedForm , o.resultingOpdState)
   }
}

}) : Em).applyAlt()
((new Em {

@annotation.experimental
def apply() = {
   ciw.eRpkImpl.toXJsString21(

      opc = { import asm.Opcodes.* ; INVOKEVIRTUAL } ,
      odst = {
         MethodDescriptorImpl1(access = {
            import asm.Opcodes.*
            ACC_PUBLIC   
         } , name = "apply", descriptor0 = MethodDescriptorImpl1.Bds.apply(descriptor = "(IJ)Ljava/lang/Object;", signature0 = null) )
      } ,
      opdState0 = s23 ,

      documentOriginalSrc = true ,
      async = false ,

   )(using mockupOpc) match {
      case o =>
         (o.transliteratedForm , o.resultingOpdState)
   }
}

}) : Em).applyAlt()


"/"






