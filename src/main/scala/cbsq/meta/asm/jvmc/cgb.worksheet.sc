


import cbsq.meta.asm.jvm.MethodDescriptorImpl1
import cbsq.meta.asm.jvm.JbltOpdStackState
import cbsq.meta.asm.jvm.FqnStronumericPair


import cbsq.meta.asm.jvmc.Jblt



Jblt.getClass.getDeclaredMethods().nn.toIndexedSeq

Jblt.OpdState.getClass.getMethods().nn.toIndexedSeq

val s2 = (
   Jblt.OpdState[FqnStronumericPair[?] ](
      opdStack = JbltOpdStackState.empty,
      storage = IndexedSeq() ,
      lastItemgenState = ("lclval", 2) ,
   )
)

s2.opdStack

s2.afterLdcOpaque

s2.afterLdcOpaque.afterLdcOpaque

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
s22
.afterPopoff
.afterPopoff
.afterLdcOpaque

(util.Try {
   s22
   .storage
   .updated(index = 3, elem = ("lclval", 21) )
} )
.failed
.get
"it threw an " + (
   classOf[IndexOutOfBoundsException]
   .getSimpleName().nn
)






