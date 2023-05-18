



import cbsq.meta.asm.jvm.MethodDescriptorImpl1
import cbsq.meta.asm.jvm.JbltOpdStackState




JbltOpdStackState.empty


val JbltOpdStackState.empty = JbltOpdStackState.empty


val JbltOpdStackState.byFromLeftRightwards(vals2) = {
   JbltOpdStackState.empty
}


val JbltOpdStackState.byFromLeftRightwards(vals3) = {
   JbltOpdStackState.byFromLeftRightwards(IndexedSeq("bar") )
}


val jblc = JbltOpdStackState.getClass()
({
   import language.unsafeNulls
   jblc
   .getMethods().toIndexedSeq
})
({
   import language.unsafeNulls
   jblc
   .getDeclaredMethods().toIndexedSeq
})




JbltOpdStackState.empty pushed 3 pushed 2 pushed 3
JbltOpdStackState.empty pushed 3 pushed 3 pushed 3
JbltOpdStackState.empty pushed 3 pushed 5 pushed 3
JbltOpdStackState.empty pushed 3 pushed 5 pushed 5
val r11 = (JbltOpdStackState.empty pushed 3 pushed 5).poppedOne()
r11._1.poppedOne()
(util.Try {
   r11._1.poppedOne()._1.poppedOne()
})
.failed
.get




