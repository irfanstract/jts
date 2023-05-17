


import cbsq.meta.asm.jvmc.*


val dsc1 = ({
   MethodDescriptorImpl1(access = {
      import ow.Opcodes.*
      ACC_STATIC & ACC_PUBLIC
   }, name = "reachabilityFence", "(Ljava/lang/Object;)V", "(Ljava/lang/Object;)V" )
})

dsc1.toShortString()(using MethodDescriptorImpl1.Fmtct(generics = true))

{
   import basicCctsi.*
   (dsc1.computeCompiledArgsTypeName(), dsc1.computeCompiledReturnTypeName())
}

