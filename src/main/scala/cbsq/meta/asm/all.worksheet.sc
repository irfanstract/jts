



org.objectweb.asm.Type.getType("()V")

cbsq.meta.asm.ow.Type.getType("(IJ)V")

cbsq.meta.asm.ow.Type.getType("(V)V")

util.Try {
   java.lang.invoke.MethodType.fromMethodDescriptorString("(V)V", null)
}

util.Try {
   java.lang.invoke.MethodType.fromMethodDescriptorString("()[V", null)
}

util.Try {
   org.objectweb.asm.Type.getType("(V)V")
   .getArgumentTypes()
}

{
   import  cbsq.meta.asm.jvmc.*
   Esig("Ljava/lang/Runnable;")
   // .asFuturified
   .asMonoFuturified
}
{
   import  cbsq.meta.asm.jvmc.*
   Esig("Ljava/lang/Runnable;")
   // .asFutureMonadified
   .asMonoFutureMonadified
}

{
   import  cbsq.meta.asm.jvmc.*
   trait L1[C[A] <: Eoft[A] ] 
}

math.random()

{
   import  cbsq.meta.asm.jvmc.*
   Esig("()Ljava/lang/Runnable;")
   // .asFutureMonadified
   .withMappedReturnType([E <: OwtBase ] => (e: E) => e.asMonoFutureMonadified )
}
{
   import  cbsq.meta.asm.jvmc.*
   Esig("(Z)[J").withMappedReturnType([E <: OwtBase ] => (e: E) => e.asMonoFutureMonadified )
}
{
   import  cbsq.meta.asm.jvmc.*
   ow.Type.getType("(Z)[J").withMappedReturnType([E <: OwtBase ] => (e: E) => e.asMonoFutureMonadified )
}
{
   import  cbsq.meta.asm.jvmc.*
   Esig("()V").withMappedReturnType([E <: OwtBase ] => (e: E) => e.asMonoFutureMonadified )
}


