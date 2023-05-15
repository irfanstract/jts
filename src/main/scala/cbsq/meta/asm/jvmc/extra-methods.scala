package cbsq.meta.asm.jvmc















extension (dest: org.objectweb.asm.ClassVisitor) {

   def withJsSpecificMethods(): org.objectweb.asm.ClassVisitor = {
      new org.objectweb.asm.ClassVisitor(ow.Opcodes.ASM9, dest) {

         // override
         // def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): ow.MethodVisitor = {
         //    ???
         // }
         
         override
         def visitEnd(): Unit = {
            try {
               ({
                  val v1 = (
                     cv.visitMethod({
                        import org.objectweb.asm.Opcodes
                        Opcodes.ACC_PUBLIC
                     }, "toLocaleString", "()Ljava/lang/String;", null, Array.empty )
                  )
                  v1.visitEnd()
               })
               ({
                  val v1 = (
                     cv.visitMethod({
                        import org.objectweb.asm.Opcodes
                        Opcodes.ACC_PROTECTED | Opcodes.ACC_SYNTHETIC
                     }, "2$meta$toStringTag", "()Ljava/lang/String;", null, Array.empty )
                  )
                  v1.visitEnd()
               })
            }
            finally super.visitEnd()
         }

      }
   }

}





















