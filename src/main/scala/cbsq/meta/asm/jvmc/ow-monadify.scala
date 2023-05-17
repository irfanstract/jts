package cbsq.meta.asm.jvmc















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
      import org.objectweb.asm
      import cbsq.meta.asm.jvm.*
      /**
       * 
       * whether the output would-be `.d.ts` file,
       * rather than regular `.ts` or `.js`
       * 
       */
      val isForTypeDeclarationFile : Boolean = true
      new asm.ClassVisitor(asm.Opcodes.ASM9, dest) {


         lazy val forInterfaces : Boolean = {
            forInterfacesP.future
            .value.getOrElse(throw IllegalStateException("NF") ).get
         }
         val forInterfacesP = concurrent.Promise[Boolean]

         override
         def visit(version: Int, access: Int, name: String | Null, signature: String | Null, superName: String | Null, interfaces: Array[String | Null] | Null): Unit = {
            import asm.Opcodes
            forInterfacesP
            .success((
               access.&(Opcodes.ACC_INTERFACE | Opcodes.ACC_ANNOTATION) 
               != 0
            ))
            super.visit(version, access, name, signature, superName, interfaces)
         }

         override
         def visitMethod(access: Int, name: String | Null, descriptor: String | Null, givenSignature: String | Null, exceptions: Array[String | Null] | Null): asm.MethodVisitor | Null = {
            //
            try {
               cv.nn
               .visitMethod(access, name, {
                  descriptor
               }, {
                  givenSignature
               }, exceptions)
            } finally {
               val signature = (
                  Some(givenSignature).collect({ case e : String => e })
                  .orElse[String](Some(descriptor.nn) )
                  .getOrElse(throw IllegalArgumentException("both null"))
               )
               /**
                * 
                * *the descriptor* and *the signature* needs to be consistent ;
                * the *compute the signature* needs to happen first, and then
                * do *compute the erasure of the obtained signt*
                * 
                * here's *the signature*
                * 
                */
               val asyncifiedSignature = ({
                  ({
                     Esig(signature)
                     .asReturnValueFutureMonadified
                     .value
                  })
               })
               /**
                * 
                * here's *the type-erased signature*
                * 
                */
               val asyncifiedNd = {
                  Esig(asyncifiedSignature)
                  .asErased()
                  .value
               }
               /**
                * 
                * not all methods deserve one
                * (eg *synthetic methods*, `&lt;clinit>`, *package-private* ones, `equals`, `hashCode`, `finalize`), and
                * different methods may need differentiated mangling-scheme
                * (eg `&lt;init>`, `of`, `from` )
                * 
                */
               name match {

                  /**
                   * 
                   */
                  case m if (isForTypeDeclarationFile && access.&(asm.Opcodes.ACC_SYNTHETIC) != 0 ) =>

                  case "<clinit>" =>

                  case m if (isForTypeDeclarationFile && access.&(asm.Opcodes.ACC_PROTECTED | asm.Opcodes.ACC_PUBLIC) == 0 ) =>

                  case m if (isForTypeDeclarationFile && access.&(asm.Opcodes.ACC_BRIDGE) != 0 ) =>

                  case "<init>" =>
                     cv.nn
                     .visitMethod(access | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_NATIVE, "$asyncNew", (
                        asyncifiedNd
                     ), asyncifiedSignature, Array.empty ).nn
                     .visitEnd()

                  case "clone" | "equals" | "toString" | "hashCode" =>

                  case "close" | "dispose" | "finalize" if isForTypeDeclarationFile =>

                  case _ =>
                     import language.unsafeNulls
                     cv.nn
                     .visitMethod(access | asm.Opcodes.ACC_NATIVE, name + {
                        name match
                           case s if (s.length() < 5 ) =>
                              "Async"
                           case _ =>
                              "Asynchronously"
                        
                     }, (
                        asyncifiedNd
                     ), asyncifiedSignature, Array.empty ).nn
                     .visitEnd()
                           
               }
            }
         }
         
      }
   }

}










