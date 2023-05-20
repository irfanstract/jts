package cbsq.meta.asm.jvmc

















object Natification {

   enum Amd {
      case forNative
      case forAbstract
   }
   
}

extension (dest: org.objectweb.asm.ClassVisitor) {

   def asNatified(
      makeAbstract: Boolean = false,
      
   ) : org.objectweb.asm.ClassVisitor = {
      import org.objectweb.asm
      import asm.Opcodes
      
      val amd = {
         import Natification.Amd
         if (makeAbstract) Amd.forAbstract
         else Amd.forNative
      } : Natification.Amd
      new asm.ClassVisitor(asm.Opcodes.ASM9, dest) {
         
         override
         def visitMethod(access: Int, name: String | Null, descriptor: String | Null, signature: String | Null, exceptions: Array[String | Null] | Null): asm.MethodVisitor | Null = {
            val visitor0 = (
               super.visitMethod(({
                  (access & ~(Opcodes.ACC_ABSTRACT | Opcodes.ACC_NATIVE) )
                  .`|`({
                     import Natification.Amd
                     amd match
                        case Amd.forAbstract => 
                           Opcodes.ACC_ABSTRACT
                        case Amd.forNative => 
                           if (access.&(Opcodes.ACC_ABSTRACT) != 0 ) Opcodes.ACC_ABSTRACT
                           else Opcodes.ACC_NATIVE
                     
                  })
               }), name, descriptor, signature, exceptions).nn
            )
            /**
             * 
             * a new MethodVisitor which only propagates calls which are not `visitCode`-specific
             * (still calls `visitEnd`, tho)
             * 
             */
            newOwNtAbstractifyingVisitor(visitor0)
         }
      }
   }
   
}



/**
 * 
 * the new MethodVisitor only propagates calls which are not `visitCode`-specific .
 * (still calls `visitEnd` as it's not `visitCode`-specific)
 * 
 */
def newOwNtAbstractifyingVisitor(visitor0: org.objectweb.asm.MethodVisitor): org.objectweb.asm.MethodVisitor = {
            import org.objectweb.asm
            new asm.MethodVisitor(asm.Opcodes.ASM9) {

               import asm.*

               override
               def visitAttribute(attribute: Attribute | Null): Unit = {
                  visitor0 visitAttribute attribute
               }

               /* annotations may affect runtime behv */
               override
               def visitAnnotation(descriptor: String | Null, visible: Boolean): AnnotationVisitor | Null = {
                  visitor0 visitAnnotation(descriptor, visible)
               }

               /* annotations may affect runtime behv */
               override
               def visitTypeAnnotation(typeRef: Int, typePath: TypePath | Null, descriptor: String | Null, visible: Boolean): AnnotationVisitor | Null = {
                  visitor0 visitTypeAnnotation(typeRef, typePath, descriptor, visible)
               }

               /* annotations may affect runtime behv */
               override
               def visitParameterAnnotation(parameter: Int, descriptor: String | Null, visible: Boolean): AnnotationVisitor | Null = {
                  visitor0 visitParameterAnnotation(parameter, descriptor, visible)
               }

               /* no-op, as stated */
               override
               def visitCode(): Unit = {}

               /* essential */
               override
               def visitEnd(): Unit = {
                  import language.unsafeNulls
                  visitor0.visitEnd()
               }
               
            }
}










