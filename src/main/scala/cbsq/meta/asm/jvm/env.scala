package cbsq.meta.asm.jvm









/**
 * 
 * a subset of `org.objectweb.asm` ;
 * would-have-been *all of them* but turns-out failing ("Implementation Restriction"),
 * hence limited to a subset
 * 
 */
@deprecated("not a complete re-export ; consider directly importing 'org.objectweb.asm'")
object ow {
   
   import org.objectweb.asm

   // export asm.{* }
   
   object Opcodes {
      export asm.Opcodes.*
   }
   
   export asm.ModuleVisitor
   export asm.ClassVisitor
   export asm.ClassReader
   export asm.ClassWriter
   export asm.AnnotationVisitor
   export asm.MethodVisitor
   
   export asm.Attribute
   export asm.ConstantDynamic
   type Handle = asm.Handle
   object Handle {
      export asm.Handle.*
   }
   type Type = asm.Type
   object Type {
      export asm.Type.*
   }

}

extension [A <: org.objectweb.asm.signature.SignatureVisitor](scv: A) {

   inline
   def visitXSig(a: String) = {
            import org.objectweb.asm
            new asm.signature.SignatureReader(a)
            .accept({
               scv
            } : asm.signature.SignatureVisitor )
   }

   // inline
   def visitXSigReturnType(sig: String) = {
            require(0 < sig.length(), s"[visitXSigReturnType] unsupported empty sig")
            import org.objectweb.asm
            /**
             * 
             * the call
             * might unexpectedly be on a "value-type" (rather than a "function-type"),
             * must handle both cases
             * 
             */
            ({
               if sig startsWith "(" then
                  new asm.signature.SignatureVisitor (asm.Opcodes.ASM9) {
                     override
                     def visitReturnType() = {
                        scv
                     }
                  }
               else scv
            } : asm.signature.SignatureVisitor)
            .visitXSig(sig)
   }

}

type VarianceChar
   >: ('+' | '-' | '=' )
   <: ('+' | '-' | '=' )








