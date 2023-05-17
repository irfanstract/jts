package cbsq.meta.asm.jvm









export cbsq.meta.asm.ow

extension [A <: org.objectweb.asm.signature.SignatureVisitor](scv: A) {

   inline
   def visitXSig(a: String) = {
            new org.objectweb.asm.signature.SignatureReader(a)
            .accept({
               scv
            } : org.objectweb.asm.signature.SignatureVisitor )
   }

   // inline
   def visitXSigReturnType(sig: String) = {
            require(0 < sig.length(), s"[visitXSigReturnType] unsupported empty sig")
            /**
             * 
             * the call
             * might unexpectedly be on a "value-type" (rather than a "function-type"),
             * must handle both cases
             * 
             */
            ({
               if sig startsWith "(" then
                  new org.objectweb.asm.signature.SignatureVisitor (ow.Opcodes.ASM9) {
                     override
                     def visitReturnType() = {
                        scv
                     }
                  }
               else scv
            } : org.objectweb.asm.signature.SignatureVisitor)
            .visitXSig(sig)
   }

}

type VarianceChar
   >: ('+' | '-' | '=' )
   <: ('+' | '-' | '=' )








