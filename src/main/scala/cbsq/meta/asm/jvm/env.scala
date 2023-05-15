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
            new org.objectweb.asm.signature.SignatureVisitor (ow.Opcodes.ASM9) {
               override
               def visitReturnType() = {
                  scv
               }
            }
            .visitXSig(sig)
   }

}

/**
 * 
 * alternative to `org.objectweb.asm.Type`
 * generalised to all *signature*s
 * 
 */
sealed
case class Esig(value: String) {

   value.nn

   override
   def toString(): String = s"type $value"

}

object Esig {

extension (sig0 : Esig) {

   def asErased(): Esig = {
            val rscv = (
               new org.objectweb.asm.signature.SignatureWriter() {
                  
                  /**
                   * overridden to return an abandoned visitor
                   */
                  override
                  def visitTypeArgument() = {}

                  /**
                   * overridden to return an abandoned visitor
                   */
                  override
                  def visitTypeArgument(variance: Char) = {
                     /**
                      * dummy `SignatureVisitor` to be abandoned
                      */
                     new org.objectweb.asm.signature.SignatureWriter()
                  }
                  
               }
            )
            rscv visitXSig sig0.value
            Esig(rscv.toString())
   }
   
}

extension (sig0 : Esig) {

   def analyse() = {
      esigAnalyseImpl(sig0)
   }

   def getParameterSig() = {
      
            val Esig(sig) = sig0

            /**
             * to be used to distill the parameter-type
             */
            val pscv =  {
               new org.objectweb.asm.signature.SignatureWriter() {

                  /**
                   * `visitReturnType` -
                   * overridden to return an abandoned visitor
                   */
                  override
                  def visitReturnType() = {
                     ({
                        val t = super.visitReturnType()
                        t.visitBaseType('V')
                     })
                     /**
                      * dummy `SignatureVisitor` to be abandoned
                      */
                     new org.objectweb.asm.signature.SignatureWriter()
                  }

               }
            }
            
            pscv visitXSig sig
            
            val pt0 = Esig(pscv.toString() )

            pt0

   }

   def getParameterTypes() = {
            
            val Esig(sig) = sig0

            var s : IndexedSeq[org.objectweb.asm.signature.SignatureWriter] = IndexedSeq()

            new org.objectweb.asm.signature.SignatureVisitor (ow.Opcodes.ASM9) {

               override 
               def visitParameterType() = {
                  val itemWr = org.objectweb.asm.signature.SignatureWriter()
                  s = s.appended(itemWr )
                  itemWr
               }
               
            }
            .visitXSig(sig)

            s
            .map(_.toString())
            .map(s => Esig(s) )

   }

   def getReturnType() = {
      
            val Esig(sig) = sig0

            /**
             * to be used to distill the return-type
             */
            val rscv = (
               new org.objectweb.asm.signature.SignatureWriter()
            )

            rscv.visitXSigReturnType(sig)

            val rt0 = Esig(rscv.toString() )

            rt0
            
   }

}

}

class esigAnalyseImpl(sig0 : Esig) {

            val Esig(sig) = sig0

            val pt0 = sig0.getParameterSig()
            
            val ptypes0 = sig0.getParameterTypes()

            val rt0 = sig0.getReturnType()
            
}








