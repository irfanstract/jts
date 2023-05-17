package cbsq.meta.asm.jvm

















/**
 * 
 * alternative to `org.objectweb.asm.Type`
 * generalised to all *signature*s
 * 
 */
sealed
case class Esig(val value: String) {

   value.nn
   require(0 < value.length(), "[Esig()] unsupported empty string")

   override
   def toString(): String = {
      val METHOD = "\\A\\(".r.unanchored
      value match
         case METHOD(_) =>
            s"l $value"
         case _ =>
            s"type $value"
      
   }

}

object Esig
{

extension (sig0 : Esig) {

   /**
    * 
    * after *type-erasure*
    * 
    */
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

   /**
    * 
    * infer as-much info possible
    * 
    */
   def analyse() = {
      esigAnalyseImpl(sig0)
   }

   /**
    * 
    * equivalent-instance with the return-type (re)set to `void`
    * 
    * see `getParameterTypes`
    * 
    */
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
                        import language.unsafeNulls
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

/**
 * 
 * eg `Seq<Bar1>`, `Tuple<Bar1, Bar2, Bar3, ...>`, `Values.Iterator<Bar1>`
 * 
 * 
 * @param c the base-type (necessarily raw-type)
 * @param p the type-argument(s)
 * 
 * @return `Esig`
 * 
 */
def implementingGenericTypeSpc(
   c: org.objectweb.asm.Type,
   p: IndexedSeq[(VarianceChar, Esig)] ,
   
): Esig = {
   import org.objectweb.asm
   val s = {
      import language.unsafeNulls
      val cv = (
         new asm.signature.SignatureWriter()
      )
      cv visitClassType(c.getInternalName() )
      for ((variance2, type2) <- p) {
         val subVisitor = (
            (cv : asm.signature.SignatureVisitor) visitTypeArgument(variance2 )
         )
         subVisitor visitXSig type2.value
         // subVisitor.visitEnd()
      }
      cv.visitEnd()
      cv.toString()
   }
   Esig(s )
}

}

class esigAnalyseImpl(sig0 : Esig) {

            val Esig(sig) = sig0

            val pt0 = sig0.getParameterSig()
            
            val ptypes0 = sig0.getParameterTypes()

            val rt0 = sig0.getReturnType()
            
}


























































