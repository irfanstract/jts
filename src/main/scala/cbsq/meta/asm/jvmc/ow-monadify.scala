package cbsq.meta.asm.jvmc







// import language.experimental.pureFunctions








case
class computeMonadifiedSigImpl1(
   descriptor: String,
   givenSignature: String | Null ,
   xMonadifyImpl: Esig => Esig ,
)
{
               //
   
               extension (signature: Esig) {

                  protected
                  // transparent inline
                  def asXMonadified : Esig = {
                     xMonadifyImpl(signature)
                  }

               }
               
               val signature = ({
                  // Some(givenSignature).collect({ case e : String => e })
                  // .orElse[String](Some(descriptor) )
                  // .getOrElse(throw IllegalArgumentException("both null"))
                  ({
                     import language.unsafeNulls
                     Option(givenSignature)
                     .getOrElse(descriptor)
                  })
               })
               
               /**
                * 
                * *the descriptor* and *the signature* needs to be consistent ;
                * the *compute the signature* needs to happen first, and then
                * do *compute the erasure of the obtained signt*
                * 
                * here's *the signature*
                * 
                */
               val monadifiedSignature = ({
                  ({
                     Esig(signature)
                     .asXMonadified
                     .value
                  })
               })

               /**
                * 
                * here's *the type-erased signature*
                * 
                */
               val monadifiedNd = {
                  Esig(monadifiedSignature)
                  .asErased()
                  .value
               }
   
}



export amevMonadificativeImpl.asMakingAsyncifiedVariants
export amevMonadificativeImpl.wasGeneratedByAsyncify

export amevMonadificativeImpl.asMakingTupledVariants

export amevMonadificativeImpl.TsConfig

protected /* can't 'private' due to 'implicit' */
object amevMonadificativeImpl {
//

import org.objectweb.asm
import cbsq.meta.asm.jvm.Esig

def shallBeConsideredPrivate(access: Int): Boolean = {
   (
                        true
                        && (access.&(asm.Opcodes.ACC_SYNTHETIC) != 0 )
                        && (access.&(asm.Opcodes.ACC_PROTECTED | asm.Opcodes.ACC_PUBLIC) == 0 )
                        && (access.&(asm.Opcodes.ACC_BRIDGE) != 0 )
                     )
}

trait TsConfig {
   
   /**
    * 
    * whether the output would-be `.d.ts` file,
    * rather than regular `.ts` or `.js`
    * 
    */
   val isForTypeDeclarationFile : Boolean

}
object TsConfig {
   given TsConfig with {
      final val isForTypeDeclarationFile = true
   }
}

extension (dest: org.objectweb.asm.ClassVisitor) {

   /**
    * 
    * 
    * this `ClassVisitor`
    * adds, for each method, an "async" variant of it (eg returning `Future` or `CompletionStage`) .
    * important since
    * the W3C's threading-model *require explicit async-and-await syntax*
    * 
    * ```
    * async function main() {
    *   const localesScript = await fetch("./localedata.mjs") ;
    *   ...
    *   const localeBundle = await loadLocaleBundle(...) ;
    *   ...
    * }
    * ```
    *
    * not all methods deserve one
    * (eg *synthetic methods*, `&lt;clinit>`, *package-private* ones, `equals`, `hashCode`, `finalize`), and
    * different methods may need differentiated mangling-scheme
    * (eg `&lt;init>`, `of`, `from` )
    * 
    * ```
    * `&lt;clinit>`, `registerNatives`, *synthetic-methods*, `readObject`, `writeObject`, `finalize`
    * are not intended to be called externally
    * ```
    * 
    * 
    */
   def asMakingAsyncifiedVariants()(using tsConfig: TsConfig ) : org.objectweb.asm.ClassVisitor = {
      import tsConfig.isForTypeDeclarationFile
      val canShortenTheAsyncKeywForShortNames: Boolean = false
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

         /**
          * 
          * 
          * not all methods deserve one
          * (eg *synthetic methods*, `&lt;clinit>`, *package-private* ones, `equals`, `hashCode`, `finalize`), and
          * different methods may need differentiated mangling-scheme
          * (eg `&lt;init>`, `of`, `from` )
          * 
          * ```
          * `&lt;clinit>`, `registerNatives`, *synthetic-methods*, `readObject`, `writeObject`, `finalize`
          * are not intended to be called externally
          * ```
          * 
          * refusing to directly do the *asyncification* here ; instead
          * leave the responsibility,
          * of checking for presence of the `asynchronously` variant and accordingly draw-in the keyw `async`,
          * to the later pass
          * 
          * ```
          * # SOURCE - JVM
          * search(...args ) { ... } ;
          * searchAsynchronously(...args ) [native code] ; // will be specially-handled
          * 
          * # COMPILED - JS/ES
          * // search(...args ) ;
          * searchAsynchronously async (...args) { ... } ;
          * 
          * ```
          * 
          */
         override
         def visitMethod(access: Int, name: String, descriptor: String, givenSignature: String | Null, exceptions: Array[String | Null] | Null): asm.MethodVisitor | Null = {
            //
            val originalSigVisitor = ({
               cv.nn
               .visitMethod(access, name, {
                  descriptor
               }, {
                  givenSignature
               }, exceptions).nn
            })
            locally[asm.MethodVisitor] {
               val csg = (
                  computeMonadifiedSigImpl1(
                     
                     descriptor = descriptor.nn , 
                     givenSignature = givenSignature ,

                     xMonadifyImpl = (s: Esig) => {
                        s.asReturnValueFutureMonadified
                     } ,

                  )
               )
               import csg.signature
               import csg.monadifiedSignature
               import csg.monadifiedNd
               /**
                * 
                * as mentioned above
                * 
                * not all methods deserve one
                * (eg *synthetic methods*, `&lt;clinit>`, *package-private* ones, `equals`, `hashCode`, `finalize`), and
                * different methods may need differentiated mangling-scheme
                * 
                * refusing to directly do the *asyncification* here ;
                * leave the relevant responsibility(s) to the later pass
                * 
                */
               if ((
                  true

                  && ({
                     isForTypeDeclarationFile && (
                        shallBeConsideredPrivate(access = access )
                     )
                  } == false )

               )) {
                  /**
                   * 
                   */
                  name match {

                     case "<clinit>" =>

                     case "<init>" =>
                        val newMethodVisitor = {
                           cv.nn
                           .visitMethod(access | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_NATIVE, "$asyncNew", (
                              monadifiedNd
                           ), monadifiedSignature, Array.empty ).nn
                        }
                        newMethodVisitor.markAsGeneratedByAsyncify(originalName = name )
                        ({
                        newMethodVisitor.visitEnd()
                        })

                     case "finalize" if isForTypeDeclarationFile => 

                     case "close" | "dispose" => 
                        
                     case "clone" | "equals" | "toString" | "hashCode" =>

                     case _ =>
                        import language.unsafeNulls
                        val newMethodVisitor = {
                           cv.nn
                           .visitMethod(access | asm.Opcodes.ACC_NATIVE, name + {
                              name match
                                 case s if (s.matches("from") || (canShortenTheAsyncKeywForShortNames && !s.matches("do|update|[gs]et|compute|search|find|match|filter|drop") && (s.length() < 5 ) ) ) =>
                                    "Async"
                                 case _ =>
                                    "Asynchronously"
                              
                           }, (
                              monadifiedNd
                           ), monadifiedSignature, Array.empty ).nn
                        }
                        newMethodVisitor.markAsGeneratedByAsyncify(originalName = name )
                        ({
                        newMethodVisitor.visitEnd()
                        })
                              
                  }
               }
               originalSigVisitor
            }
         }
         
      }
   }

   // def visitMethodTupled(tsConfig: TsConfig, )(access: Int, name: String | Null, descriptor: String | Null, givenSignature: String | Null, exceptions: Array[String | Null] | Null) = {
   //    ???
   // }
   
}

val asyncifiedVariantMarkingAnnClass = ({
   import language.unsafeNulls
   import org.objectweb.asm
   asm.Type.getObjectType("jdk/internal/misc/OwMonadify$AsyncifiedVariant")
})

extension (newMethodVisitor: org.objectweb.asm.MethodVisitor) {

   def markAsGeneratedByAsyncify(originalName: String) : Unit = {
      // import language.unsafeNulls
      import org.objectweb.asm
      // TODO
      ({
         newMethodVisitor.visitAnnotation({
            asyncifiedVariantMarkingAnnClass
            .getInternalName().nn
         } , true )
      }) match {

         case a: (AnyRef & asm.AnnotationVisitor ) =>
            import language.unsafeNulls
            ({
               a.visit("originalName", originalName)
               a.visitEnd()
            })

         case _ =>
            
      }
   }
}

extension (dest: org.objectweb.asm.tree.MethodNode) {

   def wasGeneratedByAsyncify : Boolean = {
      import language.unsafeNulls
      import scala.jdk.CollectionConverters.*
      (dest.visibleAnnotations.asScala ++ dest.invisibleAnnotations.asScala )
      .map[String](a => a.desc )
      .contains[String](asyncifiedVariantMarkingAnnClass.getInternalName() )
   }

}

extension (dest: org.objectweb.asm.ClassVisitor) {
   
   def asMakingTupledVariants()(using tsConfig: TsConfig ) : org.objectweb.asm.ClassVisitor = {
      import tsConfig.isForTypeDeclarationFile
      new asm.ClassVisitor(asm.Opcodes.ASM9, dest) {

         override
         def visitMethod(access: Int, name: String | Null, descriptor: String | Null, givenSignature: String | Null, exceptions: Array[String | Null] | Null): asm.MethodVisitor | Null = {
            try {
               dest.visitMethod(access, name, descriptor, givenSignature, exceptions)
            } finally {
               val signature = {
                  import language.unsafeNulls
                  Option(givenSignature)
                  .getOrElse(descriptor)
               }
               val sps = {
                  Esig(signature)
                  .getParameterTypes()
               }
               val newSig = {
                  Esig.implementingGenericTypeSpc({ import language.unsafeNulls ; jPairsOwClassTags.ofTuples }, (
                     sps
                     .map(s => ('=', s) )
                  ))
                  .value
                  .++:("(").:++(")").:++(Esig(signature).getReturnType().value )
               }
               val exn = name.nn
               dest.visitMethod(access, name.nn ++ "$tupled", Esig(newSig).asErased().value, Esig(newSig).value, exceptions).nn
               .visitEnd()
            }
         }

      }
   }

}

//

}











