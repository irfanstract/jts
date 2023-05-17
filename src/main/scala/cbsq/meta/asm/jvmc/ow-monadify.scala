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



export amevMonadificativeImpl.asMakingAsyncMonadifiedVariants
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
    * this `ClassVisitor`
    * adds, for each method, an "async" variant of it (eg returning `Future` or `CompletionStage`)
    * 
    * important since
    * the W3C's threading-model *require explicit async-and-await syntax*
    *
    */
   def asMakingAsyncMonadifiedVariants()(using tsConfig0: TsConfig ) : org.objectweb.asm.ClassVisitor = {
      val c1 = getAmdtConfigImpl(tsConfig0)
      // import c1.*
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
               dest.nn
               .visitMethodAsyncMonadified(using c1)(access = access , name = name , descriptor = descriptor , givenSignature = givenSignature , exceptions = exceptions )
            }
         }
         
      }
   }

   def visitMethodAsyncMonadified(using  c1: getAmdtConfigImpl )(access: Int, name: String | Null, descriptor: String | Null, givenSignature: String | Null, exceptions: Array[String | Null] | Null) = {
      val cv = dest
      import c1.*
      ({
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
                * not all methods deserve one
                * (eg *synthetic methods*, `&lt;clinit>`, *package-private* ones, `equals`, `hashCode`, `finalize`), and
                * different methods may need differentiated mangling-scheme
                * (eg `&lt;init>`, `of`, `from` )
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
               name match {

                  case "<clinit>" =>

                  case "<init>" =>
                     cv.nn
                     .visitMethod(access | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_NATIVE, "$asyncNew", (
                        monadifiedNd
                     ), monadifiedSignature, Array.empty ).nn
                     .visitEnd()

                  case "finalize" if isForTypeDeclarationFile =>

                  case "close" | "dispose" =>
                     
                  case "clone" | "equals" | "toString" | "hashCode" =>

                  case _ =>
                     import language.unsafeNulls
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
                     .visitEnd()
                           
               }
               }
               ()
      })
   }

}

class getAmdtConfigImpl(val tsConfig: TsConfig) {
   export tsConfig.isForTypeDeclarationFile
   val canShortenTheAsyncKeywForShortNames: Boolean = false
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
               dest.visitMethodTupled(access = access , name = name , descriptor = descriptor , givenSignature = givenSignature , exceptions = exceptions )
            }
         }

      }
   }

   // transparent 
   // inline
   def visitMethodTupled(using tsConfig: TsConfig )(access: Int, name: String | Null, descriptor: String | Null, givenSignature: String | Null, exceptions: Array[String | Null] | Null) = {
      import tsConfig.isForTypeDeclarationFile
      ({
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
                  Esig.implementingGenericTypeSpc({ import language.unsafeNulls ; asm.Type.getObjectType("java/lang/Tuple") }, (
                     sps
                     .map(s => ('=', s) )
                  ))
                  .value
                  .++:("(").:++(")").:++(Esig(signature).getReturnType().value )
               }
               val exn = name.nn
               dest.visitMethod(access, name.nn ++ "$tupled", Esig(newSig).asErased().value, Esig(newSig).value, exceptions).nn
               .visitEnd()
      })
   }

}

//

}











