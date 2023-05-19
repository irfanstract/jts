package cbsq.meta.asm.jvmc
















trait ERpkImplicits extends
   AnyRef
   with cbsq.meta.asm.jvm.FullyQualifiedClassNameAnalyser
   with cbsq.meta.asm.jvmc.ClassNamesCompiler
{ eRpkImpl =>

   // val boxingImplicits : (
   //    AnyRef
   //    with cbsq.meta.asm.jvm.FullyQualifiedClassNameAnalyser
   //    with cbsq.meta.asm.jvmc.ClassNamesCompiler
   // ) = eRpkImpl

}

trait ERpk extends
   AnyRef
   with cbsq.meta.asm.jvm.FullyQualifiedClassNameAnalyser
   with cbsq.meta.asm.jvmc.ClassNamesCompiler
   with ERpkImplicits
{

   import org.objectweb.asm

   extension (name0: asm.Type) {

      /**
       * 
       * compile a/the function-body-level references to the `namespace`
       * 
       * ```
       * // JS
       * return (
       *   java.lang.System.getProperty("os.arch")
       *   || ARCH_NOT_KNOWN
       * ) ;
       * 
       * // Scala
       * (
       *   java.lang.System.getProperty("os.arch")
       *   ?? archs.archUnknown
       * )
       * 
       * // Python
       * return (
       *    java.lang.System.get_property("os.arch")
       *    or ARCH_NOT_KNOWN
       * )
       * 
       * // Bash
       * return (
       *    (java_lang_System_getProperty "os.arch" )
       *    or (ARCH_NOT_KNOWN )
       * )
       * 
       * ```
       * 
       */
      inline def compileInlineLevelRef(): String = {
         rpkName(name0)
      }

   }
   
   /**
    * 
    * for `a/bpkg/c/d/E`,
    * return code which reference the `namespace` obj
    *
    */
   def rpkName(superName: asm.Type): String

   val canOmitPrivateMethods: Boolean
   
   /**
    * 
    * TODO .
    * an ES/JS file containing only any of `"use strict"` or `import ... from ...` or `// YOUR CODE GOES HERE`
    *
    */
   @deprecated("TODO")
   def getBaseTemplate() : WsnPwEmitter

   /* can't put this method here; this method makes assumption of the target platform being JS/ES/TS */
   // extension (instr: asm.tree.AbstractInsnNode) {
   // 
   //    def toJsBlockLevelStmt()(using InOpdCtx, Sdc): String
   // 
   // }

}





/**
 * 
 * source-file info
 * 
 */
trait Sdc {
   val srcFileName: String
}

/**
 * 
 * current opcode info locals
 * 
 */
trait InOpdCtx {
   val operandStackPrefix: String
   val returnValueStackPrefix: String
}

extension (this1: InOpdCtx) {

   def formatStackOperandRelative() = {
      this1.operandStackPrefix + "$" + "stack"
   }
   def formatStackReturnRelative() = {
      this1.returnValueStackPrefix + "$" + "stack"
   }
   
}

export cbsq.meta.asm.jvm.FqnStronumericPair

export cbsq.meta.asm.jvm.JbltOpdStackState

/**
 * 
 * the aspect of executing a single opcode
 * 
 */
trait Jblt {

   val transliteratedForm: String

   val resultingOpdState: Jblt.OpdState[Any]
   
}
object Jblt {

   sealed
   case class OpdState[+E](opdStack: JbltOpdStackState[E], storage: IndexedSeq[E])

   object OpdState
   {

      extension [OpdStackItem <: FqnStronumericPair[?]](e: OpdState[OpdStackItem ]) {

         def afterLdcOpaque = {
            val lastName = (
               e.opdStack
               .fromLeftRightwards
               .map(<:<.refl[FqnStronumericPair[?]] )
               .sortBy({
                  case e : NonEmptyTuple =>
                     e.last : Int
               })
               .lastOption
            )
            val newName = (
               lastName match {
                  case Some(value) => 
                     val (prefix, i) = value
                     (prefix, i + 1)
                  case None => 
                     ("lclv", 0 + 1)
               }
            )
            e.copy(opdStack = e.opdStack match { case s => s pushed newName } )
         }

         def afterPopoff = {
            e.copy(opdStack = e.opdStack match { case s => s.poppedOne()._1 } )
         }

         def afterPopoffN(n: Int) = {
            Range(0, n)
            .foldLeft[OpdState[OpdStackItem] ](e)((e, _) => e.afterPopoff )
         }

      }

   }

   type OfStorageType[+E] = Jblt {
      
      val resultingOpdState: Jblt.OpdState[E]
      
   }

}





private
val checkIsMethodInvocOpcode = {
   // import scala.language.unsafeNulls
   import scala.jdk.CollectionConverters.*
   import org.objectweb.asm
   import cbsq.meta.asm.jvm.opcodeNameTable

   ({
      case opc @ (asm.Opcodes.INVOKEVIRTUAL | asm.Opcodes.INVOKEINTERFACE | asm.Opcodes.INVOKESPECIAL | asm.Opcodes.INVOKESTATIC) =>
   } : PartialFunction[Int, Unit] )
}

opaque type MethodInvocOpcode
   <: Int
   = Int

object MethodInvocOpcode {

   def unapply(v: Int) = checkIsMethodInvocOpcode isDefinedAt(v)
   
   def check(v: Int): MethodInvocOpcode = {
      checkIsMethodInvocOpcode(v)
      v
   }

}

class analyseMethodInvocOpcImpl(opc: MethodInvocOpcode, odst: MethodDescriptorImpl1) {

         // import scala.language.unsafeNulls
         import scala.jdk.CollectionConverters.*
         import org.objectweb.asm
         import cbsq.meta.asm.jvm.opcodeNameTable

         val receiverCount = (
            opc match
               case asm.Opcodes.INVOKEVIRTUAL | asm.Opcodes.INVOKEINTERFACE =>
                  1
               case asm.Opcodes.INVOKESPECIAL =>
                  1
               case asm.Opcodes.INVOKESTATIC | asm.Opcodes.INVOKEDYNAMIC => 
                  0
            
         ) : Int

         val argdsc = (
            asm.Type.getType(odst.descriptor).nn
         )

         val nonReceiverArgsVarNames0 = (
            argdsc
            .getArgumentTypes().nn.toIndexedSeq
         )
         
         val nonReceiverArity = (
            nonReceiverArgsVarNames0
            .length
         )

}

class analyseMethodInvocOpcTranslitImpl(
   opc: MethodInvocOpcode,
   odst: MethodDescriptorImpl1,
   opdState0: Jblt.OpdState[FqnStronumericPair[?] ],
) {

         // import scala.language.unsafeNulls
         import scala.jdk.CollectionConverters.*
         import org.objectweb.asm
         import cbsq.meta.asm.jvm.opcodeNameTable

         extension (v: NonEmptyTuple ) {

            def toSingleWordNameString(): String = {
               v
               .toList.mkString("$")
            }
            
         }

         val opcodeAnalysis = (
            analyseMethodInvocOpcImpl(MethodInvocOpcode.check(opc), odst)
         )

         export opcodeAnalysis.receiverCount
         export opcodeAnalysis.argdsc
         export opcodeAnalysis.nonReceiverArgsVarNames0
         export opcodeAnalysis.nonReceiverArity

         val opdStackState0 = (
               opdState0.opdStack
         )

         /**
          * 
          * after popping-off
          * the topmost *n* items
          * (the args (conditionally including the receiver) to the invoc )
          * 
          */
         val (opdStackState1, vrs0) = {
            /**
             * 
             * after popping-off
             * the topmost *n* items
             * (the args (conditionally including the receiver) to the invoc )
             * 
             */
            opdStackState0
            .poppedN(receiverCount + nonReceiverArity )
         }

         val vrs = (
            vrs0
            .map(_.toSingleWordNameString() )
         )

         val (receiverAlias +: nonReceiverArgsVarNames) = {
            (vrs.take(receiverCount ).padTo(1, "undefined") ++ (
               vrs.drop(receiverCount )
            ) )
         }

         val nonReceiverArgsSpread = (
            nonReceiverArgsVarNames
            .map(_ + ",").mkString
         )
         val nonReceiverArgsSpreadReversed = (
            nonReceiverArgsVarNames
            .reverse
            .map(_ + ",").mkString
         )

         /**
          * 
          * after adding conditional (ie for non-void returns) return-value
          * 
          */
         val opdStackState2 = (
            opdStackState1
            .pushedAll({
               ({
                  asm.Type.getType(odst.descriptor ).nn
                  .getReturnType().nn
               }) match {
                  case returnType =>
                     if returnType == asm.Type.VOID_TYPE  then
                        Seq()
                     else {
                        opdState0.afterLdcOpaque
                        .opdStack
                        .poppedN(1)._2
                     }
               }
            } )
         )

         val opdStackStateFinal = (
            opdStackState2
         )
         
         
   
}

































