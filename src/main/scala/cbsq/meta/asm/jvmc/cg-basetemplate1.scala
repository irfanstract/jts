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

class wsnImplCtx1() {
   object eRpkImpl extends
   AnyRef
   with ERpk
   {
      
      extension (name0: ow.Type) {
         
         /**
          * 
          * the simple-name
          * 
          */
         def getSimpleName(): String = {
                  ;
                  import language.unsafeNulls
                  ;
                  // TODO
                  val name = (
                     /* avoids the "L" and ";" */
                     name0.getInternalName()
                  )
                  name
                  .split("\\/")
                  .last
         }

         /**
          * 
          * for `a/bpkg/c/d/EBar$FG`,
          * return as notation `a.bpkg.c.d.EBar$FG`
          *
          */
         def getCanonicalName(): String = {
            import language.unsafeNulls   /* a lot of vanilla Java stuffs */
            name0.getInternalName()
            .replace("/", ".")
         }

      }
      
      /**
       * 
       * for `a/bpkg/c/d/E`,
       * return as notation `&lt;rootPkgObjRef>.a.bpkg.c.d.E`
       *
       */
      def rpkName(superName: ow.Type): String = {
         superName
         .getCanonicalName()
         .prependedAll("rootPkg.")
      }

      val canOmitPrivateMethods: Boolean = {
         true
      }

      def getBaseTemplate() = {
         import language.unsafeNulls   /* a lot of vanilla Java stuffs */
         import cbsq.meta.util.PwEmitter
         val baseTemplate = (
         PwEmitter.through((o) => {
            val essentialLogicalPreamble = (
               Seq()
               .:+((
                  Seq()
                  .:+("/*                           ")
                  .:+(" *                           ")
                  .:+(" * auto-generated --        ") 
                  .:+(" * ALL CHANGES will GO AWAY ")
                  .:+(" *                           ")
                  .:+(" */ ")
                  .mkString("\r\n")
                  .stripTrailing()
               ))
               .:+("\"use strict\" ; ")
               .:+("declare const rootPkg : typeof global ; ")
               .:+("     ")
               .mkString("\r\n")
               .stripTrailing()
            )
            o println essentialLogicalPreamble
            o.println()
            o.println()
            o.println()
            o.println()
            o.println()
            o.println()
            o.println()
         })
         )
         baseTemplate
      }
      
      extension (instr: org.objectweb.asm.tree.AbstractInsnNode) {

         def toJsBlockLevelStmt()(using InOpdCtx, Sdc): String = {
                  import scala.language.unsafeNulls
                  import scala.jdk.CollectionConverters.*
                  import org.objectweb.asm
                  import cbsq.meta.asm.jvm.opcodeNameTable
                  val instrS = {
                     val opcodeName = (
                        opcodeNameTable.apply(instr.getOpcode())
                     )
                     extension (f: String) {
                        def prependedWithDef() = (
                           f
                           .prependedAll("const " + (summon[InOpdCtx].formatStackReturnRelative( ) ) + " = " )
                        )
                     }
                     instr match {

                        case c: asm.tree.InsnNode =>
                           val ReturnStmtOpName = "(\\w+)RETURN".r
                           val VConstYOpName = "(\\w)CONST_(.+)".r
                           val opcodeName = (
                              opcodeNameTable.apply(c.getOpcode())
                           )
                           opcodeName match

                              case ReturnStmtOpName(what) =>
                                 val dataTypeSimpleName = {
                                    import cbsq.meta.asm.jvm.getOpcodeDataTypeCanonicalName
                                    getOpcodeDataTypeCanonicalName(what)
                                 }
                                 s"return /* ${dataTypeSimpleName } */ (some value) "
                                 
                              case VConstYOpName(typ @ ("I" | "L" | "F" | "D"), cvString) =>
                                 // TODO
                                 (summon[InOpdCtx].formatStackOperandRelative( ) )
                                 .replaceFirst("[\\S\\s]*", "[VALUE, $0]".replace("VALUE", cvString ) )
                                 .prependedWithDef()
                                 .prependedAll(s"/* ${typ}CONST_${cvString } */" + "\n")

                              case "ACONST_NULL" =>
                                 // TODO
                                 (summon[InOpdCtx].formatStackOperandRelative( ) )
                                 .replaceFirst("[\\S\\s]*", "[VALUE, $0]".replace("VALUE", "null" ) )
                                 .prependedWithDef()

                              case "NOP" | "NOOP" =>
                                 (summon[InOpdCtx].formatStackOperandRelative( ) )
                                 .appendedAll(" /* NOOP; no change in opd-stack */")
                                 .prependedWithDef()

                              case _ =>
                                 s"${opcodeName }"
                                 .prependedWithDef()
                           
                        case c: asm.tree.MethodInsnNode =>
                           val cAsJvmDecom = s"${opcodeName } ${c.name }${c.desc } "
                           c.toXJsString(documentOriginalSrc = false )
                           .replaceFirst("[\\S\\s]*", "($0)")
                           .prependedWithDef()
                           .prependedAll(s"/* $cAsJvmDecom */" + "\n")
                           
                        case c: asm.tree.LdcInsnNode =>
                           import c.cst
                           s"ldc ${cst.getClass().getSimpleName() }(${cst })"
                           .prependedWithDef()

                        case c: asm.tree.LabelNode =>
                           s"/* label: ${c.getLabel() } ; */"
                           .appendedAll(" ")
                           .appendedAll(summon[InOpdCtx].formatStackOperandRelative( ) )
                        case c: asm.tree.FrameNode =>
                           opcodeName match {
                              case "F_NEW" | "F_FULL" =>
                                 Seq(
                                    s"/* new frame: L ${c.local } */ " ,
                                    s"/*            S ${c.stack } */ " ,
                                 ).mkString("\n")
                              case _ =>
                                 s"/* frame chg ${opcodeName }(......) */ "
                           }
                        case c: asm.tree.LineNumberNode =>
                           val lineNumber = c.line
                           val srcFileName = summon[Sdc].srcFileName
                           val ls = {
                              "" + srcFileName + ":" + lineNumber
                           }
                           s"/* line ${ls } */"

                        case c =>
                           s"(opcode ${opcodeName })(.........)"
                           .prependedWithDef()

                     }
                  }
                  instrS + " ;"
         }

      }

      extension (c: org.objectweb.asm.tree.MethodInsnNode) {

         // TODO
         def toXJsString(
            documentOriginalSrc: Boolean = true ,
            useIife: Boolean = false ,
            async: Boolean = true ,

         )(using InOpdCtx) : String = {
            // s"${opcodeName } ${c.name }${c.desc } "
            ({
               // import scala.language.unsafeNulls
               import scala.jdk.CollectionConverters.*
               import org.objectweb.asm
               import cbsq.meta.asm.jvm.opcodeNameTable
               // TODO
               c.getOpcode() match {

                  case opc @ (asm.Opcodes.INVOKEVIRTUAL | asm.Opcodes.INVOKEINTERFACE | asm.Opcodes.INVOKESPECIAL | asm.Opcodes.INVOKESTATIC) =>
                     val receiverCount = (
                        opc match
                           case asm.Opcodes.INVOKEVIRTUAL | asm.Opcodes.INVOKEINTERFACE =>
                              1
                           case asm.Opcodes.INVOKESPECIAL =>
                              1
                           case asm.Opcodes.INVOKESTATIC | asm.Opcodes.INVOKEDYNAMIC => 
                              0
                        
                     ) : Int
                     val odst = (
                     NativeSigImpl(access = 0x0, name = c.name.nn, descriptor0 = {
                        NativeSigImpl.Bds.apply(descriptor = c.desc.nn, signature0 = null )
                     })
                     )
                     val argdsc = (
                        asm.Type.getType(odst.descriptor).nn
                     )
                     val receiverAlias = (
                        "ths1"
                     )
                     val nonReceiverArgsVarNames = (
                        argdsc
                        .getArgumentTypes().nn.toIndexedSeq
                        .indices
                        .map(i => s"arg$i")
                     )
                     val nonReceiverArgsSpread = (
                        nonReceiverArgsVarNames
                        .map(_ + ",").mkString
                     )
                     val nonReceiverArgsSpreadReversed = (
                        nonReceiverArgsVarNames
                        .reverse
                        .map(_ + ",").mkString
                     )
                     (
                        odst
                        .toJsMethodName()
                        .++(/* async */ (
                           // TODO
                           if async then "Asynchronously"
                           else ""
                        )).nn
                        .++(s"($nonReceiverArgsSpread )")
                        .prependedAll(".")
                        .prependedAll(receiverAlias )
                        .++((
                           if documentOriginalSrc then s" /* $argdsc */"
                           else ""
                        ))
                        .replaceFirst("[\\S\\s]*", /* async */ (
                           if async then "await $0"
                           else "$0"
                        )).nn
                        .replaceFirst("[\\S\\s]*", (
                           "{ const returnVal = ($0) ; return [returnVal, ...unusedOpdStackValues] /* as const */ ; }"
                        )).nn
                     ) match
                     case s =>
                        import util.matching.Regex.{quote, quoteReplacement}
                        val asw = (
                           if async then "async"
                           else ""
                        )
                        val awt = (
                           if async then "await"
                           else ""
                        )
                        if useIife then 
                           s
                           .replaceFirst("[\\S\\s]*", (
                              receiverCount match {
                                 case 0 => s"$asw ([${nonReceiverArgsSpreadReversed                } ...unusedOpdStackValues]) => " + "$0"
                                 case 1 => s"$asw ([$nonReceiverArgsSpreadReversed $receiverAlias, ...unusedOpdStackValues]) => " + "$0"
                              }
                           )).nn
                           .replaceFirst("[\\S\\s]*", (
                              "($0 )" + quoteReplacement(s"(${summon[InOpdCtx ].formatStackOperandRelative( ) })")
                           )).nn
                           .replaceFirst("[\\S\\s]*", (
                              s"$awt $$0"
                           )).nn
                        else {
                           val stackRef = (
                              summon[InOpdCtx ].formatStackOperandRelative( )
                           )
                           s
                           .replaceFirst("[\\S\\s]*", ({
                              receiverCount match {
                                 case 0 => s"$awt ($asw () => { const [${nonReceiverArgsSpreadReversed                      } ...unusedOpdStackValues, ] = " + quoteReplacement(stackRef ) + " ; $0 } )()"
                                 case 1 => s"$awt ($asw () => { const [${nonReceiverArgsSpreadReversed } ${receiverAlias }, ...unusedOpdStackValues, ] = " + quoteReplacement(stackRef ) + " ; $0 } )()"
                              }
                           })).nn
                        }
                     

                  case _ =>
                     import scala.language.unsafeNulls
                     val opcodeName = opcodeNameTable(c.getOpcode() )
                     s"${opcodeName } ${c.name }${c.desc } "

               }
            })
         }

      }

   }
   /* avoid using wildcard imports as there are other stuffs */ 
   export eRpkImpl.getBaseTemplate
   export eRpkImpl.canOmitPrivateMethods
   export eRpkImpl.{getCanonicalName, getSimpleName}
   export eRpkImpl.{compileInlineLevelRef}
   export eRpkImpl.toJsBlockLevelStmt

   export cbsq.meta.asm.jvm.{MethodDescriptorImpl1 as NativeSigImpl }
   export cbsq.meta.asm.jvmc.toJsMethodDeclString
   export cbsq.meta.asm.jvmc.{isSynthetic, isEffectivelyPrivate}
   // type NativeSigImpl
   //    >: (Int, String)
   //    <: (Int, String)

}





extension (o: java.io.PrintWriter) {

   def printXHiddenMethodsCommentSection(
      hideableMethodsEffectively: collection.Iterable[cbsq.meta.asm.jvm.MethodDescriptorImpl1]
   ): Unit = {
            ;
            // import items as hideableMethodsEffectively
            ;
            o.println(s"  /* ")
            o.println(s"   * ")
            o.println(s"   * hidden methods: ")
            ({
               val maxNameChars = (
                  hideableMethodsEffectively
                  .map(e => e.name)
                  .map(_.length())
                  .max
               )
               for (m <- hideableMethodsEffectively) {
                  val mFullSigExcludingName = (
                     m.toString()
                     .replaceFirst("\\A[\\s\\w\\-]*", "")
                  )
                  o.println(s"   * - ${m.name.padTo(maxNameChars + 1, ' ') }$mFullSigExcludingName ")
               }
            })
            o.println(s"   * ")
            o.println(s"   */")
      //
   }

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

































