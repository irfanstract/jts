package cbsq.meta.asm.jvmc


















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

      def tbmt_??? =
         throw NotImplementedError()
      
      extension (instr: org.objectweb.asm.tree.AbstractInsnNode) {

         def toJsBlockLevelStmt(
            opdState0: Jblt.OpdState[FqnStronumericPair[?] ] ,

         )(using instropc : InOpdCtx)(using Sdc): Jblt.OfStorageType[FqnStronumericPair[?] ] = {
                  import scala.language.unsafeNulls
                  import scala.jdk.CollectionConverters.*
                  import org.objectweb.asm
                  extension (st: Jblt.OpdState[FqnStronumericPair[?] ] ) {

                     def newlyOpdOnstackPushedVarName: String = {
                        st.opdStack
                              .fromLeftRightwards.last
                              .toSingleWordNameString()
                     }

                  }
                  import cbsq.meta.asm.jvm.opcodeNameTable
                  val InvokeYyy  = "INVOKE(\\w+)".r
                  val YyConstYyy = "(\\w)CONST_(\\w+)".r
                  val YyLoadOrStore = "(\\w)(LOAD|STORE)".r
                  val YyArrayLoadOrStore = "(\\w)A(LOAD|STORE)".r
                  extension (opcodeName: String) {

                     def ldcTConstOpcodeNamePrependedWithDef(): Jblt.OfStorageType[FqnStronumericPair[?] ] = {
                     ;
                     //
                     val YyConstYyy(typeWord1, valStr) = opcodeName : @unchecked
                     new Jblt {

                        override
                        val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                           opdState0
                           .afterLdcOpaque
                        }
   
                        override
                        val transliteratedForm = {
                           val varName = (
                              resultingOpdState
                              .newlyOpdOnstackPushedVarName
                           )
                           s"const $varName = $valStr /* $opcodeName */ ; "
                        }
                        
                     } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                     }
                     
                  }
                  locally {
                     import instropc.toSingleWordNameString
                     val opcodeName = (
                        opcodeNameTable.apply(instr.getOpcode())
                     )
                     val unsupportedOpcodeFallbackCompiledCode = ({
                        opcodeName match {
                           
                           case InvokeYyy(what1) if (what1 != "DYNAMIC") =>
                              val c1 = instr.asInstanceOf[asm.tree.MethodInsnNode]
                              import c1.{desc, name, owner}
                              s"/* unsupported */ INVOKE_$what1(${owner}.${name}${desc}) ;"

                           case YyConstYyy(typeWord1, valStr) =>
                              s"/* unsupported */ CONST<${typeWord1 }> $valStr ;"

                           case "INVOKEDYNAMIC" =>
                              s"/* unsupported INVOKEDYNAMIC instr */ (.........) ;"

                           case _ =>
                              s"/* unsupported */ ${opcodeName }(.........) ;"

                        }
                     })
                     extension (f: String) { /* pop-off */

                        def popoffPrependedWithDef(): Jblt.OfStorageType[FqnStronumericPair[?] ] = ({
                        new Jblt {

                           override
                           val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                              opdState0
                              .afterPopoff
                           }
      
                           override
                           val transliteratedForm = {
                              // s"${opcodeName } ${c.name }${c.desc } "
                              s"/* discarded */ $f ; "
                           }
                           
                        } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                        })
                        
                     }
                     def yStorePrependedWithDef(locI: Int): Jblt.OfStorageType[FqnStronumericPair[?] ] = {
                     new Jblt {

                        val lv = {
                           opdState0
                           // .afterLdcOpaque
                        }

                        override
                        val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                           lv
                           .afterYStoreOpc(destStorageIndex = locI )
                        }
   
                        override
                        val transliteratedForm = {
                           s"/* (?)STORE */ ;"
                        }
                        
                     } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                     }
                     extension (f: String) {

                        def ldcPrependedWithDef(): Jblt.OfStorageType[FqnStronumericPair[?] ] = {
                        new Jblt {

                           override
                           val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                              opdState0
                              .afterLdcOpaque
                           }
      
                           override
                           val transliteratedForm = {
                              val c = summon(using instr)
                              val varName = (
                                 resultingOpdState
                                 .newlyOpdOnstackPushedVarName
                              )
                              // s"${opcodeName } ${c.name }${c.desc } "
                              s"const $varName = ($f ) ;"
                           }
                           
                        } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                        }
                        
                     }
                     extension (f: String) {

                        def unaryOpPrependedWithDef(): Jblt.OfStorageType[FqnStronumericPair[?] ] = ({
                        new Jblt {

                           override
                           val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                              opdState0
                              .afterPopoff
                              .afterLdcOpaque
                           }
      
                           override
                           val transliteratedForm = {
                              val c = summon(using instr)
                              val varName = (
                                 resultingOpdState
                                 .newlyOpdOnstackPushedVarName
                              )
                              // s"${opcodeName } ${c.name }${c.desc } "
                              // unsupportedOpcodeFallbackCompiledCode
                              f.ldcPrependedWithDef()
                              .transliteratedForm
                           }
                           
                        } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                        })
                        
                     }
                     extension (f: String) {

                        def fixedOpdStackOp(): Jblt.OfStorageType[FqnStronumericPair[?] ] = ({
                        new Jblt {

                           override
                           val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                              opdState0
                           }
      
                           override
                           val transliteratedForm = {
                              // s"${opcodeName } ${c.name }${c.desc } "
                              // unsupportedOpcodeFallbackCompiledCode
                              s"$f ; "
                           }
                           
                        } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                        })
                        
                     }
                     extension (f: String) {

                        def gotoPrependedWithDef(): Jblt.OfStorageType[FqnStronumericPair[?] ] = (
                           // tbmt_???
                           new Jblt {

                              override
                              val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                                 opdState0
                                 .copy(opdStack = JbltOpdStackState.empty )
                              }
         
                              override
                              val transliteratedForm = {
                                 // TODO
                                 s"goto /* what??? */ ;"
                              }
                              
                           } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                        )
                        
                     }
                     def opdStackTopmostItem(i: Int) = {
                        //
                        opdState0
                        .opdStack.
                        fromRightLeftwards(i)
                        .toSingleWordNameString()
                     }
                     instr match {

                        case c: asm.tree.InsnNode =>
                           val ReturnStmtOpName = "(\\w+)RETURN".r
                           val VConstYOpName = "(\\w)CONST_(.+)".r
                           val opcodeName = (
                              opcodeNameTable.apply(c.getOpcode())
                           )
                           opcodeName match

                              // case ReturnStmtOpName(what) =>
                              //    val dataTypeSimpleName = {
                              //       import cbsq.meta.asm.jvm.getOpcodeDataTypeCanonicalName
                              //       getOpcodeDataTypeCanonicalName(what)
                              //    }
                              //    s"return /* ${dataTypeSimpleName } */ (some value) "
                                 
                              case YyArrayLoadOrStore(typeStr, "STORE") =>
                                 val assigneeRef = opdStackTopmostItem(i = 2)
                                 val iRef        = opdStackTopmostItem(i = 1)
                                 val assigendRef = opdStackTopmostItem(i = 0)
                                 (s"$assigneeRef[$iRef] = $assigendRef " )
                                 .popoffPrependedWithDef()

                              case "DUP" =>
                                 opdStackTopmostItem(i = 0)
                                 .ldcPrependedWithDef()

                              case VConstYOpName(typ @ ("I" | "L" | "F" | "D"), cvString) =>
                                 // // TODO
                                 // (summon[InOpdCtx].formatStackOperandRelative( ) )
                                 // .replaceFirst("[\\S\\s]*", "[VALUE, $0]".replace("VALUE", cvString ) )
                                 // .ldcPrependedWithDef()
                                 opcodeName.ldcTConstOpcodeNamePrependedWithDef()

                              case "ACONST_NULL" =>
                                 // // TODO
                                 // (summon[InOpdCtx].formatStackOperandRelative( ) )
                                 // .replaceFirst("[\\S\\s]*", "[VALUE, $0]".replace("VALUE", "null" ) )
                                 // .ldcPrependedWithDef()
                                 opcodeName.ldcTConstOpcodeNamePrependedWithDef()

                              // case "NOP" | "NOOP" =>
                              //    (summon[InOpdCtx].formatStackOperandRelative( ) )
                              //    .appendedAll(" /* NOOP; no change in opd-stack */")
                              //    .ldcPrependedWithDef()

                              case _ =>
                                 s"${opcodeName }"
                                 // .gotoPrependedWithDef()
                                 .ldcPrependedWithDef()
                           
                        case c: asm.tree.MethodInsnNode =>
                           val cAsJvmDecom = s"${opcodeName } ${c.name }${c.desc } "
                           c.toXJsString(
                              opdState0 = opdState0,
                              documentOriginalSrc = false ,
                           )
                           
                        case c: asm.tree.TypeInsnNode =>
                           import eRpkImpl.{compileInlineLevelRef as compileInlineLevelRef1}
                           opcodeName match {

                              case "CHECKCAST" =>
                                 // TODO
                                 // s"/* `$opcodeName ${c.desc }` */ ${opdStackTopmostItem(i = 0) } "
                                 s"(${opdStackTopmostItem(i = 0) } ) instanceof ${(asm.Type.getObjectType(c.desc ) ).compileInlineLevelRef1() } "
                                 .unaryOpPrependedWithDef()
                                 
                              case "INSTANCEOF" =>
                                 // TODO
                                 s"/* `$opcodeName ${c.desc }` */ true "
                                 .unaryOpPrependedWithDef()
                                 
                              case "NEW" =>
                                 // TODO
                                 s"/* `new ${c.desc }` */ ??? "
                                 .ldcPrependedWithDef()
                                 
                              case _ =>
                                 s"${opcodeName }"
                                 // .gotoPrependedWithDef()
                                 .ldcPrependedWithDef()
                           
                           }
                           
                        case c: asm.tree.FieldInsnNode =>
                           import eRpkImpl.{compileInlineLevelRef as compileInlineLevelRef1}
                           opcodeName match {

                              case opc @ ("GETFIELD" | "PUTFIELD") =>
                                 // TODO
                                 val iRef        = c.name
                                 opc match {

                                    case "PUTFIELD" =>
                                       val assigneeRef = opdStackTopmostItem(i = 1)
                                       val assigendRef = opdStackTopmostItem(i = 0)
                                       s"$assigneeRef.${iRef } = ${assigendRef } "
                                       .popoffPrependedWithDef()
                                       
                                    case "GETFIELD" =>
                                       val assigneeRef = opdStackTopmostItem(i = 0)
                                       s"$assigneeRef.${iRef } "
                                       .ldcPrependedWithDef()

                                 }
                                 
                              case opc @ ("PUTSTATIC" | "GETSTATIC") =>
                                 // TODO
                                 val assigneeRef = (
                                    asm.Type.getObjectType(c.owner).compileInlineLevelRef1()
                                 )
                                 val iRef        = c.name
                                 opc match {
                                    
                                    case "PUTSTATIC" =>
                                       val assigendRef = opdStackTopmostItem(i = 0)
                                       s"${assigneeRef }.${iRef } = ${assigendRef } "
                                       .popoffPrependedWithDef()

                                    case "GETSTATIC" =>
                                       s"${assigneeRef }.${iRef } "
                                       .ldcPrependedWithDef()

                                 }
                                 
                              // case YyLoadOrStore(typeStr, "LOAD") =>
                              //    opdState0.storage.apply(c.`var` )
                              //    .toSingleWordNameString()
                              //    .prependedAll(s"/* $opcodeName ${c.`var` } */ ")
                              //    .ldcPrependedWithDef()

                           }
                           
                        case c: asm.tree.VarInsnNode =>
                           opcodeName match {

                              case YyLoadOrStore(typeStr, "STORE") =>
                                 // TODO
                                 yStorePrependedWithDef(locI = {
                                    c.`var`
                                 })
                                 
                              case YyLoadOrStore(typeStr, "LOAD") =>
                                 opdState0.storage.apply(c.`var` )
                                 .toSingleWordNameString()
                                 .prependedAll(s"/* $opcodeName ${c.`var` } */ ")
                                 .ldcPrependedWithDef()

                           }
                           
                        case c: asm.tree.LdcInsnNode =>
                           import c.cst
                           s"ldc ${cst.getClass().getSimpleName() }(${cst })"
                           .ldcPrependedWithDef()

                        case c: asm.tree.LabelNode =>
                           s"/* label: ${c.getLabel() } ; */"
                           .fixedOpdStackOp()

                        case c: asm.tree.FrameNode =>
                           opcodeName match {
                              case "F_NEW" | "F_FULL" =>
                                 Seq(
                                    s"/* new frame: L ${c.local } */ " ,
                                    s"/*            S ${c.stack } */ " ,
                                 ).mkString("\n")
                                 .fixedOpdStackOp()
                              case _ =>
                                 s"/* frame chg ${opcodeName }(......) */ "
                                 .fixedOpdStackOp()
                           }

                        case c: asm.tree.LineNumberNode =>
                           val lineNumber = c.line
                           val srcFileName = summon[Sdc].srcFileName
                           val ls = {
                              "" + srcFileName + ":" + lineNumber
                           }
                           s"/* line ${ls } */"
                           .fixedOpdStackOp()

                        case c =>
                           s"(opcode ${opcodeName })(.........)"
                           // .gotoPrependedWithDef()
                           .ldcPrependedWithDef()

                     }
                  }
         }

      }

      extension (c: org.objectweb.asm.tree.MethodInsnNode) {

         // TODO
         def toXJsString(
            opdState0: Jblt.OpdState[FqnStronumericPair[?] ],
            documentOriginalSrc: Boolean = true ,
            useIife: Boolean = false ,
            async: Boolean = true ,

         )(using InOpdCtx) : Jblt.OfStorageType[FqnStronumericPair[?] ] = {
            // s"${opcodeName } ${c.name }${c.desc } "
            ({
               // import scala.language.unsafeNulls
               import scala.jdk.CollectionConverters.*
               import org.objectweb.asm
               import cbsq.meta.asm.jvm.opcodeNameTable
               val odst = ({
                  NativeSigImpl(access = 0x0, name = c.name.nn, descriptor0 = {
                     NativeSigImpl.Bds.apply(descriptor = c.desc.nn, signature0 = null )
                  })
               })
               // TODO
               c.getOpcode() match {

                  case opc @ MethodInvocOpcode( ) =>
                     toXJsString21(
                        opc = opc ,
                        rct = {
                           asm.Type.getObjectType((
                              Option(c.owner).getOrElse("java/lang/$$asmMethodInvocInstrNoOwner")
                           ) ).nn
                        } ,
                        odst = odst ,
                        opdState0 = opdState0 ,
                        documentOriginalSrc = documentOriginalSrc ,
                        async = async ,
                     )

                  case _ =>
                     import scala.language.unsafeNulls
                     val opcodeName = opcodeNameTable(c.getOpcode() )
                     new Jblt {

                        override
                        val transliteratedForm = {
                           // s"${opcodeName } ${c.name }${c.desc } "
                           s"/* unsupported */ ${opcodeName } ${c.name }${c.desc } ;"
                        }
                        
                        override
                        val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                           opdState0
                        }
   
                     } : Jblt.OfStorageType[FqnStronumericPair[?] ]
                     // ???

               }
            })
         }

      }

      // TODO
      def toXJsString21(
         opc: Int ,
         rct: org.objectweb.asm.Type | Null , /* owner or receiver */
         odst: MethodDescriptorImpl1 ,
         opdState0: Jblt.OpdState[FqnStronumericPair[?] ],
         
         documentOriginalSrc: Boolean ,
         async: Boolean ,

      )(using InOpdCtx) : Jblt.OfStorageType[FqnStronumericPair[?] ] = {
                     // import scala.language.unsafeNulls

                     import scala.jdk.CollectionConverters.*
                     import org.objectweb.asm
                     import cbsq.meta.asm.jvm.opcodeNameTable

                     val opcodeTranslitAnalysis = (
                        analyseMethodInvocOpcTranslitImpl(MethodInvocOpcode.check(opc), odst, opdState0)
                     )
                     import opcodeTranslitAnalysis.*

                     val receiverAlias = ({
                        import asm.Opcodes
                        opc match {
                           
                           case Opcodes.INVOKEDYNAMIC | Opcodes.INVOKESTATIC =>
                              val compiledPrefix = {
                                 import eRpkImpl.{compileInlineLevelRef as compileInlineLevelRef1}
                                 rct.nn
                                 .compileInlineLevelRef1()
                              }
                              compiledPrefix
                              
                           case Opcodes.INVOKESPECIAL =>
                              val compiledRct = {
                                 import eRpkImpl.{compileInlineLevelRef as compileInlineLevelRef1}
                                 rct.nn
                                 .compileInlineLevelRef1()
                              }
                              val receiverRef = {
                                 vrs.head
                              }
                              ((compiledRct ++ "." ++ "$cbsqInvokespNsp" ++ s"($receiverRef)"))
                              
                           case Opcodes.INVOKEVIRTUAL | Opcodes.INVOKEINTERFACE =>
                              vrs.head

                        }
                     } : String)

                     val optionalReturnValueCompiledVarName = (
                        (opdStackState2.fromLeftRightwards.toSet diff opdStackState1.fromLeftRightwards.toSet )
                        .lastOption
                        .map(_.toSingleWordNameString() )
                     )

                     import util.matching.Regex.{quote, quoteReplacement}

                     val compiledCode = (
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
                           quoteReplacement(optionalReturnValueCompiledVarName match {

                              case Some(returnValCompiledVarName) =>
                                 s"const ${returnValCompiledVarName } = "

                              case None => 
                                 "/* V */ "

                           }) + "$0" + " ;"
                        )).nn
                        
                        match {
                        case s =>
                           s
                        }
                     ) 
                     
                     new Jblt {

                        override val transliteratedForm = compiledCode
                        
                        override
                        val resultingOpdState: Jblt.OpdState[FqnStronumericPair[?] ] = {
                           opdState0
                           .copy(
                              opdStack = {
                                 opdStackState2
                              } ,
                              lastItemgenState = {
                                 (opdStackState0.fromLeftRightwards ++ opdStackState2.fromLeftRightwards)
                                 .maxBy({
                                    case (_, i) =>
                                       i + (
                                          1
                                       )
                                 })
                              } ,
                           )
                        }
   
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



;
















































































































