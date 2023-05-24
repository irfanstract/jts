package cbsq.meta.asm

















export cbsq.meta.asm.jvmc.{ERpkImplicits, ERpk }

export cbsq.meta.asm.jvmc.WSourceLevelSig

export cbsq.meta.asm.jvmc.Wsn

export cbsq.meta.asm.jvmc.WsnPwEmitter

export cbsq.meta.asm.jvmc.wsnImplCtx1

def wsnImpl(
   generatedJsConfig: cbsq.meta.asm.jsgen.TsConfig ,

) = {
   val wsni = {
      wsnImplCtx1(generatedJsConfig = generatedJsConfig)
   }
   import wsni.*

   sealed case class forConfig(
      buildClassDef: (org.objectweb.asm.ClassVisitor) => Unit ,
   ) extends AnyRef with Wsn 
   {

      private [forConfig]
      class Rendered {
      //
      
      val classfileRepr1 = {
         import scala.language.unsafeNulls
         import org.objectweb.asm
         val cv = new asm.tree.ClassNode(asm.Opcodes.ASM9 )
         buildClassDef.apply(cv)
         cv.visitEnd()
         cv
      }

      val name = {
         import scala.language.unsafeNulls
         classfileRepr1.name
      }

      val superName = {
         import scala.language.unsafeNulls
         classfileRepr1.superName
      }

      val simpleName = ({
         import language.unsafeNulls
         ow.Type.getObjectType(name)
         .getSimpleName()
      })

      val superclassSimpleName = ({
         import language.unsafeNulls
         ow.Type.getObjectType(superName)
         .getSimpleName()
      })

      val methodsByTsDescMap = {
         import language.unsafeNulls
         import scala.jdk.CollectionConverters.*
         classfileRepr1.methods.asScala
         .toIndexedSeq
         .map(m => ((
            NativeSigImpl(
               access = m.access ,
               name = m.name ,
               descriptor0 = (
                  NativeSigImpl.Bds(
                     descriptor = m.desc ,
                     signature0 = m.signature ,
                  )
               ) ,
            )
         ), m ))
         .toMap
      }
      val methodsByTsDescs = {
         methodsByTsDescMap
         .toSet
         .map((m, _) => m )
      }

      val hideworthyMethods = (
            methodsByTsDescs
            .filter(e => {
               e.isEffectivelyPrivate()
            })
      )
      val hideableMethodsEffectively = (
            if canOmitPrivateMethods then hideworthyMethods
            else Set()
      )

      }
      
      type Derived >: Wsn <: Wsn
      
      def withNewFullName(
         ownName            : ow.Type ,
         superclassName     : ow.Type,
         superInterfaces    : IndexedSeq[ow.Type]
      ): Derived = {
         import language.unsafeNulls
         // TODO
         copy(
            buildClassDef = (clv) => {
               import scala.language.unsafeNulls
               import org.objectweb.asm
               if (false) {
                  buildClassDef(clv)
               }
               clv.visit((
                  // TODO
                  asm.Opcodes.V11 /* `constantDynamic` */
               ) , {
                  // TODO
                  import asm.Opcodes
                  Opcodes.ACC_PUBLIC
               } , ownName.getInternalName(), null, superclassName.getInternalName(), {
                  superInterfaces
                  .map(t => t.getInternalName() )
               }.toArray )
            } ,
         )
      }
      
      type NativeSig
         >: NativeSigImpl
         <: NativeSigImpl

      override
      def translateIntoNativeSig(sig: WSourceLevelSig) : NativeSig = {
         import org.objectweb.asm.Opcodes
         sig
      }

      override
      def withAddedMethodByNativeSig(
         sig: NativeSig,
         buildMethod: (org.objectweb.asm.MethodVisitor) => Unit ,
      ): Derived = {
         copy(
            buildClassDef = (clv) => {
               import scala.language.unsafeNulls
               import org.objectweb.asm
               buildClassDef(clv)
               val mv1 = (
                  clv.visitMethod(sig.access, sig.name, sig.descriptor, sig.signature, null)
               )
               buildMethod(mv1)
            },
         )
      }

      def withAddedMethod(sig: WMNs) = {
         throw UnsupportedOperationException()
      }

      override
      type WMNs <: Nothing

      extension (o: java.io.PrintWriter) {

         /**
          * 
          * print a static-or-nonstatic method-def as described,
          * unconditionally
          * 
          */
         def printXClassMethodDefUnconditionally(
            dsc10: NativeSigImpl,
            code: Null | org.objectweb.asm.tree.MethodNode
         )(using cbsq.meta.asm.jvmc.Sdc) = {
            import cbsq.meta.asm.jvmc.toJsMethodDeclString
            
            import cbsq.meta.asm.jvmc.{isSynthetic, isEffectivelyPrivate}

            val dsc1 = (
               dsc10
               .toJsMethodDeclString()
            )

            val getOriginalSigE : () => ((NativeSigImpl.Fmtct) ?=> String) = {
               if (code != null) then
                  import org.objectweb.asm
                  import asm.Opcodes
                  () => (fmtCtx) ?=> ((code: asm.tree.MethodNode) => {
                     code.name.nn + {
                        Option(code.signature)
                        .filter(_ => fmtCtx.generics )
                        .getOrElse(code.desc.nn )
                     }
                  })(code.nn )
               else (
                  () => (c) ?=> dsc10.toShortString()(using c )
               )
            }

            o.println(s"  /**")
            o.println(s"   * ")
            o.println(s"   * method `${dsc10.name }` ")
            o.println(s"   * ")
            o.println(s"   * ")
            o.println(s"   * @note  ")
            // o.println(s"   *    output by/from J2JS. ")
            // o.println(s"   *    needs be *async* due to how JS works. ")
            o.println(s"   *    sig in the original bytecode: ")
            o.println(s"   *    `${getOriginalSigE()(using NativeSigImpl.Fmtct(generics = false ) ) }`. ")
            o.println(s"   *    `${getOriginalSigE()(using NativeSigImpl.Fmtct(generics = true ) ) }`. ")
            o.println(s"   * ")
            o.println(s"   */ ")
            o.println(s"  /* the return-type varies depending on actual 'args' */ ")
            o println (({
               import scala.language.unsafeNulls
               dsc1
               .replaceFirst("\\A\\s*", "  ")
               .replaceFirst(";\\s*\\z", "")
            }) )

            if ((
               (generatedJsConfig.isForTypeDeclarationFile == false)
               
            ) && {
               import scala.language.unsafeNulls
               import org.objectweb.asm
               import asm.Opcodes
               code.isEffectivelyNonAbstract()
            }) {
               
               import scala.language.unsafeNulls
               
               import scala.jdk.CollectionConverters.{ListHasAsScala, IterableHasAsScala }
               import org.objectweb.asm

               import cbsq.meta.asm.jvm.opcodeNameTable

               import cbsq.meta.asm.jvm.FqnStronumericPair
               import cbsq.meta.asm.jvm.JbltOpdStackState
               import cbsq.meta.asm.jvmc.Jblt

               o println "  {"

               if true then {
                  o println s"    /* "
                  o println s"     * "
                  o println s"     * ${code.instructions.size() } instructions (including labels) "
                  o println s"     * max store    : ${code.maxLocals } "
                  o println s"     * max stack    : ${code.maxStack  } "
                  o println s"     * "
                  o println s"     */"
               }

               val operandsFmt = {
                  "code$<index>$operands"
               }

               def operandsForIndex(instrOrdinal: Int): String = {
                        import scala.util.matching.Regex.{quote, quoteReplacement}
                        operandsFmt
                        .replaceAll(quote("<index>"), ((instrOrdinal).toString().reverse.padTo(3, '0').reverse) )
               }
               
               o println s"    /* "
               o println s"     * "
               o println s"     * J2JS detail : "
               o println s"     * when(ever) an opcode produces return-value, "
               o println s"     * a fresh `const` will be assigned to reference the return-value. "
               o println s"     * the name of the newly-introduced `const` "
               o println s"     * will be tracked on *the hot-stack* and, possibly, *the store* "
               o println s"     * "
               o println s"     */"

               /**
                * dummy `InOpdCtx`
                * only for its `toSingleWordNameString`
                */
               given instropc : cbsq.meta.asm.jvmc.InOpdCtx with {
               }
               import instropc.toSingleWordNameString
               extension(ls: collection.Iterable[cbsq.meta.asm.jvm.FqnStronumericPair[?] ]) {

                  def toNameLsString(): String = {
                     ls
                     .map(_.toSingleWordNameString() )
                     .map("" + _ + ", ").mkString("[" , "" , "]")
                  }

               }
               
               extension (opdState : cbsq.meta.asm.jvmc.Jblt.OpdState[cbsq.meta.asm.jvm.FqnStronumericPair[?] ]) {
                  
                  def printIStorageAndHotStackInfo(): Unit = {
                     o println s"    /* stored: ${opdState.storage.toNameLsString() } ; hot-st: ${opdState.opdStack.fromLeftRightwards.toNameLsString() } */"
                  }
                  
                  def printIStorageInfo(): Unit = {
                     o println s"    /* stored: ${opdState.storage.toNameLsString() } */"
                  }
                  def printHotStackInfo(): Unit = {
                     o println s"    /* hot-st: ${opdState.opdStack.fromLeftRightwards.toNameLsString() } */"
                  }
                  
               }
               
               val initialStackState = ({
                  Jblt.OpdState[FqnStronumericPair[?] ](

                     // TODO
                     opdStack = (
                        JbltOpdStackState.byFromLeftRightwards((
                           IndexedSeq()
                        ))
                     ) ,
                     storage = IndexedSeq.empty ,

                     lastItemgenState = ("lclv", 1) ,
                     
                  )
               })
               
               extension (ce : (
                  Jblt.OpdState[FqnStronumericPair[?] ] ,
                  Seq[(asm.tree.AbstractInsnNode, Int)] ,
               )) {
               
               def xCompileInstructionListAndEmit(

                  disallowBackwardsJump1: Boolean = false,

               ): Unit = {
               val (preLoop1OpdState, ops) = ce
               
               for (((instr, instrOrdinal), opdState) <- ({
                  ops
                  .unfolding[Jblt.OpdState[FqnStronumericPair[?] ] ]((
                     preLoop1OpdState
                     
                  ))({ case (opdState, (instr, instrOrdinal) ) => {
                  ;
                  
                  extension (s : String) {
                     def indentWithCurrentInstrOrdinal: String = {
                        s
                        .split({ "\\r?\\n" })
                           .map(l => s"/* #$instrOrdinal */ $l ")
                           .mkString("\n")
                     }
                  }
                  
                  given cbsq.meta.asm.jvmc.InOpdCtx with {
                     
                     override
                     lazy val disallowsBackwardsJump: disallowBackwardsJump1.type = {
                        disallowBackwardsJump1
                     }
                     
                  }
                  val instrOutcomeAnalysed = {
                     instr.toJsBlockLevelStmt(opdState0 = opdState )
                  }
                  import instrOutcomeAnalysed.resultingOpdState
                  val instrS = {
                     instrOutcomeAnalysed.transliteratedForm
                     .indentWithCurrentInstrOrdinal
                  }
                  
                  /**
                   * a *label* might indicate possibly-large linebreak and
                   * the generated JS would likely have been way congested
                   */
                  if instr.isInstanceOf[asm.tree.LabelNode] then {
                     o.println()
                  }
                  if (
                     false
                     || instr.isInstanceOf[asm.tree.FrameNode]
                     || instr.isInstanceOf[asm.tree.LabelNode]
                     || ({
                        import cbsq.meta.asm.jvm.opcodeNameTable
                        val GivenTypedLoadOrStore = "(\\w)(LOAD|STORE)".r
                        
                        opcodeNameTable(instr.getOpcode() ) match {

                           case GivenTypedLoadOrStore(_*) =>
                              true

                           case _ =>
                              false
                              
                        }
                     })
                  ) then {
                     opdState.printIStorageAndHotStackInfo()
                  }
                  if (instrOrdinal % 10) == 0 then {
                     opdState.printIStorageInfo()
                  }
                  if (
                     false
                     // || (instrOrdinal % 10) == 0
                     // || {
                     //    import cbsq.meta.asm.jvm.opcodeNameTable
                     //    opcodeNameTable(instr.getOpcode() ) match {
                     //       case s =>
                     //          "LOAD|DUP|CONST|LDC|GET|INVOKE|PUT|STORE".r
                     //          .findAllIn(s)
                     //          .nonEmpty
                     //    }
                     // }
                     || (
                        summon[PartialFunction[asm.tree.AbstractInsnNode, Unit]](using {
                           case _ : asm.tree.MethodInsnNode =>
                           case _ : asm.tree.InvokeDynamicInsnNode =>

                        }) isDefinedAt instr 
                     )
                  ) then {
                     opdState.printHotStackInfo()
                  }
                  o.println(s"${instrS.indent(2 * 2).dropRight(1) }" )
                  
                  ({
                     import cbsq.meta.asm.jvm.opcodeNameTable
                     opcodeNameTable(instr.getOpcode() ) match {

                     case s =>
                        if (
                           "\\w(LOAD|STORE)".r
                           .findAllIn(s)
                           .nonEmpty
                        ) then {
                           resultingOpdState.printIStorageAndHotStackInfo()
                        }
                        if (
                           "\\w(STORE)".r
                           .findAllIn(s)
                           .nonEmpty
                        ) then {
                           /* unified above */
                        }
                        
                        if (
                           "\\A(?:INVOKE|POP\\d*|\\wCONST|LDC|DUP)".r
                           .findAllIn(s)
                           .nonEmpty
                        ) then {
                           resultingOpdState.printHotStackInfo()
                        } /* if "\\A(?:INVOKE|POP\\d*|\\wCONST|LDC|DUP)".r matches */

                     }
                  })
                  
                  // TODO
                  resultingOpdState
                  }})
               }) ) {
               }
               }

               }
               
               type XBlockSeparativeNode = (
                  // asm.tree.LabelNode
                  asm.tree.FrameNode
               )
               
               extension [I](instrs: Seq[(asm.tree.AbstractInsnNode, I)] ) {

                  /**
                   * 
                   * split at every "block-separative" instruction
                   * 
                   */
                  def splitAtLabels() = {

                     instrs
                     .foldLeft[Vector[Seq[(asm.tree.AbstractInsnNode, I) ] ] ](Vector() )({
                           
                        case (s0 :+ lastG, nextItrItem @ (_ : (
                           asm.tree.FrameNode
                        ), _)) =>
                           val lastLabel = {
                              lastG
                              .reverse
                              .collectFirst({
                                 case e @ (_ : asm.tree.LabelNode, _) =>
                                    e
                              })
                              .get
                           }
                           (s0 :+ (lastG :+ nextItrItem ) ) :+ (
                              Seq(
                                 lastLabel,
                                 nextItrItem, 
                              )
                           )
                           
                        case (s0 :+ lastG, nextItem @ (nextInstr, _) ) if (!(nextInstr.isInstanceOf[XBlockSeparativeNode] ) ) =>
                           s0 :+ (lastG :+ nextItem )
                           
                        case (Seq(), nextItem) =>
                           Vector() :+ Seq(nextItem )

                     })
                     
                  }

               }
               
               val nonReceiverArity = {
                     import language.unsafeNulls
                     import org.objectweb.asm
                     
                     asm.Type.getType(dsc10.descriptor )
                     .getArgumentTypes().toIndexedSeq
                     .length
                     
               }
               
               /**
                * 
                * the args, including conditional `this`
                * 
                */
               val srcArgRefs = (
                  Range(0, nonReceiverArity )
                  .map(srcArgIndex => (
                     s"args[$srcArgIndex]"
                  ))
                  .prependedAll[String]({
                     import asm.Opcodes
                     if (
                        (Opcodes.ACC_STATIC & (code : asm.tree.MethodNode).access )
                        == 0
                     ) then
                        Seq.empty :+ "this"
                     else Seq.empty
                  })
               )
               // assert(srcArgRefs.nonEmpty)
  
               {
               
               val (
                  postArgsPopulativeStackState ,
                  
               ) = ({
                  
                  /**
                   * 
                   * the initial-value expr(s) for the local(s), in-order
                   * 
                   * needs to be padded to `maxLocals`,
                   * due to the handling of `goto`s
                   * 
                   */
                  val localsInitialisers = {
                     srcArgRefs
                     
                     .padTo(len = {
                        (code : asm.tree.MethodNode).maxLocals
                        // .`+`(2 )

                     }, elem = "null" )
                     
                  }
                  
                  val s1 = (
                     localsInitialisers
                     .foldLeft[Jblt.OpdState[FqnStronumericPair[?] ] ]((
                        initialStackState

                     ))((s0, _) => (
                        s0
                        .afterLdcOpaque
                        .afterYStoreOpc(destStorageIndex = {
                           s0.storage
                           .length
                        })
                     ) )
                  )

                  for ((sName, srcArgRef) <- (
                     s1.storage
                     .zip(localsInitialisers)
                     // .map(srcArgIndex => (
                     //    s"args[${srcArgIndex }]"
                     // ))
                     
                  ) ) {

                     val keyw = (
                        if srcArgRef matches "undefined|null|_|unini?tiali[sz]ed" then
                           "let"
                        else "const"
                     )
                     o println s"    $keyw ${sName.toSingleWordNameString() } = $srcArgRef ; "

                  }

                  // assert(s1.storage.nonEmpty)
                  (
                     s1
                     ,
                  )
               })
               
               val preInstrItrLoopStackState1 = (
                  postArgsPopulativeStackState
                  
               )

               
               o println ""
               
               o println s"    let S_exception = null ;"

               o println ""

               o println s"    loop1 :  "
               o println s"    for (let nextBranch = 1;; ) {  "
               o println s"       "
               o println s"      switch ([nextBranch, /* clear it */ nextBranch = 0][0] ) {   "
               o println s"       "

               def printGoToBranchId(id: Int): Unit = {
                  o println s"        nextBranch = $id ; continue loop1 ; "
               }

               ({

                  (code.nn : asm.tree.MethodNode)
                  .instructions
                  .asScala
                  .toSeq
                  .zipWithIndex
                  
               })
               .splitAtLabels()
               .zipWithIndex.map((v, i) => (1 + i, v ) )
               /**
                * 
                * process every "block"
                * 
                * by the definition in `asm.Opcode` ,
                * all *stack-map* compression(s) only apply to "locals", and
                * none occur to the hot-stack,
                * so
                * we can avoid tracking, across "block"s, the state of the hot-stack
                * 
                */
               .map((currentBranchId, instructions) => {
                  val opdState10 = (
                     preInstrItrLoopStackState1
                  )
                  val startingFrameO = (
                     instructions
                     .collectFirst({ case (e : asm.tree.FrameNode, _) => e })
                  )
                  val startingStackItems1 = ({
                     import language.unsafeNulls
                     import scala.jdk.CollectionConverters.*

                     startingFrameO
                     .flatMap(e => Option(e.stack.asScala) )
                     .toList
                     .flatten

                  })
                  // TODO
                  o println s"      case $currentBranchId :   "
                  o println s"      { "
                  
                  val opdState11 = ({

                     startingStackItems1
                     .foldLeft[Jblt.OpdState[FqnStronumericPair[?] ]](opdState10)((opdState10, _) => {
                        // TODO

                        val opdState11 = {
                           opdState10.afterLdcOpaque
                        }

                        val catchLocalVarName = (
                           opdState11
                           .opdStack
                           .fromRightLeftwards.head
                           .toSingleWordNameString()
                        )

                        o println s"        const ${catchLocalVarName } = [S_exception, /* clear it */ S_exception = null ][0] ;  "
                        
                        opdState11
                     } )
                     
                  })
                  
                  o println s"         "
                  o println s"        /* ${instructions.length } instructions */ "
                  o println s"    "
                  // o println s"        throw ;  "
                  (instructions, opdState11 ).swap
                  .xCompileInstructionListAndEmit()
                  o println s"    "
                  // o println s"        throw ;  "
                  printGoToBranchId(id = currentBranchId + 1 )
                  o println s"      throw Error(`[branch $currentBranchId] must reassign 'nextBranch' and 'continue' ; can't fall-thru `) ; "
                  o println s"    "
                  o println s"      } "
                  o println s"       "
               })
               
               o println s"       "
               o println s"      default :  "
               o println "        throw Error(`invalid branch ID: ${nextBranch } `) ; "
               o println s"       "

               // TODO
               o println s"      }   "
               o println s"       "
               o println "      throw Error(`must reassign 'nextBranch' and 'continue' `) ; "
               o println s"     "
               o println s"    }"
               
               }

               o println "  }"

            }
            else {
               import scala.language.unsafeNulls
               o println "  ;"
            }
            
            o.println()

         }
         
      }

      val methodNamesOrdering: Ordering[NativeSigImpl] = {
         (
               Ordering.by((e: NativeSigImpl) => {
                  import org.objectweb.asm.Opcodes
                  import e.{access as acc  }
                  import e.{name   as nm   }
                  
                  (
                     acc.&(Opcodes.ACC_BRIDGE) ,
                     acc.&(Opcodes.ACC_SYNTHETIC) ,
                     acc.&(Opcodes.ACC_STATIC) ,
                     "public protected package-private".indexOf(e.visibility ) & ~Int.MinValue ,
                     "\\$(?!a?sync(?:hronous|)|tupled?|\\d+\\z)".r.findFirstIn(nm).nonEmpty ,
                     // "\\$(?!tupled)".r.findFirstIn(nm).nonEmpty ,
                     nm ,
                  )
               })
         )
      }

      lazy val distilledFormPwEmitter = ({
      ;

      val cvrev = Rendered()

      import cvrev.*

      extension (o: java.io.PrintWriter) {

      /**
       * 
       * the set of methods defined by JRE's `java.lang.Object` and
       * that defined by TC39's `Object.prototype` is very close, and
       * the relevant W3C's standards joined the convergence.
       * simply add some missing methods from `java.lang.Object`, and
       * it fits in (...)ly
       * 
       * print-out a commented-out code-section defining the minimum-union between them
       * 
       */
      def printXTc39sObjectPrototypeHolyGrailDisabled(): Unit = {
         o.println(s"//                   ")
         o.println(s"//   constructor() ; ")
         o.println(s"//                   ")
         o.println(s"//   equals<That>(thatOne: That ): boolean ;")
         o.println(s"//   hashCode(): string ;")
         o.println(s"//                   ")
         o.println(s"// //  toJSON(): null | {} ;")
         o.println(s"// //  clone(): $simpleName ;")
         o.println(s"//                   ")
         o.println(s"// //  close(): void ;")
         o.println(s"// //  [Symbol.asyncDispose](): Promise<void> ;")
         o.println(s"// //  finalize(): void ;")
         o.println(s"// //  [Symbol.synchronized]: Thread.SyncronizedBlockSupport ;")
         o.println(s"//                   ")
         o.println(s"//   toString(): string ;")
         o.println(s"//   toLocaleString(): string ;")
         o.println(s"//   get [Symbol.toStringTag](): string ;")
         o.println(s"//                   ")
      }

      }
      
      WsnPwEmitter((o: java.io.PrintWriter) => {
         import language.unsafeNulls
         val baseTemplate = (
            getBaseTemplate()
         )
         o println(baseTemplate.toString() : String )
         o.println()
         o.println(s"declare class $simpleName extends ${ow.Type.getObjectType(superName).compileInlineLevelRef() } ")
         o.println("{")
         o.println()
         o.printXTc39sObjectPrototypeHolyGrailDisabled()
         o.println()
         o.println()
         for ((dsc10, code10B) <- (
            methodsByTsDescMap
            .filterKeys(nameAndSig2 => !(hideableMethodsEffectively contains nameAndSig2) )
            .toSeq
            .sorted(using (
               
               methodNamesOrdering
               match {
                  case ord =>
                     Ordering.by((_: (NativeSigImpl, org.objectweb.asm.MethodVisitor) )._1 )(using ord)
               }

            ))
         )) {
            o.printXClassMethodDefUnconditionally(dsc10, code = code10B)(using {
               new cbsq.meta.asm.jvmc.Sdc {

                  override
                  val srcFileName: String = {
                     Option(classfileRepr1.sourceFile )
                     .getOrElse("(no src)")
                  }
                  
               }
            })
         }
         o.println()
         locally {
            import cbsq.meta.asm.jvmc.printXHiddenMethodsCommentSection
            ;
            o printXHiddenMethodsCommentSection(hideableMethodsEffectively)
         }
         o.println()
         o.println(s"                   ")
         o.println("}")
         o.println(s"export = $simpleName ;")
      })
      })

   }
   
   forConfig(
      buildClassDef = (v) => {} ,
   )
}

export cbsq.meta.asm.jvmc.withJsSpecificMethods 

export cbsq.meta.asm.jvmc.asMakingAsyncifiedVariants
export cbsq.meta.asm.jvmc.asMakingTupledVariants

trait Sgde
{
   val needsExportAssignment: Boolean
}

export cbsq.meta.esm.{ExportAllAssignmentKindEnum, ExportAllAssignmentKindEnumImpl }





























































































