package cbsq.meta.asm

















export cbsq.meta.asm.jvmc.{ERpkImplicits, ERpk }

export cbsq.meta.asm.jvmc.WSourceLevelSig

export cbsq.meta.asm.jvmc.Wsn

export cbsq.meta.asm.jvmc.WsnPwEmitter

export cbsq.meta.asm.jvmc.wsnImplCtx1

def wsnImpl() = {
   val wsni = wsnImplCtx1()
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
            if ({
               import scala.language.unsafeNulls
               import org.objectweb.asm
               import asm.Opcodes
               (
                  code != null
                  && ((
                     // (code.access & (Opcodes.ACC_NATIVE | Opcodes.ACC_ABSTRACT)) == 0 
                     0 < (code.nn : asm.tree.MethodNode).instructions.size()
                  ))
               )
            }) {
               import scala.language.unsafeNulls
               import scala.jdk.CollectionConverters.{ListHasAsScala, IterableHasAsScala }
               import org.objectweb.asm
               import cbsq.meta.asm.jvm.opcodeNameTable
               o println "  {"
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
               val initialStackState = ({
                  import cbsq.meta.asm.jvm.FqnStronumericPair
                  import cbsq.meta.asm.jvm.JbltOpdStackState
                  import cbsq.meta.asm.jvmc.Jblt
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
               val (
                  postArgsPopulativeStackState ,
                  
               ) = ({
                  import cbsq.meta.asm.jvm.FqnStronumericPair
                  import cbsq.meta.asm.jvm.JbltOpdStackState
                  import cbsq.meta.asm.jvmc.Jblt
                  
                  val nonReceiverArity = {
                        import language.unsafeNulls
                        import org.objectweb.asm

                        asm.Type.getType(dsc10.descriptor )
                        .getArgumentTypes().toIndexedSeq
                        .length

                  }

                  val srcArgRefs = (
                     Range(0, nonReceiverArity )
                     .map(srcArgIndex => (
                        s"args[$srcArgIndex]"
                     ))
                     .prependedAll[String]( Seq() :+ "this" )
                  )
                  assert(srcArgRefs.nonEmpty)

                  val s1 = (
                     srcArgRefs
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
                     .zip(srcArgRefs)
                     // .map(srcArgIndex => (
                     //    s"args[${srcArgIndex }]"
                     // ))
                     
                  ) ) {
                     /**
                      * dummy `InOpdCtx`
                      * only for its `toSingleWordNameString`
                      */
                     given instropc : cbsq.meta.asm.jvmc.InOpdCtx with {
                     }
                     import instropc.toSingleWordNameString

                     o println s"    const ${sName.toSingleWordNameString() } = $srcArgRef ; "

                  }

                  // assert(s1.storage.nonEmpty)
                  (
                     s1
                     ,
                  )
               })
               for (((instr, instrOrdinal), opdState) <- ({
                  import cbsq.meta.asm.jvm.FqnStronumericPair
                  import cbsq.meta.asm.jvmc.Jblt

                  (code.nn : asm.tree.MethodNode)
                  .instructions
                  .asScala
                  .toSeq
                  .zipWithIndex
                  .unfolding[Jblt.OpdState[FqnStronumericPair[?] ] ]((
                     postArgsPopulativeStackState
                     
                  ))({ case (opdState, (instr, instrOrdinal) ) => {
                  ;
                  
                  given cbsq.meta.asm.jvmc.InOpdCtx with {
                  }
                  /**
                   * a *label* might indicate possibly-large linebreak and
                   * the generated JS would likely have been way congested
                   */
                  if instr.isInstanceOf[asm.tree.LabelNode] then {
                     o.println()
                  }
                  val instrOutcomeAnalysed = {
                     instr.toJsBlockLevelStmt(opdState0 = opdState )
                  }
                  import instrOutcomeAnalysed.resultingOpdState
                  val instrS = {
                     instrOutcomeAnalysed.transliteratedForm
                  }
                  o.println(s"${instrS.indent(2 * 2).dropRight(1) }" )
                  
                  // TODO
                  resultingOpdState
                  }})
               }) ) {
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
            .sortBy((identity[NativeSigImpl]).andThen({
               case e => 
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
            }).compose[(NativeSigImpl, org.objectweb.asm.MethodVisitor)](_._1))
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





























































































