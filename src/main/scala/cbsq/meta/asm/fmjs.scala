package cbsq.meta.asm



















export cbsq.meta.asm.jvmc.{ERpkImplicits, ERpk }

def fcv(o: java.io.PrintWriter) : ow.ClassVisitor = {
   import org.objectweb.asm
   new asm.ClassVisitor(asm.Opcodes.ASM9) {

      var s : Wsn = (
         wsnImpl( )
      )
      
      override
      def visitAttribute(attribute: asm.Attribute): Unit = {
         println(attribute.toString() )
         println(attribute.`type` )
      }

      override
      def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
         ;
         import asm.Type
         s = ({
            import language.unsafeNulls
            s.withNewFullName(
               ownName             = Type.getObjectType(name) ,
               superclassName      = Type.getObjectType(superName) ,
               superInterfaces     = interfaces.toIndexedSeq.map(Type.getObjectType _ ) ,
            )
         })
      }
      
      override
      def visitMethod(access: Int, name: String, descriptor: String, signature: Null | String, exceptions: Array[String]): asm.MethodVisitor = {
         ;
         val cmv = (
            new asm.tree.MethodNode()
         )
         def onVisitEnd() : Unit = {
            s = ((s: Wsn) => (
               s
               .withAddedMethodByNativeSig((
                  s
                  .translateIntoNativeSig(({
                     import cbsq.meta.asm.jvm.MethodDescriptorImpl1
                     MethodDescriptorImpl1(
                        access = access ,
                        name = name ,
                        descriptor0 = (
                           MethodDescriptorImpl1.Bds(descriptor, signature)
                        ) ,
                     )
                  }))
               ) , mv => cmv.accept(mv) )
            ))(s)
            
         }
         new asm.MethodVisitor(asm.Opcodes.ASM9, cmv ) {

            override
            def visitEnd(): Unit = {
               super.visitEnd()
               onVisitEnd()
            }
            
         }
      }

      override
      def visitEnd(): Unit = {
         s.distilledFormPwEmitter
         .printTo(o)
      }
      
   }
}

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
      
      val cRevisited = {
         import scala.language.unsafeNulls
         import org.objectweb.asm
         val cv = new asm.tree.ClassNode(asm.Opcodes.ASM9 )
         buildClassDef.apply(cv)
         cv.visitEnd()
         cv
      }

      val name = {
         import scala.language.unsafeNulls
         cRevisited.name
      }

      val superName = {
         import scala.language.unsafeNulls
         cRevisited.superName
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
         cRevisited.methods.asScala
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
         def printXClassMethodDefUnconditionally(dsc10: NativeSigImpl, code: Null | org.objectweb.asm.tree.MethodNode) = {
            import cbsq.meta.asm.jvmc.toJsMethodDeclString
            import cbsq.meta.asm.jvmc.{isSynthetic, isEffectivelyPrivate}
            val dsc1 = (
               dsc10
               .toJsMethodDeclString()
            )
            o.println(s"  /**")
            o.println(s"   * ")
            o.println(s"   * method `${dsc10.name }` ")
            o.println(s"   * ")
            o.println(s"   * ")
            o.println(s"   * @note  ")
            // o.println(s"   *    output by/from J2JS. ")
            // o.println(s"   *    needs be *async* due to how JS works. ")
            o.println(s"   *    sig in the original bytecode: ")
            o.println(s"   *    `${dsc10.toShortString()(using NativeSigImpl.Fmtct(generics = false ) ) }`. ")
            o.println(s"   *    `${dsc10.toShortString()(using NativeSigImpl.Fmtct(generics = true ) ) }`. ")
            o.println(s"   * ")
            o.println(s"   */ ")
            o.println(s"  /* the return-type varies depending on actual 'args' */ ")
            o println (dsc1.replaceFirst("\\A\\s*", "  ") )
            if (code != null) {
               import scala.language.unsafeNulls
               import scala.jdk.CollectionConverters.*
               import org.objectweb.asm
               import cbsq.meta.asm.jvm.opcodeNameTable
               o println "  {"
               for (instr <- (code.nn : asm.tree.MethodNode).instructions.asScala) {
                  val instrS = {
                     val opcodeName = (
                        opcodeNameTable.apply(instr.getOpcode())
                     )
                     instr match {
                        case c: asm.tree.InsnNode =>
                           s"${opcodeName }"
                        case c: asm.tree.MethodInsnNode =>
                           s"${opcodeName } ${c.name }${c.desc } "
                        case c: asm.tree.LdcInsnNode =>
                           import c.cst
                           s"ldc ${cst.getClass().getSimpleName() }(${cst })"
                        case c =>
                           s"${opcodeName }(...)"
                     }
                  }
                  o.println(s"    ${instrS} ;" )
               }
               o println "  }"
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
            o.printXClassMethodDefUnconditionally(dsc10, code = code10B)
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

@main
def fcvDemo101(): Unit = {
   val path = (
      // "src\main\resources\jbc-transform\samples\bytebuffers1$package$ByteBlob$.class"
      // getClass()
      // .getResource("/jbc-transform/samples/bytebuffers1$package$ByteBlob$.class")
      getClass()
      // .getResource("/jbc-transform/samples/byteManipImplicits$.class")
      .getResource("/jbc-transform/samples/bytebuffers1$package$ByteBlob$.class").nn
   )
   val cr = (
      new org.objectweb.asm.ClassReader((
         path
         .openStream().nn
      ))
   )
   import cbsq.meta.asm.jvmc.amevMonadificativeImpl.defaultTsConfig
   //
   cr
   .accept((
      fcv((
         stdOutWriter(System.out.nn)
         .asPrintWriter()
      ) )
      .withJsSpecificMethods()
      .asMakingAsyncifiedVariants()
      // .asMakingTupledVariants()
   ) , org.objectweb.asm.ClassReader.SKIP_FRAMES )
}





















