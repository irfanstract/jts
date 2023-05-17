package cbsq.meta.asm.jvmc
















trait ERpkImplicits extends
   AnyRef
   with cbsq.meta.asm.jvm.ClassNamesAnalyser
   with cbsq.meta.asm.jvmc.ClassNamesCompiler
{ eRpkImpl =>

   // val boxingImplicits : (
   //    AnyRef
   //    with cbsq.meta.asm.jvm.ClassNamesAnalyser
   //    with cbsq.meta.asm.jvmc.ClassNamesCompiler
   // ) = eRpkImpl

}

trait ERpk extends
   AnyRef
   with cbsq.meta.asm.jvm.ClassNamesAnalyser
   with cbsq.meta.asm.jvmc.ClassNamesCompiler
   with ERpkImplicits
{

   extension (name0: ow.Type) {

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
   def rpkName(superName: ow.Type): String

   val canDropPrivateMethod: Boolean
   
   /**
    * 
    * TODO .
    * an ES/JS file containing only any of `"use strict"` or `import ... from ...` or `// YOUR CODE GOES HERE`
    *
    */
   @deprecated("TODO")
   def getBaseTemplate() : WsnPwEmitter

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

      val canDropPrivateMethod: Boolean = {
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
      
   }
   /* avoid using wildcard imports as there are other stuffs */ 
   export eRpkImpl.getBaseTemplate
   export eRpkImpl.canDropPrivateMethod
   export eRpkImpl.{getCanonicalName, getSimpleName}
   export eRpkImpl.{compileInlineLevelRef}

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

































