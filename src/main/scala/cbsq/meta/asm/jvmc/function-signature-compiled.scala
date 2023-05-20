package cbsq.meta.asm.jvmc


















/**
 * 
 * to make the return-type depend on the actual args-type
 * a dependent-typing will be needed .
 * 
 * higher-level code
 * should never be directly concerned with the actual impl.
 * 
 */
trait CompiledArgsDependendTypingSchemeImplicits1 {
   
   extension (nameAndSignature: MethodDescriptorImpl1) {

      /**
       *
       * compute
       * the expression denoting
       * the type which `args` shall implements
       * 
       */
      def computeCompiledArgsTypeName(): String

      /**
       *
       * compute
       * the expression denoting
       * the type which the return-value shall implements
       * (based on `args`)
       * 
       */
      def computeCompiledReturnTypeName(): String
      
   }

}

/**
 * 
 * a basic [[CompiledArgsDependendTypingSchemeImplicits1]] impl
 * 
 */
@deprecated
lazy val basicCctsi = {
new AnyRef with CompiledArgsDependendTypingSchemeImplicits1 {

extension (this1: MethodDescriptorImpl1) {
   
         //
         transparent inline
         def computeCompiledArgsTypeName(): String = {
            import language.unsafeNulls
            import this1.{access, name, descriptor }
            val argsTypeRef = (
               name
               // .toUpperCase(java.util.Locale.ROOT)
               // .prependedAll("$$Args$$")
               .appendedAll("$" + (name + descriptor).hashCode().&(~Int.MinValue) )
               .replaceAll("\\W", java.util.regex.Matcher.quoteReplacement("$") )
               .appendedAll("$Args")
            )
            argsTypeRef
         }
         
         //
         def computeCompiledReturnTypeName(): String = {
            import language.unsafeNulls
            {
               import util.matching.Regex.{quote, quoteReplacement}
               import cbsq.meta.asm.jsgen.formatExactPathTypeQuery
               import cbsq.meta.asm.jsgen.QualifiedName
               this1.computeCompiledArgsTypeName()
               .replaceFirst("\\z" prependedAll(quote("$Args").++:("(?:").++(")?") ), quoteReplacement("$Return"))
               .++({
                  val ARGS = QualifiedName.parse("args")
                  s"<(${formatExactPathTypeQuery(ARGS ) })>"
               })
            }
         }
         
}

}
}

extension (this1: MethodDescriptorImpl1) {


         def toJsMethodName(): String = {
            import this1.{access, name, descriptor }
            import org.objectweb.asm.Opcodes
            ({
               name match {
                  case s @ ("constructor" | "__proto__" | "prototype") => 
                     "[\"" + s + "\"]"
                  case "<init>" => 
                     "constructor"
                  case _: String => 
                     name
               }
            }: String)
         }

         def toJsMethodDeclString(): String = ({
            import this1.{access, name, descriptor }
            import org.objectweb.asm.Opcodes
            import basicCctsi.*
            val argsTypeRef = (
               this1.computeCompiledArgsTypeName()
            )
            (
               "" + this1.toJsMethodName() + ({
                  descriptor match {
                     case _ =>
                        // descriptor
                        // "(...args ): undefined | null | {}"
                        ({
                           val returnTypeStr = (
                              if (name.matches("\\<init\\>|clone|equals|hashCode|toArray|to(?:Locale)?String|valueOf") ) then
                                 "unknown"
                              else {
                                 import language.unsafeNulls
                                 import util.matching.Regex.{quote, quoteReplacement}
                                 // "Promise<unknown>"
                                 // "undefined | null | {}"
                                 this1.computeCompiledReturnTypeName()
                              }
                           )
                           name match {
                              case "<clinit>" =>
                                 s"()"
                              case "<init>" =>
                                 s"(...args : $argsTypeRef )"
                              case _ =>
                                 s"(...args: $argsTypeRef ): $returnTypeStr"
                           }
                        })
                  }
               }: String) + ";"
            ) match {
               case s =>
                  s
                  // .prependedAll((
                  //    cbsq.meta.asm.jvmc.Modifier(access)
                  //    .toString()
                  //    .appendedAll(" ")
                  // ))
                  .prependedAll((
                     if cbsq.meta.asm.jvmc.Modifier(access).describesStaticMember then "static "
                     else ""
                  ))
            }
         })
         

}


























