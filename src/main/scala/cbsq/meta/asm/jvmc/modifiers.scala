package cbsq.meta.asm.jvmc













export cbsq.meta.asm.jvm.Modifier





export cbsq.meta.asm.jvm.MethodDescriptorImpl1

export cbsq.meta.asm.jvm.isSynthetic

extension (this1: MethodDescriptorImpl1) {

         //
         def computeCompiledArgsTypeName(): String = {
            import this1.{access, name, descriptor }
            val argsTypeRef = (
               name
               // .toUpperCase(java.util.Locale.ROOT)
               .prependedAll("$$Args$$")
               .appendedAll("$" + (name + descriptor).hashCode().&(~Int.MinValue) )
               .replaceAll("\\W", java.util.regex.Matcher.quoteReplacement("$") )
            )
            argsTypeRef
         }
         
         def toJsMethodDeclString(): String = ({
            import this1.{access, name, descriptor }
            import org.objectweb.asm.Opcodes
            val argsTypeRef = (
               this1.computeCompiledArgsTypeName()
            )
            (
               "" + ({
                  name match {
                     case s @ ("constructor" | "__proto__" | "prototype") => 
                        "[\"" + s + "\"]"
                     case "<init>" => 
                        "constructor"
                     case _: String => 
                        name
                  }
               }: String) + ({
                  descriptor match {
                     case _ =>
                        // descriptor
                        // "(...args ): undefined | null | {}"
                        ({
                           val returnTypeStr = (
                              if (name.matches("\\<init\\>|clone|equals|hashCode|toArray|to(?:Locale)?String|valueOf") ) then
                                 "unknown"
                              else {
                                 // "Promise<unknown>"
                                 "undefined | null | {}"
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

extension (e: MethodDescriptorImpl1) {

   def isEffectivelyPrivate(): Boolean = {
               (
                  !(e.visibility matches "protected|public")
                  || e.isSynthetic
                  || (e.name matches "\\<clinit\\>|finalize|registerNatives")
                  || (e.name matches "readResolve|writeReplace|(?:write|read)(Object|External)(NoData|)")
                  // || (e.name matches "notify(?:All|)|wait")
                  || ("lambda\\$|\\$anonfun|access\\$".r.findFirstIn(e.name).nonEmpty)
                  || ("\\$deserializeLambda\\$".r.findFirstIn(e.name).nonEmpty)
                  || ("(?i)initcomponents".r.findFirstIn(e.name).nonEmpty)
                  || ("\\$\\d+".r.findFirstIn(e.name).nonEmpty)
               )
   }
}













