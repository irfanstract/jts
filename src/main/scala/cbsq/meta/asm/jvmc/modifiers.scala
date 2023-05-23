package cbsq.meta.asm.jvmc













export cbsq.meta.asm.jvm.Modifier





export cbsq.meta.asm.jvm.MethodDescriptorImpl1

export cbsq.meta.asm.jvm.isSynthetic

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

extension (code: org.objectweb.asm.tree.MethodNode) {

   def isEffectivelyNonAbstract(): Boolean = {
            import scala.language.unsafeNulls
            import org.objectweb.asm
            import asm.Opcodes
            (
               code != null
               && (
                  (code.access & (Opcodes.ACC_NATIVE | Opcodes.ACC_ABSTRACT)) == 0 
               )
               && (
                  0 < (code.nn : asm.tree.MethodNode).instructions.size()
               )
            )
   }
   
}














