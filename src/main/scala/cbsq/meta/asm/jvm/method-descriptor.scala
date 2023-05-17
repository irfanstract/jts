package cbsq.meta.asm.jvm
















case class MethodDescriptorImpl1(access: Int, name: String, descriptor: String) 
{

   /**
    * 
    * the visibility modifier string
    * 
    */
   val visibility: String = {
      Modifier(access)
      .visibilityKeyword
   }

   /**
    * 
    * `toNameAndParamsReturnsString`
    * 
    * in JVM
    * *the return-type* form part of *the name*,
    * hence including it
    * 
    */
   def toNameAndParamsReturnsString(
      simplify: Boolean = true ,
      
   ): String = {
      val descriptorFmatted = (
         org.objectweb.asm.Type.getType(descriptor) match {

            case d =>
               import MethodDescriptorImpl1.SIMPLENAME
               val simplifiedIfDesired = (
                  if (simplify) SIMPLENAME
                  else (identity[String] _)
               )
               ""
               .++("(")
               .++((
                  d.getArgumentTypes().toIndexedSeq
                  .map(_.getDescriptor())
                  .map(simplifiedIfDesired)
                  .map(_ + ",")
                  .mkString("")
               ) : String)
               .++(")")
               .++(": ")
               .++(simplifiedIfDesired(d.getReturnType().toString() ) : String )
               
         }
      )
      s"$name$descriptorFmatted"
   }

   def toMultilineString(): String = {
      toNameAndParamsReturnsString()
      .prependedAll((
         Modifier(access)
         .toString()
      ) + "\n")
   }

   // override
   def toShortString(): String = {
      /**
       * 
       * for brevity,
       * only (1) `static` will be retained (as `static` defines the def's prefix!) and (2) `bridge` and
       * other modifiers will not be included
       * 
       */
      toNameAndParamsReturnsString()
      .prependedAll((
         if Modifier(access).toString().contains("static") then "static "
         else ""
      ))
      .replaceFirst("(\\w+)\\s*(?=\\()", "$1 " + (
         if Modifier(access).toString().contains("bridge") then "bridge "
         else ""
      ))
   }

   override
   def toString(): String = {
      toShortString()
      .++(" --")
      .appendedAll(" " + (
         Modifier(access)
         .toString()
         .replaceAll("\\bstatic\\b", "")
      ).prependedAll("(").appendedAll(")").replaceFirst("\\(\\)", "") )
   }

}

extension [This <: MethodDescriptorImpl1](this1: This) {

   def isSynthetic: Boolean = {
      import org.objectweb.asm.Opcodes
      (this1.access & Opcodes.ACC_SYNTHETIC) != 0
   }

}

object MethodDescriptorImpl1
{

   //
   
   val SIMPLENAME = (
      ({
            case v if (v.length() == 1 || v.matches("\\[+.")) =>
               java.lang.invoke.MethodType.fromMethodDescriptorString(s"()$v", null)
               .returnType()
               .getSimpleName()
            case v =>
               v
      } : (String => String))
      .andThen(t => (
         t
         .replaceFirst(";?\\z", "")
      ) )
      .andThen(t => (
         t
         .replaceFirst("(\\[*L)", "$1" + java.util.regex.Matcher.quoteReplacement("/") )
      ) )
      .andThen(t => (
         t
         .replaceAll("(L)\\/", "$1:")
         .replaceAll("\\/", ".")
      ) )
   )
   
}




















