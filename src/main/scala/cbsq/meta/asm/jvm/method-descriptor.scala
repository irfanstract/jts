package cbsq.meta.asm.jvm
















/**
 * 
 * quick info about its *mod-bits*, *name*, *params*, *return-type*
 * 
 *
 * @param access
 * @param name
 * @param descriptor
 * @param signature - if not available then can be same as `descriptor`
 * 
 */
case class MethodDescriptorImpl1(
   access: Int,
   name: String, 
   descriptor0: MethodDescriptorImpl1.Bds,
) 
{
   
   export descriptor0.{descriptor, signature }

   /**
    * without the build flag `-Yexplicit-nulls`
    * one can pass `null` in place of concrete `String` --
    * i don't want it ☹
    */
   descriptor.nn
   signature.nn

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
      
   )(using MethodDescriptorImpl1.Fmtct): String = {
      val sgnAnalysis = {
         Esig(signature)
         .analyse()
      }
      extension (sig0 : Esig) {
         def erasedIfAskedTo: Esig = {
            if (summon[MethodDescriptorImpl1.Fmtct ].generics) sig0
            else sig0.asErased()
         }
      }
      // import sgnAnalysis.{pt0, ptypes0, rt0}
      val descriptorFmatted = {
               ;
               import MethodDescriptorImpl1.SIMPLENAME
               val simplifiedIfDesired = (
                  if (simplify) SIMPLENAME
                  else (identity[String] _)
               )
               ""
               .++("(")
               .++((
                  (sgnAnalysis.ptypes0).toIndexedSeq
                  .map(_.erasedIfAskedTo)
                  .map((_: Esig).value)
                  .map(simplifiedIfDesired)
                  .map(_ + ",")
                  .mkString("")
               ) : String)
               .++(")")
               .++(": ")
               .++(simplifiedIfDesired((sgnAnalysis.rt0.erasedIfAskedTo.value).toString() ) : String )
      }
      s"$name$descriptorFmatted"
   }

   def toMultilineString()(using MethodDescriptorImpl1.Fmtct): String = {
      toNameAndParamsReturnsString()
      .prependedAll((
         Modifier(access)
         .toString()
      ) + "\n")
   }

   // override
   def toShortString()(using MethodDescriptorImpl1.Fmtct): String = {
      import language.unsafeNulls
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
      import language.unsafeNulls
      toShortString()(using MethodDescriptorImpl1.Fmtct(generics = false ) )
      .++(" --")
      .appendedAll(" " + (
         Modifier(access)
         .toString()
         .replaceAll("\\bstatic\\b", "")
      ).prependedAll("(").appendedAll(")").replaceFirst("\\(\\)", "") )
   }

}

export MethodDescriptorImpl1.isSynthetic

object MethodDescriptorImpl1
{

   extension [This <: MethodDescriptorImpl1](this1: This) {

      def isSynthetic: Boolean = {
         import org.objectweb.asm.Opcodes
         (this1.access & Opcodes.ACC_SYNTHETIC) != 0
      }

   }

   //
   
   sealed 
   case class Bds private[MethodDescriptorImpl1] (
      val descriptor : String ,
      val signature  : String ,
   )
   object Bds {

      def apply(
         descriptor : String ,
         signature0 : Null | String ,
      ) : Bds = {
         new Bds(
            descriptor = descriptor , 
            signature = if (signature0 != null) signature0 else descriptor ,
         )
      }

   }
   
   val SIMPLENAME = ({
      import language.unsafeNulls
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
         .replaceAll("(\\[*L(?!azy|at|ine|ind|ink|int|ist|iter|itr|ock|one|ove|ut))", "$1" + java.util.regex.Matcher.quoteReplacement("/") )
      ) )
      .andThen(t => (
         t
         .replaceAll("(L)\\/", "$1:")
         .replaceAll("\\/", ".")
      ) )
   })

   // opaque type Egn <: Boolean = Boolean
   // given [T <: Boolean : ValueOf] : (Egn & T) = valueOf[T]
   case class Fmtct(generics: Boolean )
   
}




















