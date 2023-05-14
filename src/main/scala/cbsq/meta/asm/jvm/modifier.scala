package cbsq.meta.asm.jvm















sealed
case class Modifier(value: Int) {

   import value as access

   val describesStaticMember: Boolean = {
      import org.objectweb.asm.Opcodes
      (access & Opcodes.ACC_STATIC) != 0
   }

   /**
    * 
    * the visibility modifier string
    * 
    */
   val visibilityKeyword: String = {
      import org.objectweb.asm.Opcodes
      (access & (Opcodes.ACC_PRIVATE | Opcodes.ACC_PROTECTED | Opcodes.ACC_PUBLIC) ) match {
         case Opcodes.ACC_PRIVATE =>
            "private"
         case 0 =>
            "package-private"
         case Opcodes.ACC_PROTECTED =>
            "protected"
         case Opcodes.ACC_PUBLIC =>
            "public"
      }
   }

   val describesFinal: Boolean = {
      import org.objectweb.asm.Opcodes
      (access & Opcodes.ACC_FINAL) != 0
   }

   val describesBridgeMethod: Boolean = {
      import org.objectweb.asm.Opcodes
      (access & Opcodes.ACC_BRIDGE) != 0
   }

   val describesSynthetic: Boolean = {
      import org.objectweb.asm.Opcodes
      (access & Opcodes.ACC_SYNTHETIC) != 0
   }

   override
   def toString(): String = {
      
                  import org.objectweb.asm.Opcodes
                  // ""
                  // .replaceFirst("\\A", (
                  //    if (0 != (Opcodes.ACC_STATIC & access) ) "static "
                  //    else ""
                  // ))
                  // .replaceFirst("\\A", (
                  //    if (0 != (Opcodes.ACC_PROTECTED & access) ) "protected "
                  //    else ""
                  // ))
                  // .replaceFirst("\\A", (
                  //    if (0 != (Opcodes.ACC_PRIVATE & access) ) "private "
                  //    else ""
                  // ))
                  // .replaceFirst("\\A", (
                  //    if (0 == (access) ) "package-private "
                  //    else ""
                  // ))
                  (
                     /** 
                      * 
                      * `static` shall precede the visibility-modifier, as
                      * `static` defines the def's prefix
                      * 
                      * `bridge` needs to be included, as
                      * `bridge`
                      * marks methods of-which direct/explict source-level calls-to shall be disallowed
                      * 
                      * `abstract` and `native` and `synchronized` and `strictfp` can be omitted -
                      * the four don't form signature at all
                      * 
                      */
                     Seq()
                     :+ (if (describesStaticMember) "static" else "") 
                     :+ visibilityKeyword 
                     :+ (if (describesBridgeMethod) "bridge" else "")
                     :+ (if (describesSynthetic) "synthetic" else "")
                     :+ (if (describesFinal) "final" else "")
                  )
                  .mkString(" ")
                  /** 
                   * 
                   * strip extraneous whitespace(s)
                   * 
                   */
                  .replaceAll("\\s+", " ").replaceAll("\\A\\s+|\\s+\\z", "")
   }

}














