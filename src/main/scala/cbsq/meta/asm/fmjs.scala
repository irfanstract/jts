package cbsq.meta.asm

















trait ERpk
{

   extension (name0: ow.Type) {
      
         /**
          * 
          * the simple-name
          * .
          * analogue to
          * `getSimpleName` in `j.l.Class`
          * 
          */
         def getSimpleName(): String

   }

   extension (name0: ow.Type) {
      
         /**
          * 
          * for `a/bpkg/c/d/EBar$FG`,
          * return as notation `a.bpkg.c.d.EBar$FG`
          * .
          * analogue to
          * `getCanonicalName` in `java.lang.Class`
          * 
          */
         def getCanonicalName(): String

   }

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

}

def fcv(o: java.io.PrintWriter) : ow.ClassVisitor = {
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
      
   }
   import eRpkImpl.{*}
   new org.objectweb.asm.ClassVisitor(ow.Opcodes.ASM9) {

      var strRepr: (java.io.PrintWriter => Unit) = (
         o => {
            o.println(s"export = class {} ; " )
         }
      )
      
      override
      def visitAttribute(attribute: org.objectweb.asm.Attribute): Unit = {
         println(attribute.toString() )
         println(attribute.`type` )
      }

      override
      def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
         ;
         val simpleName = (
            ow.Type.getObjectType(name)
            .getSimpleName()
         )
         val superclassSimpleName = (
            ow.Type.getObjectType(superName)
            .getSimpleName()
         )
         strRepr = (o: java.io.PrintWriter) => {
            o println("declare const rootPkg : typeof global ; ")
            o.println()
            o.println()
            o.println()
            o.println(s"declare class $simpleName extends ${ow.Type.getObjectType(superName).compileInlineLevelRef() } ")
            o.println("{")
            o.println()
            o.println(s"  constructor() ; ")
            o.println()
            o.println(s"  equals<That>(thatOne: That ): boolean ;")
            o.println(s"  hashCode(): string ;")
            o.println()
            o.println(s"//  toJSON(): null | {} ;")
            o.println(s"//  clone(): $simpleName ;")
            o.println()
            o.println(s"//  close(): void ;")
            o.println(s"//  [Symbol.asyncDispose](): Promise<void> ;")
            o.println(s"//  finalize(): void ;")
            o.println(s"//  [Symbol.synchronized]: Thread.SyncronizedBlockSupport ;")
            o.println()
            o.println(s"  toString(): string ;")
            o.println(s"  toLocaleString(): string ;")
            o.println(s"  get [Symbol.toStringTag](): string ;")
            o.println()
            o.println("}")
            o.println(s"export = $simpleName ;")
         }
      }
      
      override
      def visitEnd(): Unit = {
         strRepr(o)
      }
      
   }
}

trait Sgde
{
   val needsExportAssignment: Boolean
}

export cbsq.meta.esm.{ExportAllAssignmentKindEnum, ExportAllAssignmentKindEnumImpl }

@main
def fcvDemo101(): Unit = {
   val path = (
      // "src\main\resources\jbc-transform\samples\bytebuffers1$package$ByteBlob$.class"
      getClass()
      .getResource("/jbc-transform/samples/bytebuffers1$package$ByteBlob$.class")
   )
   val cr = (
      new org.objectweb.asm.ClassReader((
         path
         .openStream()
      ))
   )
   cr
   .accept((
      fcv((
         stdOutWriter(System.out)
         .asPrintWriter()
      ) )
   ) , org.objectweb.asm.ClassReader.SKIP_FRAMES )
}





















