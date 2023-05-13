package cbsq.meta.asm














object utilityImplicits {
   
   extension (o: java.io.Writer) {

      def asPrintWriter() : java.io.PrintWriter = {
         new java.io.PrintWriter(o, true)
      }

   }

}

export utilityImplicits._

@deprecated
def stdOutWriter(o: java.io.OutputStream) : java.io.Writer = {
   new java.io.OutputStreamWriter(o)
}










object ow {

   // export org.objectweb.asm.{* }
   
   object Opcodes {
      export org.objectweb.asm.Opcodes.*
   }
   
   export org.objectweb.asm.ModuleVisitor
   export org.objectweb.asm.ClassVisitor
   export org.objectweb.asm.ClassReader
   export org.objectweb.asm.ClassWriter
   export org.objectweb.asm.AnnotationVisitor
   export org.objectweb.asm.MethodVisitor
   
   export org.objectweb.asm.Attribute
   export org.objectweb.asm.ConstantDynamic
   type Handle = org.objectweb.asm.Handle
   object Handle {
      export org.objectweb.asm.Handle.*
   }
   type Type = org.objectweb.asm.Type
   object Type {
      export org.objectweb.asm.Type.*
   }

}


















