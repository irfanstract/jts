
import language.unsafeNulls
import scala.jdk.CollectionConverters.*


import org.objectweb.asm


def fetchAndParse1(p: String) = {
   val c1 = {
      import asm.tree.ClassNode
      ClassNode(asm.Opcodes.ASM9 )
   }
   asm.ClassReader((
      getClass()
      .getResource(p)
      .openStream()
      .readAllBytes()
   ))
   .accept(c1, (
      0 
      | asm.ClassReader.EXPAND_FRAMES
   ) )
   c1
}

extension (e: asm.tree.MethodNode) {

   def toCodeString(): String = {
      (e.name, {
         e.instructions
         .asScala
         .zipWithIndex.map(_.swap)
      } )
      .toString()
   }

}


val c1 = {
   // getClass().getResource("/jbc-transform/samples/IOMR$MarkableInputStreamImpl.class")
   fetchAndParse1("/jbc-transform/samples/IOMR$MarkableInputStreamImpl.class")
}

({
   c1.methods
   .asScala
   .map(e => e.toCodeString() )
   .map(v => ("" + v + "\n") )
})




