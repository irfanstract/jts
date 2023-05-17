package cbsq.meta.asm.jvm
















protected 
val asmOpcodesConstsTable = {
   import scala.language.unsafeNulls
   import org.objectweb.asm
   import scala.jdk.CollectionConverters.*
   classOf[asm.Opcodes]
   .getFields()
   .toIndexedSeq
   .filter(m => {
      import java.lang.reflect.Modifier
      val modifiers = m.getModifiers()
      Modifier.isStatic(modifiers) && Modifier.isPublic(modifiers ) && Modifier.isFinal(modifiers )
   })
   .map(c => (c.getName(), c.get(null).asInstanceOf[java.lang.Number].intValue() ) )
}

protected 
val opcodeTable = {
   asmOpcodesConstsTable
   .filterNot((name, _) => {
      val AsmVerS = "ASM(.+?)".r
      val MethodUnreflectTYpe = "H_(.+?)".r
      name match
         case AsmVerS(_) | MethodUnreflectTYpe(_) => true
         case _ => false
      
   })
   .sortBy((name, _) => ({
      import java.util.Locale
      val MethodInsn = "INVOKE(.+?)".r
      val Ldc = "LDC".r
      val DConst = "\\wCONST(.*?)".r
      name match
         case "NOP" | "NOOP" => -22005
         case "INVOKEDYNAMIC" => -20010
         case MethodInsn(_*)  => -20000
         case Ldc(_*)      => -19998
         case DConst(_*)   => -19996
         case _ => 10000
      
   }))
}

val opcodeNameTable = {
   opcodeTable
   .map(_.swap)
   .toMap
}





















