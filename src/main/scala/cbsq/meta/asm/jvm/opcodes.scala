package cbsq.meta.asm.jvm
















protected 
val asmOpcodesConstsTable = {
   import scala.language.unsafeNulls
   import org.objectweb.asm
   import scala.jdk.CollectionConverters.*
   ({
      this.getClass()
      .getResource("/jvmsdocs/opcodes-table-asm.txt")
   }) match {
      case s0 =>
         import java.io.*
         val s = new StringWriter
         new InputStreamReader(s0.openStream() )
         .transferTo(s)
         s.toString()
   } match {
      case s =>
         "int\\s+(\\w+)\\s*\\=\\s*(\\d+)".r
         .findAllMatchIn(s)
         .map(s => (
            (s group 1, BigInt(s group 2).toInt )
         ))
         .toIndexedSeq
   }
}

protected 
val opcodeTable = {
   asmOpcodesConstsTable
   .filterNot((name, _) => {
      val AsmVerS = "ASM(.+?)".r
      val AccessFlagBitConstName = "ACC_(.+?)".r
      val MethodUnreflectTYpe = "H_(.+?)".r
      val FrameDeltaType = "F_(.+?)".r
      val FrameElementType = "(TOP|INTEGER|LONG|FLOAT|DOUBLE|NULL|UNITIALIZED_THIS)".r
      name match
         case AsmVerS(_) =>
            true
         case AccessFlagBitConstName(_) | FrameDeltaType(_) | FrameElementType(_*) | MethodUnreflectTYpe(_) =>
            true
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
   .appended((-1, "(opcode -1)") )
   .toMap
}

def getOpcodeDataTypeCanonicalName(what: String): String = {
                  import scala.language.unsafeNulls
                  what match {

                     case "" => 
                        "void"

                     case "I" => 
                        "i32"
                     case "L" => 
                        "i64"
                     case "F" => 
                        "f32"
                     case "D" => 
                        "f64"
                        
                     case "A" => 
                        "Object | Null"

                  }
}





















