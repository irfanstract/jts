package cbsq.meta.asm














@annotation.experimental
def ??? = {
   throw NotImplementedError()
}

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

export cbsq.meta.util.PwEmitter












export cbsq.meta.asm.jvm.ow


















