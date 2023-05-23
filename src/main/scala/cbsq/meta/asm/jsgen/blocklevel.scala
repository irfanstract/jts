package cbsq.meta.asm.jsgen
















export cbsq.meta.util.asIndentedPrintWriter

extension (o: java.io.PrintWriter) {

   def printIndentedBlockLevelComment(i: Int, c: String): Unit = {
      import language.unsafeNulls
      import scala.jdk.CollectionConverters.*
      for (l <- c.lines().iterator().asScala ) {
         o println ("".padTo(i, ' ') + l )
      }
   }

}




























