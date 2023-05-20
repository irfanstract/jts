package cbsq.meta.asm.jsgen















opaque type QualifiedName
   = QualifiedNameImpl

extension (nm: QualifiedName) {

   def pathElements: IndexedSeq[String] = {
      nm.elements
   }

}

object QualifiedName {

   def parse(v: String) : QualifiedName = {
      import language.unsafeNulls
      QualifiedNameImpl((
         v
         .split("\\.").toIndexedSeq
         .tapEach(vElement => (
            require((vElement: String) != "" , s"empty-element in path '$v'" )
         ))
      ))
   }

   def byElements(v0: Seq[String]) : QualifiedName = {
      import language.unsafeNulls
      QualifiedNameImpl((
         v0
         .flatMap(s => (
            parse(s)
            .elements
         ))
         .toIndexedSeq
      ))
   }
   
}

sealed
case class QualifiedNameImpl(elements: IndexedSeq[String] ) {

   override
   def toString(): String = {
      elements.mkString(".")
   }

}


















