


import cbsq.meta.asm.jsgen.*



QualifiedName.parse("cbsq")
QualifiedName.parse("cbsq.meta")
QualifiedName.parse("cbsq.meta.asm")
util.Try( {
   QualifiedName.parse("cbsq.meta.asm..jsgen")
   //
})
.failed.get
util.Try( {
   QualifiedName.parse("")
   //
})
QualifiedName.byElements(Seq() :+ "cbsq.meta.asm" :+ "cbsq.meta.asm" :+ "more" )
.pathElements
util.Try( {
   QualifiedName.byElements(Seq() :+ "" :+ "cbsq.meta.asm" :+ "more" )
   //
})
.failed.get

formatFormalTypeQuery((
   QualifiedName.parse("cbsq.meta.asm")
))
formatExactPathTypeQuery((
   QualifiedName.parse("cbsq.meta.asm")
))




