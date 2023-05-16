package cbsq.meta.asm.jsgen

















def formatFormalTypeQuery(nm: QualifiedName): String = {
   s"typeof $nm"
}

def formatExactPathTypeQuery(nm: QualifiedName): String = {
   // s"valueof $nm" /* looking forward to write a proposal for this :D */
   s"typeof $nm"
}
















