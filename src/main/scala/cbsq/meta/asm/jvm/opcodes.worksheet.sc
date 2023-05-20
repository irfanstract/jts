




import cbsq.meta.asm.jvm.opcodeNameTable

this.getClass()

val p = ({
   import language.unsafeNulls
   this.getClass()
   .getResource("/jvmsdocs/opcodes-table-asm.txt")
   .toURI()
})

// ({
//    import language.unsafeNulls
//    import java.nio.file.*
//    FileSystems.getFileSystem(p)
//    // .provider()
// })

opcodeNameTable



