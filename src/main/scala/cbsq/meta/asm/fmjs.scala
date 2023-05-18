package cbsq.meta.asm



















def fcv(o: java.io.PrintWriter) : ow.ClassVisitor = {
   import org.objectweb.asm
   new asm.ClassVisitor(asm.Opcodes.ASM9) {

      var s : Wsn = (
         wsnImpl( )
      )
      
      override
      def visitAttribute(attribute: asm.Attribute): Unit = {
         println(attribute.toString() )
         println(attribute.`type` )
      }

      override
      def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
         ;
         import asm.Type
         s = ({
            import language.unsafeNulls
            s.withNewFullName(
               ownName             = Type.getObjectType(name) ,
               superclassName      = Type.getObjectType(superName) ,
               superInterfaces     = interfaces.toIndexedSeq.map(Type.getObjectType _ ) ,
            )
         })
      }
      
      override
      def visitMethod(access: Int, name: String, descriptor: String, signature: Null | String, exceptions: Array[String]): asm.MethodVisitor = {
         ;
         val cmv = (
            new asm.tree.MethodNode()
         )
         def onVisitEnd() : Unit = {
            s = ((s: Wsn) => (
               s
               .withAddedMethodByNativeSig((
                  s
                  .translateIntoNativeSig(({
                     import cbsq.meta.asm.jvm.MethodDescriptorImpl1
                     MethodDescriptorImpl1(
                        access = access ,
                        name = name ,
                        descriptor0 = (
                           MethodDescriptorImpl1.Bds(descriptor, signature)
                        ) ,
                     )
                  }))
               ) , mv => cmv.accept(mv) )
            ))(s)
            
         }
         new asm.MethodVisitor(asm.Opcodes.ASM9, cmv ) {

            override
            def visitEnd(): Unit = {
               super.visitEnd()
               onVisitEnd()
            }
            
         }
      }

      override
      def visitEnd(): Unit = {
         s.distilledFormPwEmitter
         .printTo(o)
      }
      
   }
}

@main
def fcvDemo101(): Unit = {
   val path = (
      // "src\main\resources\jbc-transform\samples\bytebuffers1$package$ByteBlob$.class"
      // getClass()
      // .getResource("/jbc-transform/samples/bytebuffers1$package$ByteBlob$.class")
      getClass()
      // .getResource("/jbc-transform/samples/byteManipImplicits$.class")
      // .getResource("/jbc-transform/samples/bytebuffers1$package$ByteBlob$.class").nn
      .getResource("/jbc-transform/samples/IOMR$MarkableInputStreamImpl.class").nn
   )
   val cr = (
      new org.objectweb.asm.ClassReader((
         path
         .openStream().nn
      ))
   )
   import cbsq.meta.asm.jvmc.amevMonadificativeImpl.defaultTsConfig
   //
   cr
   .accept((
      fcv((
         stdOutWriter(System.out.nn)
         .asPrintWriter()
      ) )
      .withJsSpecificMethods()
      // .asMakingAsyncifiedVariants()
      // .asMakingTupledVariants()
   ) , org.objectweb.asm.ClassReader.SKIP_FRAMES )
}





















