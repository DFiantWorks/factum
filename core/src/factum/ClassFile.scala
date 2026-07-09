package factum

import java.io.{ByteArrayInputStream, DataInputStream}

/** Minimal JVM class-file reader: extracts the classes referenced from the constant pool. The
  * constant-pool format is stable across class-file versions; only new constant tags are ever added
  * (all tags through Java 21 are handled here).
  */
private[factum] object ClassFile:
  /** Internal names (slash-separated) of all classes referenced by this class file, with array
    * descriptors unwrapped to their element class and primitive arrays dropped. Includes the
    * superclass, interfaces, and every field/method owner, since they all appear as CONSTANT_Class
    * entries.
    */
  def referencedClasses(bytes: Array[Byte]): Set[String] =
    val in = DataInputStream(ByteArrayInputStream(bytes))
    require(in.readInt() == 0xcafebabe, "not a class file")
    in.skipNBytes(4) // minor + major version
    val cpCount = in.readUnsignedShort()
    val utf8 = collection.mutable.Map.empty[Int, String]
    val classNameIndices = collection.mutable.ListBuffer.empty[Int]
    var i = 1
    while i < cpCount do
      val tag = in.readUnsignedByte()
      tag match
        case 1     => utf8(i) = in.readUTF() // Utf8
        case 3 | 4 => in.skipNBytes(4) // Integer, Float
        case 5 | 6 => // Long, Double take two constant-pool slots
          in.skipNBytes(8)
          i += 1
        case 7                          => classNameIndices += in.readUnsignedShort() // Class
        case 8 | 16 | 19 | 20           => in.skipNBytes(2) // String, MethodType, Module, Package
        case 9 | 10 | 11 | 12 | 17 | 18 =>
          in.skipNBytes(4) // Field/Method/InterfaceMethodref, NameAndType, (Invoke)Dynamic
        case 15    => in.skipNBytes(3) // MethodHandle
        case other => throw IllegalArgumentException(s"unknown constant pool tag: $other")
      i += 1
    end while
    classNameIndices.iterator.flatMap(utf8.get).flatMap(unwrapArray).toSet
  end referencedClasses

  private def unwrapArray(internalName: String): Option[String] =
    if internalName.startsWith("[") then
      val element = internalName.dropWhile(_ == '[')
      if element.startsWith("L") && element.endsWith(";") then
        Some(element.drop(1).dropRight(1))
      else None // primitive array
    else Some(internalName)
end ClassFile
