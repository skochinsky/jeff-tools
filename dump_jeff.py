# Intel ME DAL module dumper
# Copyright (c) 2013 Igor Skochinsky
# Version 0.1 2015-12-24
#
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
#    1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
#
#    2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
#
#    3. This notice may not be removed or altered from any source
#    distribution.


import ctypes
import struct
import sys
import os
import array

uint8_t  = ctypes.c_ubyte
char     = ctypes.c_char
uint32_t = ctypes.c_uint
uint64_t = ctypes.c_uint64
uint16_t = ctypes.c_ushort

def replace_bad(value, deletechars):
    for c in deletechars:
        value = value.replace(c,'_')
    return value

def read_struct(li, struct):
    s = struct()
    slen = ctypes.sizeof(s)
    bytes = li.read(slen)
    fit = min(len(bytes), slen)
    ctypes.memmove(ctypes.addressof(s), bytes, fit)
    return s

def get_struct(str_, off, struct):
    s = struct()
    slen = ctypes.sizeof(s)
    bytes = str_[off:off+slen]
    fit = min(len(bytes), slen)
    ctypes.memmove(ctypes.addressof(s), bytes, fit)
    return s

def DwordAt(f, off):
    return struct.unpack("<I", f[off:off+4])[0]


TU1 = uint8_t
TU2 = uint16_t
TU4 = uint32_t

VMACCESS  = uint16_t # Access Flag                      16-bit vector
VMTYPE    = uint8_t  # Type descriptor                  8-bit vector
VMNCELL   = uint16_t # Index in an array of U4 values   16-bit unsigned integer
VMOFFSET  = uint16_t # Memory offset                    16-bit unsigned integer (offset in bytes from the beginning of the class header)
VMDOFFSET = uint32_t # Memory offset                    32-bit unsigned integer (offset in bytes from the beginning of the file header)
VMCINDEX  = uint16_t # Class Index                      16-bit unsigned integer
VMPINDEX  = uint16_t # Package Index                    16-bit unsigned integer
VMMINDEX  = uint32_t # Method Index                     32-bit unsigned integer
VMFINDEX  = uint32_t # Field Index                      32-bit unsigned integer

# VMACCESS flags           # class field method
ACC_PUBLIC       = 0x0001  #   +     +     +     Is public; may be accessed from outside of its package.
ACC_PRIVATE      = 0x0002  #         +     +     Is private; usable only within the defined class.
ACC_PROTECTED    = 0x0004  #         +     +     Is protected; may be accessed within subclasses.
ACC_STATIC       = 0x0008  #         +     +     Is static.
ACC_FINAL        = 0x0010  #   +     +     +     Is final; no subclasses allowed/no further overriding or assignment after initialization/no overriding is allowed.
ACC_SUPER        = 0x0020  #   +                 Modify the behavior of the jeff_invokespecial bytecodes included in the bytecode area list of this class.
ACC_SYNCHRONIZED = 0x0020  #               +     Is synchronized; wrap use in monitor lock.
ACC_VOLATILE     = 0x0040  #         +           Is volatile; cannot be cached.
ACC_TRANSIENT    = 0x0080  #         +           Is transient; not written or read by a persistent object manager.
ACC_NATIVE       = 0x0100  #               +     Is native; implemented in a language other than the source language.
ACC_INTERFACE    = 0x0200  #   +                 Is an interface.
ACC_ABSTRACT     = 0x0400  #   +           +     Is abstract; may not be instantiated/no implementation is provided.
ACC_STRICT       = 0x0800  #               +     The VM is required to perform strict floating-point operations.

def acc2flags(acc, ismethod = False):
    r = []
    if acc & ACC_PUBLIC:
      r.append("ACC_PUBLIC")
    if acc & ACC_PRIVATE:
      r.append("ACC_PRIVATE")
    if acc & ACC_PROTECTED:
      r.append("ACC_PROTECTED")
    if acc & ACC_STATIC:
      r.append("ACC_STATIC")
    if acc & ACC_FINAL:
      r.append("ACC_FINAL")
    if acc & ACC_SUPER:
      if ismethod:
        r.append("ACC_SYNCHRONIZED")
      else:
        r.append("ACC_SUPER")
    if acc & ACC_VOLATILE:
      r.append("ACC_VOLATILE")
    if acc & ACC_TRANSIENT:
      r.append("ACC_TRANSIENT")
    if acc & ACC_NATIVE:
      r.append("ACC_NATIVE")
    if acc & ACC_INTERFACE:
      r.append("ACC_INTERFACE")
    if acc & ACC_ABSTRACT:
      r.append("ACC_ABSTRACT")
    if acc & ACC_STRICT:
      r.append("ACC_STRICT")
    return r

def acc2flagsj(acc, ismethod = False):
    r = []
    if acc & ACC_PUBLIC:
      r.append("public")
    if acc & ACC_PRIVATE:
      r.append("private")
    if acc & ACC_PROTECTED:
      r.append("protected")
    if acc & ACC_STATIC:
      r.append("static")
    if acc & ACC_FINAL:
      r.append("final")
    if acc & ACC_SUPER:
      if ismethod:
        r.append("synchronized")
      else:
        r.append("super")
    if acc & ACC_VOLATILE:
      if ismethod:
        r.append("bridge")
      else:
        r.append("volatile")
    if acc & ACC_TRANSIENT:
      r.append("transient")
    if acc & ACC_NATIVE:
      r.append("native")
    if acc & ACC_INTERFACE:
      r.append("interface")
    if acc & ACC_ABSTRACT:
      r.append("abstract")
    if acc & ACC_STRICT:
      r.append("strict")
    return r

# VMTYPE byte
# 0000 | XX | YY |
# size: 1<<YY
VM_TYPE_VOID     = 0x00 # Used for the return type of a method
VM_TYPE_SHORT    = 0x01
VM_TYPE_INT      = 0x02
VM_TYPE_LONG     = 0x03
VM_TYPE_BYTE     = 0x04
VM_TYPE_CHAR     = 0x05
VM_TYPE_FLOAT    = 0x06
VM_TYPE_DOUBLE   = 0x07
VM_TYPE_BOOLEAN  = 0x08
VM_TYPE_OBJECT   = 0x0A

# flags
VM_TYPE_TWO_CELL = 0x10 # for a type using two virtual machine cells (this flag is not set for an array)
VM_TYPE_REF      = 0x20 # for an object or an array
VM_TYPE_MONO     = 0x40 # for a mono-dimensional array
VM_TYPE_MULTI    = 0x80 # for an n-dimensional array, where n >= 2

BASIC_TYPES = [
  "void",      # 0x00 0 or absent absent
  "short",     # 0x01 0 or absent absent
  "int",       # 0x02 0 or absent absent
  "long",      # 0x03 0 or absent absent
  "byte",      # 0x04 0 or absent absent
  "char",      # 0x05 0 or absent absent
  "float",     # 0x06 0 or absent absent
  "double",    # 0x07 0 or absent absent
  "boolean",   # 0x08 0 or absent absent
  "<bad 0x9>", # 0x09
  "reference", # 0x0A
  "<bad 0xB>", # 0x0B
  "<bad 0xC>", # 0x0C
  "<bad 0xD>", # 0x0D
  "<bad 0xE>", # 0x0E
  "<bad 0xF>", # 0x0F
]

BASIC_TYPESJ = [
  "V",         # 0x00 0 or absent absent
  "S",         # 0x01 0 or absent absent
  "I",         # 0x02 0 or absent absent
  "J",         # 0x03 0 or absent absent
  "B",         # 0x04 0 or absent absent
  "C",         # 0x05 0 or absent absent
  "F",         # 0x06 0 or absent absent
  "D",         # 0x07 0 or absent absent
  "Z",         # 0x08 0 or absent absent
  "<bad 0x9>", # 0x09
  "<reference>", # 0x0A
  "<bad 0xB>", # 0x0B
  "<bad 0xC>", # 0x0C
  "<bad 0xD>", # 0x0D
  "<bad 0xE>", # 0x0E
  "<bad 0xF>", # 0x0F
]

def type2str(typ, dim=0, showflags = True):
    nm = BASIC_TYPES[typ & 0xF]
    if dim != 0:
        nm += "[]" * dim
    if typ & VM_TYPE_TWO_CELL:
        nm += " VM_TYPE_TWO_CELL"
    if typ & VM_TYPE_REF:
        nm += " VM_TYPE_REF"
    if typ & VM_TYPE_MONO:
        nm += " VM_TYPE_MONO"
    if typ & VM_TYPE_MULTI:
        nm += " VM_TYPE_MULTI"
    return nm

"""
struct VMFileHeader
{
 TU1 nMagicWord1;
 TU1 nMagicWord2;
 TU1 nMagicWord3;
 TU1 nMagicWord4;
 TU1 nFormatVersionMajor;
 TU1 nFormatVersionMinor;
 TU1 nByteOrder;
 TU1 nOptions;
 TU4 nFileLength;
 TU2 nFileVersion;
 TU2 nTotalPackageCount;
 TU2 nInternalClassCount;
 TU2 nTotalClassCount;
 TU4 nTotalFieldCount;
 TU4 nTotalMethodCount;
 VMDOFFSET dofAttributeSection;
 VMDOFFSET dofSymbolicData;
 VMDOFFSET dofConstantDataPool;
 VMDOFFSET dofFileSignature;
 VMDOFFSET dofClassHeader[];
};
"""

VM_ORDER_INT_BIG      = 0x01 # If this flag is set, integer values are stored using the big-endian convention.
                             # Otherwise, they are stored using the little-endian convention.
VM_ORDER_INT_64_INV   = 0x02 # If this flag is set, the two 32-bit parts of the 64-bit integer values are inverted.
VM_ORDER_FLOAT_BIG    = 0x04 # If this flag is set, JFLOAT and JDOUBLE values are stored using the big-endian convention.
                             # Otherwise, they are stored using the little-endian convention.
VM_ORDER_FLOAT_64_INV = 0x08 # If this flag is set, the two 32-bit parts of the JDOUBLE values are inverted.


VM_USE_LONG_TYPE      = 0x01 # One of the classes uses the "long" type (in the fields types, the methods signatures, the constant values or the bytecode instructions).
VM_USE_UCS_BMP        = 0x02 # All the characters encoded in the strings of this file are in the "Basic Multilingual Plane",
                             # therefore their encoding is in the range U+0000 to U+FFFF included.
VM_USE_FLOAT_TYPE     = 0x04 # One of the classes uses the "float" type and/or the "double" type
                             # (in the fields types, the methods signatures, the constant values or the bytecode instructions).
VM_USE_STRICT_FLOAT   = 0x08 # One of the classes contains bytecodes with strict floating-point computation (the "strictfp" keyword is used in the source file).
VM_USE_NATIVE_METHOD  = 0x10 # One of the classes contains native methods.
VM_USE_FINALIZER      = 0x20 # One of the classes has an instance finalizer or a class finalizer.
VM_USE_MONITOR        = 0x40 # One of the classes uses the flag ACC_SYNCHRONIZED or the bytecodes jeff_monitorenter or jeff_monitorexit in one of its methods.

class VMFileHeaderStart(ctypes.LittleEndianStructure):
    _fields_ = [
        ("nMagicWord1",         TU1),       # 'J'
        ("nMagicWord2",         TU1),       # 'E'
        ("nMagicWord3",         TU1),       # 'F'
        ("nMagicWord4",         TU1),       # 'F'
        ("nFormatVersionMajor", TU1),       # 1
        ("nFormatVersionMinor", TU1),       # 0
        ("nByteOrder",          TU1),       # VM_ORDER_xxx
        ("nOptions",            TU1),       # VM_USE_xxx
    ]

vmnheader_fields_endian = [
        ("nFileLength",         TU4),       # Size in bytes of the file (all elements included).
        ("nFileVersion",        TU2),       # Version number of the file itself (major<<8 | minor)
        ("nTotalPackageCount",  TU2),       # The total number of unique packages referenced in the file
                                            # (for the internal classes and the external classes).
        ("nInternalClassCount", TU2),       # The number of classes in the file (internal classes).
        ("nTotalClassCount",    TU2),       # The total number of the classes referenced in the file (internal classes and external classes).
        ("nTotalFieldCount",    TU4),       # The total number of field symbolic names used in the file.
        ("nTotalMethodCount",   TU4),       # The total number of method symbolic names used in the file.
        ("dofAttributeSection", VMDOFFSET), # Offset of the Optional Attribute Section (0 if absent)
        ("dofSymbolicData",     VMDOFFSET), # Offset of the symbolic data section (0 if absent)
        ("dofConstantDataPool", VMDOFFSET), # Offset of the constant data pool (0 if absent)
        ("dofFileSignature",    VMDOFFSET), # Offset of the file signature (0 if absent)
        # ("dofClassHeader",      VMDOFFSET[] # Offsets of the VMClassHeader structures for all internal classes
]

class VMFileHeaderRestLE(ctypes.LittleEndianStructure):
    _fields_ = vmnheader_fields_endian

class VMFileHeaderRestBE(ctypes.BigEndianStructure):
    _fields_ = vmnheader_fields_endian

def print_hdr(h1, h2, dofClassHeader):
    print "File magic:        JEFF"
    print "Format version:    %d.%d" % (h1.nFormatVersionMajor, h1.nFormatVersionMinor)
    print "Byte order:        0x%02X" % (h1.nByteOrder)
    if h1.nByteOrder & VM_ORDER_INT_BIG:
        print "                     VM_ORDER_INT_BIG"
    if h1.nByteOrder & VM_ORDER_INT_64_INV:
        print "                     VM_ORDER_INT_64_INV"
    if h1.nByteOrder & VM_ORDER_FLOAT_BIG:
        print "                     VM_ORDER_FLOAT_BIG"
    if h1.nByteOrder & VM_ORDER_FLOAT_64_INV:
        print "                     VM_ORDER_FLOAT_64_INV"

    print "Options:           0x%02X" % (h1.nOptions)
    if h1.nOptions & VM_ORDER_FLOAT_64_INV:
        print "                     VM_USE_LONG_TYPE"
    if h1.nOptions & VM_USE_UCS_BMP:
        print "                     VM_USE_UCS_BMP"
    if h1.nOptions & VM_USE_FLOAT_TYPE:
        print "                     VM_USE_FLOAT_TYPE"
    if h1.nOptions & VM_USE_STRICT_FLOAT:
        print "                     VM_USE_STRICT_FLOAT"
    if h1.nOptions & VM_USE_NATIVE_METHOD:
        print "                     VM_USE_NATIVE_METHOD"
    if h1.nOptions & VM_USE_FINALIZER:
        print "                     VM_USE_FINALIZER"
    if h1.nOptions & VM_USE_MONITOR:
        print "                     VM_USE_MONITOR"

    print "File length:       0x%02X" % (h2.nFileLength)
    print "File version:      %d.%d"  % (h2.nFileVersion>>8, h2.nFileVersion&0xFF)
    print "Total packages:    %d"     % (h2.nTotalPackageCount)
    print "Internal classes:  %d"     % (h2.nInternalClassCount)
    print "Total classes:     %d"     % (h2.nTotalClassCount)
    print "Total fields:      %d"     % (h2.nTotalFieldCount)
    print "Total methods:     %d"     % (h2.nTotalMethodCount)
    print "Attribute section: 0x%04X" % (h2.dofAttributeSection)
    print "Symbolic data:     0x%04X" % (h2.dofSymbolicData)
    print "Const data pool:   0x%04X" % (h2.dofConstantDataPool)
    print "File signature:    0x%04X" % (h2.dofFileSignature)
    print "Class headers:",
    for i in range(len(dofClassHeader)):
        if i % 8 == 0:
            print ""
        print "0x%04X " % dofClassHeader[i],
    print ""

class JeffFile:
    def get_struct(self, s):
        r = get_struct(self.f, self.offset, s)
        self.offset += ctypes.sizeof(r)
        return r

    def tell(self):
        return self.offset

    def seek(self, off):
        self.offset = off

    def skip(self, cnt):
        self.offset += cnt

    def align(self, sz):
        mask = sz-1
        if self.offset & mask:
            self.offset = (self.offset+mask) & ~mask

    def _read_one(self, aligned, fmt):
        if self.bigendian:
            fmt = ">" + fmt
        else:
            fmt = "<" + fmt
        sz = struct.calcsize(fmt)
        if aligned:
            self.align(sz)
        r = struct.unpack(fmt, self.f[self.offset:self.offset+sz])[0]
        self.offset += sz
        return r

    def read_8(self, aligned = False):
        return self._read_one(aligned, 'B')

    def read_8s(self, aligned = False):
        return self._read_one(aligned, 'b')

    def read_16(self, aligned = False):
        return self._read_one(aligned, 'H')

    def read_16s(self, aligned = False):
        return self._read_one(aligned, 'h')

    def read_32(self, aligned = False):
        return self._read_one(aligned, 'I')

    def read_32s(self, aligned = False):
        return self._read_one(aligned, 'i')

    def read_32f(self, aligned = False):
        return self._read_one(aligned, 'f')

    def read_64(self, aligned = False):
        return self._read_one(aligned, 'Q')

    def read_64s(self, aligned = False):
        return self._read_one(aligned, 'q')

    def read_64d(self, aligned = False):
        return self._read_one(aligned, 'd')

    def read_descriptor(self, jclass):
        return VMDescriptor(self, jclass)

    def read_bytes(self, count):
        r = self.f[self.offset:self.offset+count]
        self.offset += count
        return r

    def read_string(self):
        nStringLength = self.read_16()
        return self.read_bytes(nStringLength)

    def _get_helper(self, fn, offs):
        saved = self.offset
        self.offset = offs
        r = fn()
        self.offset = saved
        return r

    def get_string(self, offs):
        return self._get_helper(self.read_string, offs)

    def get_8(self, offs):
        return self._get_helper(self.read_8, offs)

    def get_16(self, offs):
        return self._get_helper(self.read_16, offs)

    def get_16s(self, offs):
        return self._get_helper(self.read_16s, offs)

    def get_32(self, offs):
        return self._get_helper(self.read_32, offs)

    def get_32s(self, offs):
        return self._get_helper(self.read_32s, offs)

    def get_classname(self, idx):
        if idx == 0xFFFF:
            return "<none>"
        if idx >= len(self.symdata.classNames):
            return "<bad class index %d>" % idx
        nm = self.symdata.classNames[idx]
        if idx >= self.nInternalClassCount:
            return self.symdata.packageNames[self.symdata.pidExtClassPackage[idx - self.nInternalClassCount]] + "." + nm
        return nm

    def get_pkgname(self, idx):
        return self.symdata.packageNames[idx]

    def get_javaname(self, cidx):
        if cidx >= self.nInternalClassCount:
            pidx = self.symdata.pidExtClassPackage[cidx - self.nInternalClassCount]
        else:
            pidx = self.classes[cidx].pidPackage
        pkgname = self.symdata.packageNames[pidx]
        cname = self.symdata.classNames[cidx]
        return pkgname.replace(".", "/") + "/" + cname

    def get_field(self, idx):
        if idx >= len(self.symdata.fieldInfos):
            return "<bad field index %d>" % idx
        return self.symdata.fieldInfos[idx]

    def get_method(self, idx):
        if idx >= len(self.symdata.methodInfos):
            return "<bad method index %d>" % idx
        return self.symdata.methodInfos[idx]

    def _print(self, indent=0):
        print " "*indent, "JEFF file header:"
        print_hdr(self.h1, self.h2, self.dofClassHeader)
        print " "*indent, "file attributes:"
        self.attributes._print(indent+2)
        print " "*indent, "file symbolic data:"
        self.symdata._print(indent+2)
        print " "*indent, "classes:"
        for i in range(len(self.classes)):
            print "\nClass %d (offset 0x%04X):\n===========================" % (i, self.dofClassHeader[i])
            self.classes[i]._print(2)

    def __init__(self, f, offset = 0):
        self.offset = 0
        self.f = f[offset:]
        h1 = self.get_struct(VMFileHeaderStart)
        if ( h1.nMagicWord1 != ord('J') or
             h1.nMagicWord2 != ord('E') or
             h1.nMagicWord3 != ord('F') or
             h1.nMagicWord4 != ord('F')):
            raise Exception("Bad signature! Not a JEFF file?")

        self.linked = h1.nFormatVersionMajor == 254
        if not self.linked and (h1.nFormatVersionMajor > 1 or (h1.nFormatVersionMajor == 1 and h1.nFormatVersionMinor > 0)):
            print "Warning: file format (%d.%d) newer than expected (1.0)" % (h1.nFormatVersionMajor, h1.nFormatVersionMinor)

        self.bigendian = h1.nByteOrder & VM_ORDER_INT_BIG
        if self.bigendian:
          h2 = self.get_struct(VMFileHeaderRestBE)
        else:
          h2 = self.get_struct(VMFileHeaderRestLE)

        self.h1 = h1
        self.h2 = h2
        self.f = f[offset:offset+h2.nFileLength]
        self.dofClassHeader = [self.read_32() for i in range(h2.nInternalClassCount)]
        for x in h2._fields_:
          setattr(self, x[0], getattr(h2, x[0]))
        self.attributes = JeffAtributes(self)
        self.symdata = JeffSymbolicData(self)
        self.classes = [JeffClass(self, off) for off in self.dofClassHeader]

"""
VMAttributeSection {
  VMDOFFSET dofFileAttributeList;
  VMDOFFSET dofClassAttributes[nInternalClassCount];
  TU2 nAttributeTypeCount;
  TU2 nClassAttributeCount;
  VMAttributeType sAttributeType[nAttributeTypeCount];
  VMClassAttributes sClassAttributes[nClassAttributeCount]
  TU2 nAttributeTableCount;
  VMAttributeTable sAttributeTable[nAttributeTableCount];
}

VMAttributeType {
  VMDOFFSET dofTypeName;
  TU2 nTypeFlags;
  TU2 nTypeLength;
}

VMClassAttributes {
  VMDOFFSET dofClassAttributeList;
  VMDOFFSET dofFieldAttributeList[nFieldCount];
  VMDOFFSET dofMethodAttributeList[nMethodCount];
}

VMAttributeTable {
  TU2 nAttributeCount;
  {
    TU2 nAttributeType;
    TU1 <0-2 byte pad>
    TU4 nTypeLength;    // only if VM_ATTR_CST_LENGTH is not set in the type's nTypeFlags
    TU1 nData[nTypeLength];
  } VMAttribute[nAttributeCount]
}
"""

# values for VMAttributeType's nTypeFlags
VM_ATTR_INDEXES    = 0x0001 # The attribute contains some index values of type
                            # VMPINDEX, VMCINDEX, VMMINDEX or VMFINDEX.
VM_ATTR_VMOFFSETS  = 0x0002 # The attribute contains some values of type VMOFFSET.
VM_ATTR_VMDOFFSETS = 0x0004 # The attribute contains some values of type VMDOFFSET.
VM_ATTR_BYTE_ORDER = 0x0008 # The elements stored in nData (See the
                            # VMAttributeTable structure) contain byte ordered
                            # values. Must be set if the VM_ATTR_INDEXES, VM_ATTR_VMOFFSETS,
                            # or VM_ATTR_VMDOFFSETS flags are specified.
VM_ATTR_CST_LENGTH = 0x0010 # The length of the attribute is constant and given by
                            # the nTypeLength item. This flag can only be used if
                            # the length of the attribute structure is not subject to
                            # variations caused by the type alignment and if the
                            # length can be encoded with a TU2 variable.

class VMAttributeType:
    def __init__(self, jeff):
        self.dofTypeName = jeff.read_32()
        self.nTypeFlags  = jeff.read_16()
        self.nTypeLength = jeff.read_16()
        self.name = jeff.get_string(self.dofTypeName)

    def _print(self, indent = 0):
        print " "*indent, "Type name: %r" % self.name
        print " "*indent, "Type flags (%02X):" % self.nTypeFlags,
        if self.nTypeFlags & VM_ATTR_INDEXES:
            print "VM_ATTR_INDEXES",
        if self.nTypeFlags & VM_ATTR_VMOFFSETS:
            print "VM_ATTR_VMOFFSETS",
        if self.nTypeFlags & VM_ATTR_VMDOFFSETS:
            print "VM_ATTR_VMDOFFSETS",
        if self.nTypeFlags & VM_ATTR_BYTE_ORDER:
            print "VM_ATTR_BYTE_ORDER",
        if self.nTypeFlags & VM_ATTR_CST_LENGTH:
            print "VM_ATTR_CST_LENGTH",
        print ""
        if self.nTypeFlags & VM_ATTR_CST_LENGTH:
            print " "*indent, "Type length: %d" % self.nTypeLength

class VMAttribute:
    def __init__(self, jeff, types):
        self.nAttributeType = jeff.read_16()
        self.jeff = jeff
        self.typ  = types[self.nAttributeType]
        if self.typ.nTypeFlags & VM_ATTR_CST_LENGTH:
            nTypeLength = self.typ.nTypeLength
        else:
            nTypeLength = jeff.read_32(True)
        self.nData = jeff.read_bytes(nTypeLength)

    def _print(self, indent = 0):
        print " "*indent, "Attribute type:"
        self.typ._print(indent+2)
        print " "*indent, "Attribute data: %r" % (self.nData)

class VMAttributeTable:
    def __init__(self, jeff, types):
        nAttributeCount = jeff.read_16()
        self.jeff = jeff
        self.types = types
        self.attributes = [VMAttribute(jeff, types) for i in range(nAttributeCount)]
    def __getitem__(self, key):
        return self.attributes[key]

    def _print(self, indent = 0):
        for i in range(len(self.attributes)):
            print " "*indent, "attribute %d:" % i
            self.attributes[i]._print(indent+2)


class JeffAtributes:
    def __init__(self, jeff):
        self.jeff = jeff
        self.FileAttribute = None
        self.ClassAttributes = []
        if jeff.dofAttributeSection != 0:
            jeff.offset = jeff.dofAttributeSection
            dofFileAttributeList = jeff.read_32()
            dofClassAttributes = [jeff.read_32() for i in range(jeff.nInternalClassCount)]
            nAttributeTypeCount = jeff.read_16()
            nClassAttributeCount = jeff.read_16()
            self.types = [VMAttributeType(jeff) for i in range(nAttributeTypeCount)]
            if dofFileAttributeList != 0:
                jeff.offset = dofFileAttributeList
                self.FileAttribute = VMAttributeTable(jeff, self.types)
            for ca in dofClassAttributes:
                if ca != 0:
                    jeff.offset = dofFileAttributeList
                    self.ClassAttributes.append(VMAttributeTable(jeff, self.types))
                else:
                    self.ClassAttributes.append(None)

    def _print(self, indent = 0):
        if self.FileAttribute:
            print " "*indent, "File attributes:"
            self.FileAttribute._print(indent + 2)
        if self.ClassAttributes:
            print " "*indent, "Class attributes:"
            for i in range(len(self.ClassAttributes)):
                ca = self.ClassAttributes[i]
                if ca:
                  print " "*(indent+2), "Class %d:" % i
                  ca._print(indent+4)

"""
VMClassHeader {
  VMOFFSET ofThisClassIndex;
  VMPINDEX pidPackage;
  VMACCESS aAccessFlag;
  TU2 nClassData;
  VMOFFSET ofClassConstructor;
  VMOFFSET ofInterfaceTable;
  VMOFFSET ofFieldTable;
  VMOFFSET ofMethodTable;
  VMOFFSET ofReferencedFieldTable;
  VMOFFSET ofReferencedMethodTable;
  VMOFFSET ofReferencedClassTable;
  VMOFFSET ofConstantDataSection;
  VMOFFSET ofSuperClassIndex;       // not present for interface
  TU2 nInstanceData;                // not present for interface
  VMOFFSET ofInstanceConstructor;   // not present for interface
}

VMInterfaceTable {
  TU2 nInterfaceCount;
  VMOFFSET ofInterfaceIndex [nInterfaceCount];
}

VMFieldInfoTable {
  TU2 nFieldCount;
  TU1 <0-2 byte pad>
  {
    VMFINDEX fidFieldIndex;
    VMOFFSET ofThisClassIndex;
    VMTYPE tFieldType;
    TU1 nTypeDimension;
    VMACCESS aAccessFlag;
    TU2 nFieldDataOffset;
  } VMFieldInfo [nFieldCount];
}

VMMethodInfoTable {
  TU2 nMethodCount;
  TU1 <0-2 byte pad>
  {
    VMMINDEX midMethodIndex;
    VMOFFSET ofThisClassIndex;
    VMNCELL ncStackArgument;
    VMACCESS aAccessFlag;
    VMOFFSET ofCode;
  } VMMethodInfo [nMethodCount];
  TU4 nNativeReference[];
}

VMReferencedFieldTable {
  TU2 nFieldCount;
  TU1 <0-2 byte pad>
  {
    VMFINDEX fidFieldIndex;
    VMOFFSET ofClassIndex;
    VMTYPE tFieldType;
    TU1 nTypeDimension;
  } VMReferencedField [nFieldCount];
}

VMReferencedMethodTable {
  TU2 nMethodCount;
  TU1 <0-2 byte pad>
  {
    VMMINDEX midMethodIndex;
    VMOFFSET ofClassIndex;
    VMNCELL ncStackArgument;
  } VMReferencedMethod [nMethodCount];
}

VMReferencedClassTable {
  TU2 nReferencedClassCount;
  VMCINDEX cidReferencedClass [nReferencedClassCount];
}

VMBytecodeBlock {
  VMNCELL ncMaxStack;
  VMNCELL ncMaxLocals;
  VMOFFSET ofExceptionCatchTable;
  TU2 nByteCodeSize;
  TU1 bytecode[nByteCodeSize];
}

VMExceptionCatchTable {
  TU2 nCatchCount;
  {
    VMOFFSET ofStartPc;
    VMOFFSET ofEndPc;
    VMOFFSET ofHandlerPc;
    VMOFFSET ofExceptionIndex;
  } VMExceptionCatch [nCatchCount];
}

VMConstantDataSection {
  TU2 nConstFlags;
  TU2 nDoubleNumber;
  TU2 nLongNumber;
  TU2 nFloatNumber;
  TU2 nIntNumber;
  TU2 nShortNumber;
  TU2 nByteNumber;
  TU2 nStringNumber;
  JDOUBLE nDoubleValue[nDoubleNumber];
  JLONG nLongValue[nLongNumber];
  JFLOAT nFloatValue[nFloatNumber];
  JINT nIntValue[nIntNumber];
  JSHORT nShortValue[nShortNumber];
  JBYTE nByteValue[nByteNumber];
  TU1 <0-1 byte pad>
  VMString strConstString[nStringNumber];
}

"""

class VMFieldInfo:
    def __init__(self, jeff):
        self.jeff = jeff
        self.fidFieldIndex    = jeff.read_32()
        self.ofThisClassIndex = jeff.read_16()
        self.tFieldType       = jeff.read_8()
        self.nTypeDimension   = jeff.read_8()
        self.aAccessFlag      = jeff.read_16()
        self.nFieldDataOffset = jeff.read_16()

    def _dump(self, indent = 0):
        f = self.jeff.get_field(self.fidFieldIndex)
        print ".field %s %s %s" % (" ".join(acc2flagsj(self.aAccessFlag)), f.name, f.descr.get_jtype())

    def _print(self, indent = 0):
        print " "*indent, "fidFieldIndex:        0x%02X (%s)" % (self.fidFieldIndex, self.jeff.get_field(self.fidFieldIndex))
        print " "*indent, "ofThisClassIndex:     0x%02X" % (self.ofThisClassIndex)
        print " "*indent, "tFieldType:           0x%02X (%s)" % (self.tFieldType, type2str(self.tFieldType))
        print " "*indent, "nTypeDimension:       0x%02X" % (self.nTypeDimension)
        print " "*indent, "aAccessFlag:          0x%02X (%s)" % (self.aAccessFlag, "|".join(acc2flags(self.aAccessFlag)))
        print " "*indent, "nFieldDataOffset:     0x%02X" % (self.nFieldDataOffset)

class VMMethodInfo:
    def __init__(self, jeff, jclass):
        self.jeff = jeff
        self.jclass = jclass
        self.midMethodIndex   = jeff.read_32()
        self.ofThisClassIndex = jeff.read_16()
        self.ncStackArgument  = jeff.read_16()
        self.aAccessFlag      = jeff.read_16()
        self.ofCode           = jeff.read_16()
        if self.ofCode != 0 and ((self.aAccessFlag & ACC_NATIVE) == 0):
            savepos = jeff.tell()
            jeff.seek(self.ofCode + jclass.offset)
            self.code = VMBytecodeBlock(jeff, jclass)
            jeff.seek(savepos)

    def _dump(self, indent = 0):
        md = self.jeff.get_method(self.midMethodIndex)
        print ".method %s %s : %s" % (" ".join(acc2flagsj(self.aAccessFlag, True)), md.name, md.get_jtype())
        if self.aAccessFlag & ACC_NATIVE:
            print ";", " "*(indent+2), "native method %d" % (self.jeff.get_16(self.jclass.offset + self.ofCode))
        elif self.ofCode != 0:
            self.code._dump(indent+2)
            b = BytecodePrinter(self.jclass, self.ofCode, len(self.code.bytecode))
            print ""
            print ";", " "*(indent+2), "Bytecode disassembly:"
            b.print_java(indent+4)
            #b = BytecodePrinter(self.jclass, self.ofCode, len(self.code.bytecode))
            #b._print(indent+4)
        print ".end method"

    def _print(self, indent = 0):
        print " "*indent, "midMethodIndex:       0x%02X (%s)" % (self.midMethodIndex, self.jeff.get_method(self.midMethodIndex))
        print " "*indent, "ofThisClassIndex:     0x%02X" % (self.ofThisClassIndex)
        print " "*indent, "ncStackArgument:      0x%02X" % (self.ncStackArgument)
        print " "*indent, "aAccessFlag:          0x%02X (%s)" % (self.aAccessFlag, "|".join(acc2flags(self.aAccessFlag, True)))
        print " "*indent, "ofCode:               0x%02X" % (self.ofCode),
        if self.aAccessFlag & ACC_NATIVE:
            print "(%d)" % (self.jeff.get_16(self.jclass.offset + self.ofCode))
        else:
            print ""
            if self.ofCode != 0:
                print " "*indent, "Bytecode block:"
                self.code._print(indent+2)
                b = BytecodePrinter(self.jclass, self.ofCode, len(self.code.bytecode))
                print ""
                print " "*(indent+2), "Bytecode disassembly:"
                b.print_jeff(indent+4)

class VMExceptionCatch:
    def __init__(self, jeff):
        self.ofStartPc        = jeff.read_16()
        self.ofEndPc          = jeff.read_16()
        self.ofHandlerPc      = jeff.read_16()
        self.ofExceptionIndex = jeff.read_16()

from opcodes import BytecodePrinter

class VMBytecodeBlock:
    def __init__(self, jeff, jclass):
        self.jeff = jeff
        self.jclass = jclass
        self.ncMaxStack            = jeff.read_16()
        self.ncMaxLocals           = jeff.read_16()
        self.ofExceptionCatchTable = jeff.read_16()
        nByteCodeSize              = jeff.read_16()
        self.bytecode              = jeff.read_bytes(nByteCodeSize)
        if self.ofExceptionCatchTable != 0:
            jeff.seek(jclass.offset + self.ofExceptionCatchTable)
            nCatchCount  = jeff.read_16()
            self.catches = [VMExceptionCatch(jeff) for i in range(nCatchCount)]
        else:
            self.catches = []

    def _dump(self, indent = 0):
        print " "*indent, ".limit stack %d" % (self.ncMaxStack)
        print " "*indent, ".limit locals %d" % (self.ncMaxLocals)
        for c in self.catches:
            if c.ofExceptionIndex == 0xFFFF:
                ctype = "[0]"
            else:
                cidx = self.jclass.get_index(c.ofExceptionIndex)
                ctype = self.jeff.get_javaname(cidx)
                # print ".catch %s from L%04X to L%04X using L%04X" % (ctype, c.ofStartPc, c.ofEndPc, c.ofHandlerPc)

    def _print(self, indent = 0):
        print " "*indent, "ncMaxStack:            0x%02X" % (self.ncMaxStack)
        print " "*indent, "ncMaxLocals:           0x%02X" % (self.ncMaxLocals)
        print " "*indent, "ofExceptionCatchTable: 0x%02X" % (self.ofExceptionCatchTable)
        print " "*indent, "nByteCodeSize:         0x%02X" % (len(self.bytecode))
        if self.ofExceptionCatchTable != 0:
            print ""
            print " "*indent, "===Exception Table List===="
            print " "*indent, "nCatchCount:           0x%02X" % (len(self.catches))
            print " "*indent, " # ofStartPc ofEndPc ofHandlerPc ofExceptionIndex"
            for i in range(len(self.catches)):
                c = self.catches[i]
                if c.ofExceptionIndex != 0:
                    cidx = self.jclass.get_index(c.ofExceptionIndex)
                    ename = self.jeff.get_classname(cidx)
                else:
                    ename = "..."

                print " "*indent, "%2d    0x%04X  0x%04X      0x%04X             0x%02X (%s)" % (i, c.ofStartPc, c.ofEndPc, c.ofHandlerPc, c.ofExceptionIndex, ename)


class JeffClass:
    def __init__(self, jeff, offs):
        self.jeff = jeff
        self.offset = offs
        jeff.seek(offs)
        self.ofThisClassIndex        = jeff.read_16()
        self.pidPackage              = jeff.read_16()
        self.aAccessFlag             = jeff.read_16()
        self.nClassData              = jeff.read_16()
        self.ofClassConstructor      = jeff.read_16()
        self.ofInterfaceTable        = jeff.read_16()
        self.ofFieldTable            = jeff.read_16()
        self.ofMethodTable           = jeff.read_16()
        self.ofReferencedFieldTable  = jeff.read_16()
        self.ofReferencedMethodTable = jeff.read_16()
        self.ofReferencedClassTable  = jeff.read_16()
        self.ofConstantDataSection   = jeff.read_16()
        if (self.aAccessFlag & ACC_INTERFACE) == 0:
            self.ofSuperClassIndex     = jeff.read_16()
            self.nInstanceData         = jeff.read_16()
            self.ofInstanceConstructor = jeff.read_16()

        self._read_fields()
        self._read_methods()
        self._read_ifaces()

    def get_index(self, fld):
        if self.jeff.linked:
            return fld
        else:
            return self.jeff.get_16(self.offset + fld)

    def _soff_by_idx(self, idx):
        "Seek to the string in constant data section by its index"

        # see VMConstantDataSection
        if self.ofConstantDataSection == 0:
            return 0
        savepos = self.jeff.tell()
        self.jeff.seek(self.offset + self.ofConstantDataSection)
        nConstFlags   = self.jeff.read_16()
        nDoubleNumber = self.jeff.read_16() if (nConstFlags & 0x01) else 0
        nLongNumber   = self.jeff.read_16() if (nConstFlags & 0x02) else 0
        nFloatNumber  = self.jeff.read_16() if (nConstFlags & 0x04) else 0
        nIntNumber    = self.jeff.read_16() if (nConstFlags & 0x08) else 0
        nShortNumber  = self.jeff.read_16() if (nConstFlags & 0x10) else 0
        nByteNumber   = self.jeff.read_16() if (nConstFlags & 0x20) else 0
        nStringNumber = self.jeff.read_16() if (nConstFlags & 0x40) else 0
        if nDoubleNumber or nLongNumber:
            self.jeff.align(8)
            self.jeff.skip(8*(nDoubleNumber+nLongNumber))
        if nFloatNumber or nIntNumber:
            self.jeff.align(4)
            self.jeff.skip(4*(nFloatNumber+nIntNumber))
        self.jeff.skip(2*nShortNumber + nByteNumber)
        self.jeff.align(2)
        if idx >= nStringNumber:
            res = 0
        else:
            for i in range(idx):
                self.jeff.read_string()
                self.jeff.align(2)
            res = self.jeff.tell()
        self.jeff.seek(savepos)
        return res

    """
    VMString {
      TU2 nStringLength;
      TU1 nStringValue[nStringLength];
    }
    """

    def get_sconst(self, ofConstant, count = 1):
        soff = self.offset + ofConstant
        if self.jeff.linked:
            # ofConstant is the index of the string in the constant pool
            soff = self._soff_by_idx(ofConstant)
            if soff == 0:
                return "<bad string index %d>" % ofConstant
            # print "string index %d, offset 0x%x" % (ofConstant, soff)
        r = []
        for i in range(count):
            s = self.jeff.get_string(soff)
            soff += (len(s) + 3) & ~1
            r.append(s)
        if count == 1:
            return r[0]
        else:
            return r

    def _const_helper(self, ofConstant, count, reader):
        savepos = self.jeff.tell()
        self.jeff.seek(self.offset + ofConstant)
        r = [reader() for i in range(count)]
        self.jeff.seek(savepos)
        return r

    def get_iconst(self, ofConstant, count = 1):
        r = self._const_helper(ofConstant, count, self.jeff.read_32s)
        if count==1:
            return r[0]
        else:
            return r

    def get_lconst(self, ofConstant, count = 1):
        r = self._const_helper(ofConstant, count, self.jeff.read_64s)
        if count==1:
            return r[0]
        else:
            return r

    def get_array(self, tArrayType, ofConstData, nLength):
        t = tArrayType & 0xF
        arrtyp = type2str(t, 1, False)
        if t in [VM_TYPE_BYTE, VM_TYPE_CHAR, VM_TYPE_BOOLEAN]:
            vals = self._const_helper(ofConstData, nLength, self.jeff.read_8)
            if t == VM_TYPE_CHAR:
                vals = map(chr, vals)
        elif t == VM_TYPE_SHORT:
            vals = self._const_helper(ofConstData, nLength, self.jeff.read_16s)
        elif t == VM_TYPE_INT:
            vals = self._const_helper(ofConstData, nLength, self.jeff.read_32s)
        else:
            return "<unhandled array type: %s>" % arrtyp

        return "<%s %r>" % (arrtyp, vals)

    def get_array_values(self, tArrayType, ofConstData, nLength):
        t = tArrayType & 0xF
        arrtyp = type2str(t, 0, False)
        if t in [VM_TYPE_BYTE, VM_TYPE_CHAR, VM_TYPE_BOOLEAN]:
            vals = self._const_helper(ofConstData, nLength, self.jeff.read_8)
        elif t == VM_TYPE_SHORT:
            vals = self._const_helper(ofConstData, nLength, self.jeff.read_16s)
        elif t == VM_TYPE_INT:
            vals = self._const_helper(ofConstData, nLength, self.jeff.read_32s)
        else:
            raise Exception("unhandled array type: %s" % arrtyp)
        return (arrtyp, vals)

    def _read_fields(self):
        if self.ofFieldTable == 0:
            self.fields = []
            return
        self.jeff.seek(self.offset + self.ofFieldTable)
        nFieldCount = self.jeff.read_16()
        self.jeff.align(4)
        self.fields = [VMFieldInfo(self.jeff) for i in range(nFieldCount)]

    def _read_methods(self):
        self.methods = []
        if self.ofMethodTable == 0:
            return
        self.jeff.seek(self.offset + self.ofMethodTable)
        nMethodCount = self.jeff.read_16()
        self.jeff.align(4)
        nNativeCount = 0
        for i in range(nMethodCount):
            m = VMMethodInfo(self.jeff, self)
            if m.aAccessFlag & ACC_NATIVE:
                nNativeCount += 1
            self.methods.append(m)
        self.nNativeReference = [self.jeff.read_32() for i in range(nNativeCount)]

    def _read_ifaces(self):
        if self.ofInterfaceTable == 0:
            self.interfaces = []
            return
        self.jeff.seek(self.offset + self.ofInterfaceTable)
        nIntfCount = self.jeff.read_16()
        self.interfaces = [self.get_index(self.jeff.read_16()) for i in range(nIntfCount)]

    def _print(self, indent=0):
        cidx = self.get_index(self.ofThisClassIndex)
        #print " "*indent, "Package: '%s'" % (self.jeff.get_pkgname(self.pidPackage))
        #print " "*indent, "Class:   '%s'" % (self.jeff.get_classname(cidx))

        print " "*indent, "ofThisClassIndex:        0x%02X (%s)" % (self.ofThisClassIndex, self.jeff.get_classname(cidx))
        print " "*indent, "pidPackage:              0x%02X (%s)" % (self.pidPackage, self.jeff.get_pkgname(self.pidPackage))
        print " "*indent, "aAccessFlag:             0x%02X (%s)" % (self.aAccessFlag, "|".join(acc2flags(self.aAccessFlag)))
        print " "*indent, "nClassData:              0x%02X" % (self.nClassData)
        print " "*indent, "ofClassConstructor:      0x%02X" % (self.ofClassConstructor)
        print " "*indent, "ofInterfaceTable:        0x%02X" % (self.ofInterfaceTable)
        print " "*indent, "ofFieldTable:            0x%02X" % (self.ofFieldTable)
        print " "*indent, "ofMethodTable:           0x%02X" % (self.ofMethodTable)
        print " "*indent, "ofReferencedFieldTable:  0x%02X" % (self.ofReferencedFieldTable)
        print " "*indent, "ofReferencedMethodTable: 0x%02X" % (self.ofReferencedMethodTable)
        print " "*indent, "ofConstantDataSection:   0x%02X" % (self.ofConstantDataSection)
        if (self.aAccessFlag & ACC_INTERFACE) == 0:
            sidx = self.get_index(self.ofSuperClassIndex)
            print " "*indent, "ofSuperClassIndex:       0x%02X (%s)" % (self.ofSuperClassIndex, self.jeff.get_classname(sidx))
            print " "*indent, "nInstanceData:           0x%02X" % (self.nInstanceData)
            print " "*indent, "ofInstanceConstructor:   0x%02X" % (self.ofInstanceConstructor)

        if self.interfaces:
            print ""
            print " "*indent, "Interfaces:", ", ".join([self.jeff.get_classname(self.interfaces[i]) for i in range(len(self.interfaces))])

        if self.fields:
            print ""
            for i in range(len(self.fields)):
                print ""
                print " "*indent, "Field %d:" % i
                self.fields[i]._print(indent+2)

        if self.methods:
            print ""
            for i in range(len(self.methods)):
                print ""
                print " "*indent, "Method %d:" % i
                self.methods[i]._print(indent+2)
            if self.nNativeReference:
                print " "*indent, "Native references: %s" % " ".join("0x%04X" %n for n in self.nNativeReference)

        print ""

    def _dump_consts(self, indent=0, as_java = False):
        "Print constant section"

        # see VMConstantDataSection
        if self.ofConstantDataSection == 0:
            return 0
        savepos = self.jeff.tell()
        self.jeff.seek(self.offset + self.ofConstantDataSection)
        nConstFlags   = self.jeff.read_16()
        nDoubleNumber = self.jeff.read_16() if (nConstFlags & 0x01) else 0
        nLongNumber   = self.jeff.read_16() if (nConstFlags & 0x02) else 0
        nFloatNumber  = self.jeff.read_16() if (nConstFlags & 0x04) else 0
        nIntNumber    = self.jeff.read_16() if (nConstFlags & 0x08) else 0
        nShortNumber  = self.jeff.read_16() if (nConstFlags & 0x10) else 0
        nByteNumber   = self.jeff.read_16() if (nConstFlags & 0x20) else 0
        nStringNumber = self.jeff.read_16() if (nConstFlags & 0x40) else 0
        pos = self.jeff.tell() - self.offset
        if nDoubleNumber or nLongNumber:
            self.jeff.align(8)
            pos = self.jeff.tell() - self.offset
        if nDoubleNumber:
            pos = self.jeff.tell() - self.offset
            vals = self._const_helper(pos, nDoubleNumber, self.jeff.read_64d)
            for i,v in enumerate(vals):
               print " "*indent, ".const [o%d] = Double %f" % (pos, v)
               pos+=8

        if nLongNumber:
            vals = self._const_helper(pos, nLongNumber, self.jeff.read_64s)
            for i,v in enumerate(vals):
               # print " "*indent, ".const [o%d] = Long %dL ; 0x%X" % (pos, v, v&0xFFFFFFFFFFFFFFFF)
               pos+=8

        if nFloatNumber or nIntNumber:
            self.jeff.align(4)
            pos = self.jeff.tell() - self.offset

        if nFloatNumber:
            vals = self._const_helper(pos, nFloatNumber, self.jeff.read_32f)
            for i,v in enumerate(vals):
               print " "*indent, ".const [o%d] = Float %f" % (pos, v)
               pos+=4

        if nIntNumber:
            vals = self._const_helper(pos, nIntNumber, self.jeff.read_32s)
            for i,v in enumerate(vals):
               print " "*indent, ".const [o%d] = Int %d ; 0x%08X" % (pos, v, v&0xFFFFFFFF)
               pos+=4

        if nShortNumber:
            vals = self._const_helper(pos, nShortNumber, self.jeff.read_16s)
            for i,v in enumerate(vals):
               # print " "*indent, ".const [o%d] = Short %d ; 0x%04X" % (pos, v, v&0xFFFF)
               pos+=2

        if nByteNumber:
            vals = self._const_helper(pos, nByteNumber, self.jeff.read_8s)
            for i,v in enumerate(vals):
               # print " "*indent, ".const [%d] = Byte %d ; 0x%02X" % (pos, v, v&0xFF)
               pos+=1

        self.jeff.seek(self.offset + pos)
        self.jeff.align(2)
        for i in range(nStringNumber):
            pos = self.jeff.tell() - self.offset
            s = self.jeff.read_string()
            rs = repr(s)
            # print " "*indent, '.const [o%d] = String "%s"' % (pos, rs[1:-1])
            self.jeff.align(2)

        self.jeff.seek(savepos)

    def _dump(self, indent=0, as_java = False):
        cidx = self.get_index(self.ofThisClassIndex)
        fullname = self.jeff.get_javaname(cidx)
        print ".class %s %s" % (" ".join(acc2flagsj(self.aAccessFlag)), fullname)

        sidx = 0xFFFF
        if (self.aAccessFlag & ACC_INTERFACE) == 0 and self.ofSuperClassIndex != 0:
            sidx = self.get_index(self.ofSuperClassIndex)

        if sidx != 0xFFFF:
            print ".super %s" % self.jeff.get_javaname(sidx)
        else:
            if fullname=='java/lang/Object':
                print ".super [0]"
            else:
                 print ".super java/lang/Object"

        if self.interfaces:
            print ""
            for iidx in self.interfaces:
                print ".implements %s" % self.jeff.get_javaname(iidx)

        self._dump_consts(indent, as_java)

        if self.fields:
            print ""
            for f in self.fields:
                print ""
                f._dump(indent+2)

        if self.methods:
            print ""
            for i, m in enumerate(self.methods):
                print ""
                m._dump(indent+2)

        print ".end class"

"""
VMSymbolicDataSection {
  VMPINDEX pidExtClassPackage[nTotalClassCount-nInternalClassCount];
  TU1 <0-2 byte pad>
  VMDOFFSET dofPackageName[nTotalPackageCount];
  VMDOFFSET dofClassName[nTotalClassCount];
  {
    VMDOFFSET dofFieldName;
    VMDOFFSET dofFieldDescriptor;
  } VMFieldSymbolicInfo[nTotalFieldCount]
  {
    VMDOFFSET dofMethodName;
    VMDOFFSET dofMethodDescriptor;
  } VMMethodSymbolicInfo[nTotalMethodCount]
}

VMDescriptor
{
  VMTYPE tDataType;
  TU1 nDataTypeDimension;      // value is only present if the type is an n-dimensional array, where n >= 2.
  TU1 <0-1 byte pad>
  VMCINDEX cidDataTypeIndex;   // The class index associated with the data type. This item is present only if
                               // the tDataType is not a primitive type or an array of primitive types.
}
"""

class VMDescriptor:
    def __init__(self, jeff, jclass = None):
        self.jeff = jeff
        self.tDataType = jeff.read_8()
        if self.tDataType & VM_TYPE_MULTI:
            self.nDataTypeDimension = jeff.read_8()
        elif self.tDataType & VM_TYPE_MONO:
            self.nDataTypeDimension = 1
        else:
            self.nDataTypeDimension = 0
        if (self.tDataType & 0xF) == VM_TYPE_OBJECT:
            self.cidDataTypeIndex = jeff.read_16(True)
            if jclass:
                self.cidDataTypeIndex = jclass.get_index(self.cidDataTypeIndex)

    def is_object(self):
        return (self.tDataType & 0xF) == VM_TYPE_OBJECT

    def get_jtype(self, forarray = False):
        if (self.tDataType & 0xF) == VM_TYPE_OBJECT:
            nm = "L%s;" % self.jeff.get_javaname(self.cidDataTypeIndex)
        else:
            nm = BASIC_TYPESJ[self.tDataType & 0xF]
        if forarray:
           assert(self.nDataTypeDimension==1)
           return nm
        if self.nDataTypeDimension != 0:
            nm = "[" * self.nDataTypeDimension + nm
        return nm

    def arrtype(self):
        if self.nDataTypeDimension != 1:
          raise Exception("descriptor %s is not a 1-dimensional array type!", self.__str__())
        if (self.tDataType & 0xF) == VM_TYPE_OBJECT:
            nm = self.jeff.get_classname(self.cidDataTypeIndex)
        else:
            nm = BASIC_TYPES[self.tDataType & 0xF]
        return nm

    def __str__(self):
        if (self.tDataType & 0xF) == VM_TYPE_OBJECT:
            nm = self.jeff.get_classname(self.cidDataTypeIndex)
        else:
            nm = BASIC_TYPES[self.tDataType & 0xF]
        if self.nDataTypeDimension != 0:
            nm += "[]" * self.nDataTypeDimension
        return nm

class JeffFieldDescInfo:
    def __init__(self, jeff):
        self.jeff = jeff
        self.name = jeff.get_string(jeff.read_32())
        dofDescr = jeff.read_32()
        saveoff = jeff.offset
        jeff.offset = dofDescr
        self.descr = VMDescriptor(jeff)
        jeff.offset = saveoff

    def __str__(self):
        return "%s %s" % (self.descr, self.name)

    def _print(self, indent=0):
        print " "*indent, "%s;" % (self)

"""
VMMethodDescriptor {
  TU2 nArgCount;
  VMDescriptor sArgumentType[nArgCount];
  VMDescriptor sReturnType;
}
"""

class VMMethodDescriptor:
    def __init__(self, jeff):
        self.jeff = jeff
        nArgCount = jeff.read_16()
        self.sArgumentType = [VMDescriptor(jeff) for i in range(nArgCount)]
        self.sReturnType = VMDescriptor(jeff)

class JeffMethodDescInfo:
    def __init__(self, jeff):
        self.jeff = jeff
        self.name = jeff.get_string(jeff.read_32())
        dofDescr = jeff.read_32()
        saveoff = jeff.offset
        jeff.offset = dofDescr
        self.descr = VMMethodDescriptor(jeff)
        jeff.offset = saveoff

    def get_jtype(self):
        tret = self.descr.sReturnType.get_jtype()
        targs = [d.get_jtype() for d in self.descr.sArgumentType]
        return "(%s)%s" % ("".join(targs), tret)

    def get_jtypen(self):
        tret = self.descr.sReturnType.get_jtype()
        targs = [d.get_jtype() for d in self.descr.sArgumentType]
        return "(%s)%s %d" % ("".join(targs), tret, len(self.descr.sArgumentType))

    def __str__(self):
        return "%s %s(%s)" % (self.descr.sReturnType, self.name, ", ".join(str(a) for a in self.descr.sArgumentType))

    def _print(self, indent=0):
        print " "*indent, "%s" % (self)

class JeffSymbolicData:
    def __init__(self, jeff):
        self.jeff = jeff
        jeff.offset = jeff.dofSymbolicData
        self.pidExtClassPackage = [jeff.read_16() for i in range(jeff.nTotalClassCount - jeff.nInternalClassCount)]
        jeff.offset = (jeff.offset + 3) & ~3
        self.packageNames = [jeff.get_string(jeff.read_32()) for i in range(jeff.nTotalPackageCount)]
        self.classNames   = [jeff.get_string(jeff.read_32()) for i in range(jeff.nTotalClassCount)]
        self.fieldInfos   = [JeffFieldDescInfo(jeff) for i in range(jeff.nTotalFieldCount)]
        self.methodInfos  = [JeffMethodDescInfo(jeff) for i in range(jeff.nTotalMethodCount)]

    def _print(self, indent=0):
        print " "*indent, "Package names:"
        print " "*(indent+2), ", ".join(self.packageNames)
        print " "*indent, "Class names:"
        print " "*(indent+2), ", ".join(self.classNames)
        print " "*indent, "Internal classes:"
        print " "*(indent+2), ", ".join(self.classNames[i] for i in range(self.jeff.nInternalClassCount))
        print " "*indent, "External classes:"
        print " "*(indent+2), ", ".join(self.packageNames[self.pidExtClassPackage[i]]+"."+self.classNames[i+self.jeff.nInternalClassCount] for i in range(self.jeff.nTotalClassCount-self.jeff.nInternalClassCount))
        print " "*indent, "Fields:"
        for f in self.fieldInfos:
            f._print(indent+2)
        print " "*indent, "Methods:"
        for m in self.methodInfos:
            m._print(indent+2)

def usage():
  print "Usage: dump_jeff.py [options] file.jeff"
  print " Options:"
  print " -d: dump JEFF structure (default)"
  print " -s: print class definition summary"
  print " -j: print Jasmin-style disassembly (experimental)"
  sys.exit(1)

if len(sys.argv) < 2:
  usage()

do_dump = True
do_summary = False
do_jasmin = False
for a in sys.argv[1:]:
  if a=="-d": do_dump = True
  elif a=="-j": do_jasmin = True
  elif a=="-s": do_summary = True
  elif a[:1]=='-': usage()
  elif a: fn = a

jeff = None
f = open(fn, "rb").read()
if f[:4]=='JEFF':
  jeff = JeffFile(f)
elif f[:4] == '\x04\x00\x00\x00' and f[0x1C:0x20]=='$MN2' and f[0x284:0x28A] == 'Medal\x00':
  print "<packaged Medal App>"
  maniflen = DwordAt(f, 0x18)*4
  skip = ord(f[maniflen])
  if skip > 8:
    print "bad skip value?! (%d)" % skip
    sys.exit(1)
  extralen = DwordAt(f, maniflen + skip)
  if extralen > 0xFF:
    extralen = struct.unpack("> I", f[maniflen + skip:maniflen + skip+4])[0]
  pos = maniflen + extralen + skip
  print hex(pos)
  while pos < len(f):
    dtag, dlen = f[pos:pos+4], DwordAt(f, pos+4)
    print "tag: %s, len: 0x%x" % (dtag, dlen)
    pos += 8
    data = f[pos:pos+dlen]
    if dtag == 'JEFF':
        jeff = JeffFile(data)
    elif dtag == 'SMAN':
        print "Applet properties:"
        print "========================================"
        print data
        print "========================================"
    elif dtag == 'BHBT':
        print "BHBT tag"
        joff, jlen = struct.unpack("<II", f[pos+0x18:pos+0x20])
        joff+=pos-8
        print "jeff offset: %08X, length: %08X" %(joff, jlen)
        data = f[joff:joff+jlen]
        jeff = JeffFile(data)
        break

    pos += dlen
else:
  idx = 0
  while True:
    idx = f.find('JEFF', idx)
    if idx == -1: break
    try:
      jeff = JeffFile(f, idx)
      print "; found JEFF at 0x%08X, length=%08X" % (idx, jeff.h2.nFileLength)
      break
      idx += 4
    except:
      raise
  # print "not a JEFF?"

if not jeff:
  #print "JEFF file not found!"
  if not jeff:
     print "JEFF file not found!"
     sys.exit(1)

if do_summary:
    print "****"

    for cl in jeff.classes:
       cidx = cl.get_index(cl.ofThisClassIndex)
       print "%s.%s" % (jeff.get_pkgname(cl.pidPackage), jeff.get_classname(cidx))
       curaccess = None
       for f in cl.fields:
            print "  /* 0x%02X */ %s; /* %s */" % (f.nFieldDataOffset, jeff.get_field(f.fidFieldIndex), "|".join(acc2flags(f.aAccessFlag)))
       for m in cl.methods:
            print "  %s; /* %s */" % (jeff.get_method(m.midMethodIndex), "|".join(acc2flags(m.aAccessFlag)))

if do_dump and not do_jasmin:
  jeff._print(0)

if do_jasmin:
  for i, cl in enumerate(jeff.classes):
    print "; class %d/%d" % (i, len(jeff.classes))
    cl._dump(2, True)
    print ""
