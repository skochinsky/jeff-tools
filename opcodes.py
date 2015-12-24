opcodetbl = [
    ("jeff_nop",             0),  # 0x00
    ("jeff_aconst_null",     0),  # 0x01
    ("jeff_iconst_m1",       0),  # 0x02
    ("jeff_iconst_0",        0),  # 0x03
    ("jeff_iconst_1",        0),  # 0x04
    ("jeff_iconst_2",        0),  # 0x05
    ("jeff_iconst_3",        0),  # 0x06
    ("jeff_iconst_4",        0),  # 0x07
    ("jeff_iconst_5",        0),  # 0x08
    ("jeff_lconst_0",        0),  # 0x09
    ("jeff_lconst_1",        0),  # 0x0a
    ("jeff_fconst_0",        0),  # 0x0b
    ("jeff_fconst_1",        0),  # 0x0c
    ("jeff_fconst_2",        0),  # 0x0d
    ("jeff_dconst_0",        0),  # 0x0e
    ("jeff_dconst_1",        0),  # 0x0f
    ("jeff_bipush",          1),  # 0x10
    ("jeff_sipush",         -1),  # 0x11
    ("jeff_unused_0x12",     0),  # 0x12
    ("jeff_unused_0x13",     0),  # 0x13
    ("jeff_unused_0x14",     0),  # 0x14
    ("jeff_iload",           1),  # 0x15
    ("jeff_lload",           1),  # 0x16
    ("jeff_fload",           1),  # 0x17
    ("jeff_dload",           1),  # 0x18
    ("jeff_aload",           1),  # 0x19
    ("jeff_iload_0",         0),  # 0x1a
    ("jeff_iload_1",         0),  # 0x1b
    ("jeff_iload_2",         0),  # 0x1c
    ("jeff_iload_3",         0),  # 0x1d
    ("jeff_lload_0",         0),  # 0x1e
    ("jeff_lload_1",         0),  # 0x1f
    ("jeff_lload_2",         0),  # 0x20
    ("jeff_lload_3",         0),  # 0x21
    ("jeff_fload_0",         0),  # 0x22
    ("jeff_fload_1",         0),  # 0x23
    ("jeff_fload_2",         0),  # 0x24
    ("jeff_fload_3",         0),  # 0x25
    ("jeff_dload_0",         0),  # 0x26
    ("jeff_dload_1",         0),  # 0x27
    ("jeff_dload_2",         0),  # 0x28
    ("jeff_dload_3",         0),  # 0x29
    ("jeff_aload_0",         0),  # 0x2a
    ("jeff_aload_1",         0),  # 0x2b
    ("jeff_aload_2",         0),  # 0x2c
    ("jeff_aload_3",         0),  # 0x2d
    ("jeff_iaload",          0),  # 0x2e
    ("jeff_laload",          0),  # 0x2f
    ("jeff_faload",          0),  # 0x30
    ("jeff_daload",          0),  # 0x31
    ("jeff_aaload",          0),  # 0x32
    ("jeff_baload",          0),  # 0x33
    ("jeff_caload",          0),  # 0x34
    ("jeff_saload",          0),  # 0x35
    ("jeff_istore",          1),  # 0x36
    ("jeff_lstore",          1),  # 0x37
    ("jeff_fstore",          1),  # 0x38
    ("jeff_dstore",          1),  # 0x39
    ("jeff_astore",          1),  # 0x3a
    ("jeff_istore_0",        0),  # 0x3b
    ("jeff_istore_1",        0),  # 0x3c
    ("jeff_istore_2",        0),  # 0x3d
    ("jeff_istore_3",        0),  # 0x3e
    ("jeff_lstore_0",        0),  # 0x3f
    ("jeff_lstore_1",        0),  # 0x40
    ("jeff_lstore_2",        0),  # 0x41
    ("jeff_lstore_3",        0),  # 0x42
    ("jeff_fstore_0",        0),  # 0x43
    ("jeff_fstore_1",        0),  # 0x44
    ("jeff_fstore_2",        0),  # 0x45
    ("jeff_fstore_3",        0),  # 0x46
    ("jeff_dstore_0",        0),  # 0x47
    ("jeff_dstore_1",        0),  # 0x48
    ("jeff_dstore_2",        0),  # 0x49
    ("jeff_dstore_3",        0),  # 0x4a
    ("jeff_astore_0",        0),  # 0x4b
    ("jeff_astore_1",        0),  # 0x4c
    ("jeff_astore_2",        0),  # 0x4d
    ("jeff_astore_3",        0),  # 0x4e
    ("jeff_iastore",         0),  # 0x4f
    ("jeff_lastore",         0),  # 0x50
    ("jeff_fastore",         0),  # 0x51
    ("jeff_dastore",         0),  # 0x52
    ("jeff_aastore",         0),  # 0x53
    ("jeff_bastore",         0),  # 0x54
    ("jeff_castore",         0),  # 0x55
    ("jeff_sastore",         0),  # 0x56
    ("jeff_pop",             0),  # 0x57
    ("jeff_pop2",            0),  # 0x58
    ("jeff_dup",             0),  # 0x59
    ("jeff_dup_x1",          0),  # 0x5a
    ("jeff_dup_x2",          0),  # 0x5b
    ("jeff_dup2",            0),  # 0x5c
    ("jeff_dup2_x1",         0),  # 0x5d
    ("jeff_dup2_x2",         0),  # 0x5e
    ("jeff_swap",            0),  # 0x5f
    ("jeff_iadd",            0),  # 0x60
    ("jeff_ladd",            0),  # 0x61
    ("jeff_fadd",            0),  # 0x62
    ("jeff_dadd",            0),  # 0x63
    ("jeff_isub",            0),  # 0x64
    ("jeff_lsub",            0),  # 0x65
    ("jeff_fsub",            0),  # 0x66
    ("jeff_dsub",            0),  # 0x67
    ("jeff_imul",            0),  # 0x68
    ("jeff_lmul",            0),  # 0x69
    ("jeff_fmul",            0),  # 0x6a
    ("jeff_dmul",            0),  # 0x6b
    ("jeff_idiv",            0),  # 0x6c
    ("jeff_ldiv",            0),  # 0x6d
    ("jeff_fdiv",            0),  # 0x6e
    ("jeff_ddiv",            0),  # 0x6f
    ("jeff_irem",            0),  # 0x70
    ("jeff_lrem",            0),  # 0x71
    ("jeff_frem",            0),  # 0x72
    ("jeff_drem",            0),  # 0x73
    ("jeff_ineg",            0),  # 0x74
    ("jeff_lneg",            0),  # 0x75
    ("jeff_fneg",            0),  # 0x76
    ("jeff_dneg",            0),  # 0x77
    ("jeff_ishl",            0),  # 0x78
    ("jeff_lshl",            0),  # 0x79
    ("jeff_ishr",            0),  # 0x7a
    ("jeff_lshr",            0),  # 0x7b
    ("jeff_iushr",           0),  # 0x7c
    ("jeff_lushr",           0),  # 0x7d
    ("jeff_iand",            0),  # 0x7e
    ("jeff_land",            0),  # 0x7f
    ("jeff_ior",             0),  # 0x80
    ("jeff_lor",             0),  # 0x81
    ("jeff_ixor",            0),  # 0x82
    ("jeff_lxor",            0),  # 0x83
    ("jeff_iinc",           -1),  # 0x84
    ("jeff_i2l",             0),  # 0x85
    ("jeff_i2f",             0),  # 0x86
    ("jeff_i2d",             0),  # 0x87
    ("jeff_l2i",             0),  # 0x88
    ("jeff_l2f",             0),  # 0x89
    ("jeff_l2d",             0),  # 0x8a
    ("jeff_f2i",             0),  # 0x8b
    ("jeff_f2l",             0),  # 0x8c
    ("jeff_f2d",             0),  # 0x8d
    ("jeff_d2i",             0),  # 0x8e
    ("jeff_d2l",             0),  # 0x8f
    ("jeff_d2f",             0),  # 0x90
    ("jeff_i2b",             0),  # 0x91
    ("jeff_i2c",             0),  # 0x92
    ("jeff_i2s",             0),  # 0x93
    ("jeff_lcmp",            0),  # 0x94
    ("jeff_fcmpl",           0),  # 0x95
    ("jeff_fcmpg",           0),  # 0x96
    ("jeff_dcmpl",           0),  # 0x97
    ("jeff_dcmpg",           0),  # 0x98
    ("jeff_ifeq",           -1),  # 0x99
    ("jeff_ifne",           -1),  # 0x9a
    ("jeff_iflt",           -1),  # 0x9b
    ("jeff_ifge",           -1),  # 0x9c
    ("jeff_ifgt",           -1),  # 0x9d
    ("jeff_ifle",           -1),  # 0x9e
    ("jeff_if_icmpeq",      -1),  # 0x9f
    ("jeff_if_icmpne",      -1),  # 0xa0
    ("jeff_if_icmplt",      -1),  # 0xa1
    ("jeff_if_icmpge",      -1),  # 0xa2
    ("jeff_if_icmpgt",      -1),  # 0xa3
    ("jeff_if_icmple",      -1),  # 0xa4
    ("jeff_if_acmpeq",      -1),  # 0xa5
    ("jeff_if_acmpne",      -1),  # 0xa6
    ("jeff_goto",           -1),  # 0xa7
    ("jeff_jsr",            -1),  # 0xa8
    ("jeff_ret",             1),  # 0xa9
    ("jeff_tableswitch",    -1),  # 0xaa
    ("jeff_lookupswitch",   -1),  # 0xab
    ("jeff_ireturn",         0),  # 0xac
    ("jeff_lreturn",         0),  # 0xad
    ("jeff_freturn",         0),  # 0xae
    ("jeff_dreturn",         0),  # 0xaf
    ("jeff_areturn",         0),  # 0xb0
    ("jeff_return",          0),  # 0xb1
    ("jeff_getstatic",      -1),  # 0xb2
    ("jeff_putstatic",      -1),  # 0xb3
    ("jeff_getfield",       -1),  # 0xb4
    ("jeff_putfield",       -1),  # 0xb5
    ("jeff_invokevirtual",  -1),  # 0xb6
    ("jeff_invokespecial",  -1),  # 0xb7
    ("jeff_invokestatic",   -1),  # 0xb8
    ("jeff_invokeinterface",-1),  # 0xb9
    ("jeff_unused_0xba",     0),  # 0xba
    ("jeff_new",            -1),  # 0xbb
    ("jeff_newarray",       -1),  # 0xbc
    ("jeff_unused_0xbd",     0),  # 0xbd
    ("jeff_arraylength",     0),  # 0xbe
    ("jeff_athrow",          0),  # 0xbf
    ("jeff_checkcast",      -1),  # 0xc0
    ("jeff_instanceof",     -1),  # 0xc1
    ("jeff_monitorenter",    0),  # 0xc2
    ("jeff_monitorexit",     0),  # 0xc3
    ("jeff_unused_0xc4",     0),  # 0xc4
    ("jeff_multianewarray", -1),  # 0xc5
    ("jeff_ifnull",         -1),  # 0xc6
    ("jeff_ifnonnull",      -1),  # 0xc7
    ("jeff_unused_0xc8",     0),  # 0xc8
    ("jeff_unused_0xc9",     0),  # 0xc9
    ("jeff_breakpoint",      0),  # 0xca
    ("jeff_newconstarray",  -1),  # 0xcb
    ("jeff_slookupswitch",  -1),  # 0xcc
    ("jeff_stableswitch",   -1),  # 0xcd
    ("jeff_ret_w",           2),  # 0xce
    ("jeff_iinc_w",         -1),  # 0xcf
    ("jeff_sldc",           -1),  # 0xd0
    ("jeff_ildc",           -1),  # 0xd1
    ("jeff_lldc",           -1),  # 0xd2
    ("jeff_fldc",           -1),  # 0xd3
    ("jeff_dldc",           -1),  # 0xd4
    ("jeff_dload_w",         2),  # 0xd5
    ("jeff_dstore_w",        2),  # 0xd6
    ("jeff_fload_w",         2),  # 0xd7
    ("jeff_fstore_w",        2),  # 0xd8
    ("jeff_iload_w",         2),  # 0xd9
    ("jeff_istore_w",        2),  # 0xda
    ("jeff_lload_w",         2),  # 0xdb
    ("jeff_lstore_w",        2),  # 0xdc
    ("jeff_aload_w",         2),  # 0xdd
    ("jeff_astore_w",        2),  # 0xde
]

for i in range(len(opcodetbl)):
    oname = opcodetbl[i][0]
    globals()[oname] = i

class BytecodePrinter:
    def __init__(self, jclass, ofcode, codelen):
        self.jeff = jclass.jeff
        self.jclass = jclass
        self.ofcode = ofcode
        self.codelen = codelen

    def seek(self, off):
        self.offset = off

    def align(self, sz):
        mask = sz-1
        check = self.offset + self.jclass.offset
        if check & mask:
            check = (check + mask) & ~mask
            self.offset = check - self.jclass.offset

    def read_8(self):
        self.offset += 1
        return ord(self.f[self.offset-1])

    def read_16(self, aligned = False):
        if self.bigendian:
            fmt = ">H"
        else:
            fmt = "<H"
        if aligned:
            self.align(2)
        r = struct.unpack(fmt, self.f[self.offset:self.offset+2])[0]
        self.offset += 2
        return r

    def read_32(self, aligned = False):
        if self.bigendian:
            fmt = ">I"
        else:
            fmt = "<I"
        if aligned:
            self.align(4)
        r = struct.unpack(fmt, self.f[self.offset:self.offset+4])[0]
        self.offset += 4
        return r

    def read_bytes(self, count):
        r = self.f[self.offset:self.offset+count]
        self.offset += count
        return r

    class JeffInsn:
        def __init__(self, pc, opcode, operands):
            self.pc = pc
            self.opcode = opcode
            self.operands = operands

    def _disasm(self):
        self.jeff.seek(self.jclass.offset + self.ofcode + 8)
        eoff = self.jeff.tell() + self.codelen
        insns = []
        while self.jeff.tell() < eoff:
            pc = self.jeff.tell() - self.jclass.offset
            b = self.jeff.read_8()
            o = opcodetbl[b]
            if o[1] == 1:   # one-byte operand
                opnd = [self.jeff.read_8()]
            elif o[1] == 2: # two-byte operand
                opnd = [self.jeff.read_16()]
            elif o[1] == 0: # no operands
                opnd = []
            elif o[1] == -1:
                if b in [jeff_getfield, jeff_getstatic, jeff_putfield, jeff_putstatic,                    # VMOFFSET ofFieldInfo
                         jeff_invokespecial, jeff_invokevirtual, jeff_invokestatic, jeff_invokeinterface, # VMOFFSET ofMethodInfo
                         jeff_sldc, jeff_ildc, jeff_fldc, jeff_lldc, jeff_dldc,                           # VMOFFSET ofConstant

                         jeff_new,                                                                        # VMOFFSET ofClassIndex
                         jeff_goto, jeff_if_acmpeq, jeff_if_acmpne, jeff_if_icmpeq, jeff_if_icmpne,       # VMOFFSET ofJump
                         jeff_if_icmplt, jeff_if_icmpge, jeff_if_icmpgt, jeff_if_icmple, jeff_ifeq,
                         jeff_ifne, jeff_iflt, jeff_ifge, jeff_ifgt, jeff_ifle, jeff_ifnonnull,
                         jeff_ifnull, jeff_jsr,
                         jeff_sipush,                                                                     # TS2 nValue
                        ]:
                    opnd = [ self.jeff.read_16s(True) ]
                elif b in [jeff_slookupswitch, jeff_lookupswitch]:
                    # VMOFFSET ofDefault
                    # TU2 nPairs
                    # TS2 nMatch [nPairs] | TS4 nMatch [nPairs]
                    # VMOFFSET ofJump [nPairs]
                    ofDefault = self.jeff.read_16(True)
                    nPairs = self.jeff.read_16()
                    if b == jeff_slookupswitch:
                        nMatch = [self.jeff.read_16s() for i in range(nPairs)]
                    else:
                        self.jeff.align(4)
                        nMatch = [self.jeff.read_32s() for i in range(nPairs)]
                    ofJump = [self.jeff.read_16() for i in range(nPairs)]
                    opnd = [ nMatch, ofJump, ofDefault ]
                elif b in [jeff_tableswitch, jeff_stableswitch]:
                    # TU1 <0-1 byte pad>
                    # VMOFFSET ofDefault
                    #                  TU1 <0-2 byte pad>
                    # TS2 nLowValue  | TS4 nLowValue
                    # TS2 nHighValue | TS4 nHighValue
                    # VMOFFSET ofJump [nHighValue - nLowValue + 1]
                    ofDefault = self.jeff.read_16(True)
                    if b == jeff_stableswitch:
                        nLowValue  = self.jeff.read_16s()
                        nHighValue = self.jeff.read_16s()
                    else:
                        nLowValue  = self.jeff.read_32s(True)
                        nHighValue = self.jeff.read_32s()
                    ncases = nHighValue - nLowValue + 1
                    ofJump = [self.jeff.read_16() for i in range(ncases)]
                    opnd = [ nLowValue, ofJump, ofDefault ]
                elif b in [jeff_newarray, jeff_checkcast, jeff_instanceof]:
                    # VMTYPE tDescriptor
                    # TU1 nDimension (optional)
                    # TU1 <0-1 byte pad>
                    # VMOFFSET ofClassIndex (optional)
                    descr = self.jeff.read_descriptor(self.jclass)
                    opnd = [ descr ]
                elif b == jeff_newconstarray:
                    # VMTYPE tArrayType
                    # TU1 <0-1 byte pad>
                    # TU2 nLength
                    # VMOFFSET ofConstData
                    tArrayType = self.jeff.read_8()
                    nLength = self.jeff.read_16(True)
                    ofConstData = self.jeff.read_16()
                    opnd = [ tArrayType, nLength, ofConstData ]
                elif b in [jeff_iinc, jeff_iinc_w]:
                    if b == jeff_iinc:
                        nIndex = self.jeff.read_8()
                        nConstant = self.jeff.read_8s()
                    else:
                        nIndex = self.jeff.read_16(True)
                        nConstant = self.jeff.read_16s()
                    opnd = [nIndex, nConstant]
                else:
                    raise Exception("Unhandled opcode %02X" % b)
            else:
                raise Exception("Unhandled opsize: %d" % o[1])
            insns.append((pc, b, opnd))
        return insns
                 
    def print_jeff(self, indent = 0):
        insns = self._disasm()
        for pc, b, opnd in insns:
            o = opcodetbl[b]
            print " "*indent, "%04X: %02X %s" % (pc, b, o[0]),
            if o[1] == 0:
                print ""
            elif o[1] in [1, 2]:
                v = opnd[0]
                print "%d ; (0x%x)" % (v, v)
                print "%d ; (0x%x)" % (v, v)
            elif o[1] == -1:
                if b in [jeff_getfield, jeff_getstatic, jeff_putfield, jeff_putstatic]:
                    # VMOFFSET ofFieldInfo
                    ofFieldInfo = opnd[0]
                    fidFieldIndex = self.jeff.get_32(self.jclass.offset + ofFieldInfo)
                    ofClassIndex = self.jeff.get_16(self.jclass.offset + ofFieldInfo + 4)
                    cidx = self.jclass.get_index(ofClassIndex)
                    cname = self.jeff.get_classname(cidx)
                    fldname = self.jeff.get_field(fidFieldIndex)
                    print "<class: %s> <field: %s>" % (cname, fldname)
                elif b in [jeff_invokespecial, jeff_invokevirtual, jeff_invokestatic, jeff_invokeinterface]:
                    # VMOFFSET ofMethodInfo
                    ofMethodInfo = opnd[0]
                    midMethodIndex = self.jeff.get_32(self.jclass.offset + ofMethodInfo)
                    ofClassIndex = self.jeff.get_16(self.jclass.offset + ofMethodInfo + 4)
                    cidx = self.jclass.get_index(ofClassIndex)
                    cname = self.jeff.get_classname(cidx)
                    mname = self.jeff.get_method(midMethodIndex)
                    print "<class: %s> <method: %s>" % (cname, mname)
                elif b in [jeff_sldc, jeff_ildc, jeff_fldc, jeff_lldc, jeff_dldc]:
                    # VMOFFSET ofConstant
                    ofConstant = opnd[0]
                    print "o%X=" % ofConstant
                    if b == jeff_sldc:
                        print "<string: '%s'>" % (self.jclass.get_sconst(ofConstant))
                    elif b == jeff_ildc:
                        i = self.jclass.get_iconst(ofConstant)
                        print "<int: %d (0x%X)>" % (i, i&0xFFFFFFFF)
                    elif b == jeff_lldc:
                        i = self.jclass.get_lconst(ofConstant)
                        print "<long: %d (0x%X)>" % (i, i&0xFFFFFFFFFFFFFFFF)
                    elif b == jeff_fldc:
                        f = self.jclass.get_fconst(ofConstant)
                        print "<float: %f>" % (f)
                    elif b == jeff_dldc:
                        f = self.jclass.get_dconst(ofConstant)
                        print "<double: %f>" % (f)
                elif b in [jeff_goto, jeff_if_acmpeq, jeff_if_acmpne,
                           jeff_if_icmpeq, jeff_if_icmpne, jeff_if_icmplt,
                           jeff_if_icmpge, jeff_if_icmpgt, jeff_if_icmple,
                           jeff_ifeq, jeff_ifne, jeff_iflt, jeff_ifge,
                           jeff_ifgt, jeff_ifle, jeff_ifnonnull, jeff_ifnull,
                           jeff_jsr]:
                    # VMOFFSET ofJump
                    ofJump = opnd[0]
                    print "0x%04X" % ofJump
                elif b in [jeff_slookupswitch, jeff_lookupswitch]:
                    nMatch, ofJump, ofDefault = opnd
                    print "\n",
                    print " "*indent, "           default: %04X" % ofDefault
                    for i in range(len(ofJump)):
                        print " "*indent, "          case %3d: %04X" % (nMatch[i], ofJump[i])
                elif b in [jeff_tableswitch, jeff_stableswitch]:
                    nLowValue, ofJump, ofDefault = opnd
                    print "\n",
                    print " "*indent, "           default: %04X" % ofDefault
                    for i in range(len(ofJump)):
                        print " "*indent, "          case %3d: %04X" % (nLowValue+i, ofJump[i])
                elif b in [jeff_newarray, jeff_checkcast, jeff_instanceof]:
                    # VMTYPE tDescriptor
                    # TU1 nDimension (optional)
                    # TU1 <0-1 byte pad>
                    # VMOFFSET ofClassIndex (optional)
                    descr = opnd[0]
                    print "<type: %s>" % descr
                elif b == jeff_new:
                    # VMOFFSET ofClassIndex
                    ofClassIndex = opnd[0]
                    cidx = self.jclass.get_index(ofClassIndex)
                    cname = self.jeff.get_classname(cidx)
                    print "<class: %s>" % (cname)
                elif b == jeff_sipush:
                    # TS2 nValue
                    nValue = opnd[0]
                    print "<%d (0x%X)>" % (nValue, nValue&0xFFFFFFFF)
                elif b == jeff_newconstarray:
                    # VMTYPE tArrayType
                    # TU1 <0-1 byte pad>
                    # TU2 nLength
                    # VMOFFSET ofConstData
                    tArrayType  = opnd[0]
                    nLength     = opnd[1]
                    ofConstData = opnd[2]
                    vals = self.jclass.get_array(tArrayType, ofConstData, nLength)
                    print vals
                elif b in [jeff_iinc, jeff_iinc_w]:
                    nIndex, nConstant = opnd
                    print "%d %d" % (nIndex, nConstant)
                else:
                    print "<unhandled opcode>"
                    break
            else:
                print "<unhandled opsize: %d>" % o[1]
                break

    def dump_consts(self, insns, indent):
        for pc, b, opnd in insns:
            o = opcodetbl[b]
            mnem = o[0]
            if b in [jeff_sldc, jeff_ildc, jeff_fldc, jeff_lldc, jeff_dldc]:
              # VMOFFSET ofConstant
              ofConstant = opnd[0]
              print " "*indent, ".const [o%d] =" % ofConstant,
              if b == jeff_sldc:
                  print 'Utf8 "%s"'% (self.jclass.get_sconst(ofConstant))
              elif b == jeff_ildc:
                  i = self.jclass.get_iconst(ofConstant)
                  print "Int %d ; 0x%X" % (i, i&0xFFFFFFFF)
              elif b == jeff_lldc:
                  i = self.jclass.get_lconst(ofConstant)
                  print "Long  %ldL; 0x%X" % (i, i&0xFFFFFFFFFFFFFFFF)
              elif b == jeff_fldc:
                  f = self.jclass.get_fconst(ofConstant)
                  print "Float %f" % (f)
              elif b == jeff_dldc:
                  f = self.jclass.get_dconst(ofConstant)
                  print "Double %f" % (f)

    def gather_labels(self, insns):
        labels = []
        for pc, b, opnd in insns:
            o = opcodetbl[b]
            mnem = o[0]
            if b in [jeff_goto, jeff_if_acmpeq, jeff_if_acmpne,
                   jeff_if_icmpeq, jeff_if_icmpne, jeff_if_icmplt,
                   jeff_if_icmpge, jeff_if_icmpgt, jeff_if_icmple,
                   jeff_ifeq, jeff_ifne, jeff_iflt, jeff_ifge,
                   jeff_ifgt, jeff_ifle, jeff_ifnonnull, jeff_ifnull,
                   jeff_jsr]:
                # VMOFFSET ofJump
                ofJump = opnd[0]
                labels.append(ofJump)
            elif b in [jeff_slookupswitch, jeff_lookupswitch]:
                nMatch, ofJump, ofDefault = opnd
                for i in range(len(ofJump)):
                    labels.append(ofJump[i])
                labels.append(ofDefault)
            elif b in [jeff_tableswitch, jeff_stableswitch]:
                nLowValue, ofJump, ofDefault = opnd
                for i in range(len(ofJump)):
                    labels.append(ofJump[i])
                labels.append(ofDefault)
        return labels



    def printconstarray(self, tArrayType, ofConstData, nLength, indent):
        #
        #sipush 256
        #newarray int|byte|short|boolean|char|long
        #dup       ;  copy array ref
        #iconst_0  ; index
        #sipush 146 ; value
        #iastore    ; store value
        # dup
        # ...
        #sipush 255
        #bipush 10
        #iastore

        t = tArrayType & 0xF
        arrtyp, values = self.jclass.get_array_values(tArrayType, ofConstData, nLength)
        if arrtyp in ["byte", "char", "boolean"]:
            storeins = "bastore"
        elif arrtyp == "short":
            storeins = "sastore"
        elif arrtyp == "int":
            storeins = "iastore"

        print " "*indent,
        if nLength<=32767:
          print "sipush %d" % nLength
        else:
          print "ldc %d" % nLength
        print " "*indent, "newarray %s" % arrtyp
        for i, v in enumerate(values):
            print " "*indent, "dup"
            if i<=5:
              print " "*indent, "iconst_%d" % i
            elif i<=32767:
              print " "*indent, "sipush %d" % i
            else:
              print " "*indent, "ldc %d" % i
            if -127<=v<=127:
              print " "*indent, "bipush %d" % v
            elif -32768<=v<=32767:
              print " "*indent, "sipush %d" % v
            else:
              print " "*indent, "ldc %d" % v
            print " "*indent, "%s" % storeins



    def print_java(self, indent = 0):
        insns = self._disasm()
        # self.dump_consts(insns, indent)
        labels = self.gather_labels(insns)

        for pc, b, opnd in insns:
            o = opcodetbl[b]
            mnem = o[0]
            if mnem.startswith("jeff_"): mnem = mnem[5:]
            print " "*indent,
            if labels and (pc in labels):
                print "LABEL_%04X:" % pc
                print " "*indent,
            if not b in [jeff_sldc, jeff_ildc, jeff_fldc, jeff_lldc, jeff_dldc,jeff_newconstarray,
                        jeff_tableswitch, jeff_stableswitch, jeff_slookupswitch, jeff_lookupswitch,
                        jeff_newarray, jeff_iinc, jeff_iinc_w]:
                print "%s" % (mnem),
            if o[1] == 0:
                print ""
            elif o[1] in [1, 2]:
                v = opnd[0]
                if b==jeff_bipush and v&0x80:
                  v -= 0x100
                elif b==jeff_sipush and v&0x8000:      
                  v -= 0x10000
                print "%d ; (0x%x)" % (v, opnd[0])
            elif o[1] == -1:
                if b in [jeff_getfield, jeff_getstatic, jeff_putfield, jeff_putstatic]:
                    # VMOFFSET ofFieldInfo
                    ofFieldInfo = opnd[0]
                    fidFieldIndex = self.jeff.get_32(self.jclass.offset + ofFieldInfo)
                    ofClassIndex = self.jeff.get_16(self.jclass.offset + ofFieldInfo + 4)
                    cidx = self.jclass.get_index(ofClassIndex)
                    cname = self.jeff.get_classname(cidx)
                    f  = self.jeff.get_field(fidFieldIndex)
                    print "%s/%s %s" % (cname, f.name, f.descr.get_jtype())
                elif b in [jeff_invokespecial, jeff_invokevirtual, jeff_invokestatic, jeff_invokeinterface]:
                    # VMOFFSET ofMethodInfo
                    ofMethodInfo = opnd[0]
                    midMethodIndex = self.jeff.get_32(self.jclass.offset + ofMethodInfo)
                    ofClassIndex = self.jeff.get_16(self.jclass.offset + ofMethodInfo + 4)
                    cidx = self.jclass.get_index(ofClassIndex)
                    cname = self.jeff.get_javaname(cidx)
                    # mname = self.jeff.get_method(midMethodIndex)
                    md = self.jeff.get_method(midMethodIndex)
                    if  b == jeff_invokeinterface:
                     print "%s/%s%s" % (cname, md.name, md.get_jtypen())
                    else:
                     print "%s/%s%s" % (cname, md.name, md.get_jtype())
                elif b in [jeff_sldc, jeff_ildc, jeff_fldc, jeff_lldc, jeff_dldc]:
                    # VMOFFSET ofConstant
                    ofConstant = opnd[0]
                    if b == jeff_lldc:
                      i = self.jclass.get_lconst(ofConstant)
                      print "ldc2_w %dL" % i
                    elif b == jeff_sldc:
                      #convert string index to offset
                      #ofConstant = self.jclass._soff_by_idx(ofConstant) - self.jclass.offset
                      rs = repr(self.jclass.get_sconst(ofConstant))
                      print 'ldc "%s"' % (rs[1:-1])
                    else:
                      print "ldc [o%d]" % (ofConstant)
                elif b in [jeff_goto, jeff_if_acmpeq, jeff_if_acmpne,
                           jeff_if_icmpeq, jeff_if_icmpne, jeff_if_icmplt,
                           jeff_if_icmpge, jeff_if_icmpgt, jeff_if_icmple,
                           jeff_ifeq, jeff_ifne, jeff_iflt, jeff_ifge,
                           jeff_ifgt, jeff_ifle, jeff_ifnonnull, jeff_ifnull,
                           jeff_jsr]:
                    # VMOFFSET ofJump
                    ofJump = opnd[0]
                    print "LABEL_%04X" % ofJump
                elif b in [jeff_slookupswitch, jeff_lookupswitch]:
                    nMatch, ofJump, ofDefault = opnd
                    print "lookupswitch"
                    for i in range(len(ofJump)):
                        print " "*indent, "          %3d: LABEL_%04X" % (nMatch[i], ofJump[i])
                    print " "*indent, "           default: LABEL_%04X" % ofDefault
                elif b in [jeff_tableswitch, jeff_stableswitch]:
                    nLowValue, ofJump, ofDefault = opnd
                    print "tableswitch %d" % (nLowValue)
                    for i in range(len(ofJump)):
                        print " "*indent, "          LABEL_%04X ; case %d" % (ofJump[i], nLowValue+i)
                    print " "*indent, "           default: LABEL_%04X" % ofDefault
                elif b in [jeff_newarray, jeff_checkcast, jeff_instanceof]:
                    # VMTYPE tDescriptor
                    # TU1 nDimension (optional)
                    # TU1 <0-1 byte pad>
                    # VMOFFSET ofClassIndex (optional)
                    descr = opnd[0]
                    if b == jeff_newarray:
                      if descr.is_object():
                        print "anewarray %s" % descr.get_jtype(True)
                      else:
                       print "newarray %s" % descr.arrtype()
                    else:
                      print descr.get_jtype()
                elif b == jeff_new:
                    # VMOFFSET ofClassIndex
                    ofClassIndex = opnd[0]
                    cidx = self.jclass.get_index(ofClassIndex)
                    cname = self.jeff.get_javaname(cidx)
                    print "%s" % (cname)
                elif b == jeff_sipush:
                    # TS2 nValue
                    nValue = opnd[0]
                    print "%d ; (0x%X)" % (nValue, nValue&0xFFFFFFFF)
                elif b == jeff_newconstarray:
                    # VMTYPE tArrayType
                    # TU1 <0-1 byte pad>
                    # TU2 nLength
                    # VMOFFSET ofConstData
                    tArrayType  = opnd[0]
                    nLength     = opnd[1]
                    ofConstData = opnd[2]
                    self.printconstarray(tArrayType, ofConstData, nLength, indent)
                    # vals = self.jclass.get_array(tArrayType, ofConstData, nLength)
                    # print vals
                elif b in [jeff_iinc, jeff_iinc_w]:
                    nIndex, nConstant = opnd
                    if b == jeff_iinc_w: print "wide",
                    print "iinc %d %d" % (nIndex, nConstant)
                else:
                    print "<unhandled opcode>"
                    break
            else:
                print "<unhandled opsize: %d>" % o[1]
                break
