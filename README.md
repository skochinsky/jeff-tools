dump_jeff.py
========

This script allows you to dump and the JEFF files used by Intel ME's DAL (Dynamic Application Loader).
It supports the following input formats:
  - raw JEFF file ('JEFF signature')
  - JEFF packaged as an ME applet with signed manifest header ($MN2 magic) (currently ME 8-9.5 only)
  - any binary containing an uncompressed JEFF file inside (e.g. JOM_mod.bin produced by unpacking an ME firmware)

Usage: dump_jeff.py [options] file.jeff >output.txt
 Where options are:
 -d: dump JEFF structure (default).
 This dumps the detailed structure of the JEFF file, including classes, fields and methods with disassembly. For the description
 of the JEFF format and instructions, see the JEFF standard documens in the docs/ directory.
 -s: print class definition summary.
 Prints only a list of classes, fields (with offsets) and methods
 -j: print Jasmin-style disassembly (experimental)
 Prints the JEFF file as a Jasmin-style Java assembly (jeff instructions are transformed into equivalent Java ones). 
 The result can be directly assembled using Krakatau into .class files (and then decompiled).
 
unp_dalp.py
========

This script unpacks DAL applets from a .dalp file (such files are used by Intel to package several ME applets into one XML).

