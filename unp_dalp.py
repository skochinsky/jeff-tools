#unpack applets from a  .dalp XML file
import sys, os.path

import xml.etree.ElementTree as ET
infn = sys.argv[1]
tree = ET.parse(infn)
root = tree.getroot()
name = root.find('packageInfo').find('name').text
if not name:
    name = os.path.splitext(os.path.basename(infn))[0]

for applet in root.find('applets'):
    ver = applet.find('appletVersion').text
    fwver = applet.find('fwVersion').text
    blob = applet.find('appletBlob').text
    platf = applet.find('platform').text
    print "Applet: v%s for %s fw v%s" % (ver, platf, fwver)
    open("%s_%s_%s_v%s.bin" % (name, platf, fwver, ver), "wb").write(blob.decode('base64'))
