# Trying to use xml.etree.ElemTree which is insecure: https://docs.python.org/3/library/xml.etree.elementtree.html
# import xml.etree.ElementTree as ET

billion_laughs = input()
root = xml.etree.ElementTree.XML(billion_laughs)
# root = tree.getroot()

