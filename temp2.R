## add just tables stuff to new xml
xml_new = xml_new_document()

doc = XML::newXMLDoc()
root = XML::newXMLNode("session", doc = doc)

write_xml(xml_new, file='web/just_tables.xml', encoding  = "UTF-8")


xml_new %>% xml_add_child(inside_tables)

xml_new_root(xml_new) %>% xml_add_child(inside_tables)
XML::saveXML(xmlDoc, "test.xml")

xml_add_parent()
xml2::write_xml(inside_tables, file='web/just_tables.xml', encoding = "UTF-8")



xml2::write_xml(inside_tables, file='web/just_tables.xml', encoding = "UTF-8")


write.table(inside_tables, file='web/just_tables.xml', fileEncoding  = "UTF-8")


XML::saveXML(inside_tables, file='web/just_tables.xml')
