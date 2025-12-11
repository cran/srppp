# Read in the current SRPPP from the XML URL
# Interception of the system message by sink() was taken from https://stackoverflow.com/a/66139071
nullcon <- file(nullfile(), open = "wb")
sink(nullcon, type = "message")
srppp_cur <- srppp_dm()
sink(type = "message")
close(nullcon)

# Read in the test datasets included in the package
test_data_1 <- system.file("testdata/Daten_Pflanzenschutzmittelverzeichnis_2024-12-16.zip",
  package = "srppp")
srppp_test_xml_1 <- srppp_xml_get_from_path(test_data_1, from = "2024-12-16")
srppp_test_1 <- srppp_dm(srppp_test_xml_1)

test_data_2 <- system.file("testdata/PublicationData_2025_12_01.xml.xz",
  package = "srppp")
srppp_test_xml_2 <- srppp_xml_get_from_path(test_data_2, from = "2025-12-01")
srppp_test_2 <- srppp_dm(srppp_test_xml_2)
