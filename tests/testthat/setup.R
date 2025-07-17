# Read in the current SRPPP from the XML URL
# Interception of the system message by sink() was taken from https://stackoverflow.com/a/66139071
nullcon <- file(nullfile(), open = "wb")
sink(nullcon, type = "message")
srppp_cur <- srppp_dm()
sink(type = "message")
close(nullcon)

# Read in the test dataset included in the package
test_data <- system.file("testdata/Daten_Pflanzenschutzmittelverzeichnis_2024-12-16.zip",
  package = "srppp")
srppp_test_xml <- srppp_xml_get_from_path(test_data, from = "2024-12-16")
srppp_test <- srppp_dm(srppp_test_xml)
