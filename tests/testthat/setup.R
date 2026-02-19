# Read in the test datasets included in the package
test_data_1 <- system.file("testdata/Daten_Pflanzenschutzmittelverzeichnis_2024-12-16.zip",
  package = "srppp")
srppp_test_xml_1 <- srppp_xml_get_from_path(test_data_1, from = "2024-12-16")
srppp_test_1 <- srppp_dm(srppp_test_xml_1)

test_data_2 <- system.file("testdata/PublicationData_2026_02_03.xml.xz",
  package = "srppp")
srppp_test_xml_2 <- srppp_xml_get_from_path(test_data_2, from = "2026-02-03")
srppp_test_2 <- srppp_dm(srppp_test_xml_2)
