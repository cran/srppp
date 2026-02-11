test_that("Resolving cultures to highest detail works", {

  example_dataset_1 <- data.frame(
   substance_de = c("Spirotetramat", "Spirotetramat", "Spirotetramat", "Spirotetramat"),
   pNbr = c(7839, 7839, 7839, 7839),
   use_nr = c(5, 7, 18, 22),
   application_area_de = c("Obstbau", "Obstbau", "Obstbau", "Obstbau"),
   culture_de = c("Birne", "Kirsche", "Steinobst", "Kernobst"),
   pest_de = c("Birnblattsauger", "Kirschenfliege", "Blattläuse (Röhrenläuse)", "Spinnmilben"))

  result_1 <- resolve_cultures(example_dataset_1, srppp_test_1)

  expect_setequal(result_1$leaf_culture_de,
    c("Birne", "Kirsche",
      "Aprikose", "Kirsche", "Pfirsich / Nektarine", "Pflaume", "Zwetschge",
      "Apfel", "Quitte", "Birne"))

  # Repeat with test data in the updated format
  result_1_2 <- resolve_cultures(example_dataset_1, srppp_test_2)

  expect_setequal(result_1_2$leaf_culture_de,
    c("Birne", "Kirsche",
      "Aprikose", "Kirsche", "Pfirsich / Nektarine", "Pflaume", "Zwetschge",
      "Apfel", "Quitte", "Birne"))

  # Same as above, but with "Kirschen" instead of "Kirsche"
  example_dataset_2 <- data.frame(
    substance_de = c("Spirotetramat", "Spirotetramat", "Spirotetramat", "Spirotetramat"),
    pNbr = c(7839, 7839, 7839, 7839),
    use_nr = c(5, 7, 18, 22),
    application_area_de = c("Obstbau", "Obstbau", "Obstbau", "Obstbau"),
    culture_de = c("Birne", "Kirschen", "Steinobst", "Kernobst"),
      pest_de = c("Birnblattsauger", "Kirschenfliege", "Blattläuse (Röhrenläuse)", "Spinnmilben"))

  result_2 <- resolve_cultures(example_dataset_2, srppp_test_1)

  expect_setequal(result_2$leaf_culture_de,
    c("Birne", NA,
      "Aprikose", "Kirsche", "Pfirsich / Nektarine", "Pflaume", "Zwetschge",
      "Apfel", "Quitte", "Birne"))

  # Repeat with test data in the updated format
  result_2_2 <- resolve_cultures(example_dataset_2, srppp_test_2)

  expect_setequal(result_2_2$leaf_culture_de,
    c("Birne", NA,
      "Aprikose", "Kirsche", "Pfirsich / Nektarine", "Pflaume", "Zwetschge",
      "Apfel", "Quitte", "Birne"))

   # Example showing how cereals "Getreide" are resolved
   example_dataset_3 <- data.frame(
     substance_de = c("Pirimicarb"),
     pNbr = c(2210),
     use_nr = c(3),
     application_area_de = c("Feldbau"),
     culture_de = c("Getreide"),
     pest_de = c("Blattläuse (Röhrenläuse)") )

  result_3 <- resolve_cultures(example_dataset_3, srppp_test_1)
  expect_equal(nrow(result_3), 10)

  # Repeat with test data in the updated format
  result_3_2 <- resolve_cultures(example_dataset_3, srppp_test_2)
  expect_equal(nrow(result_3), 10)

  # Example resolving ornamental plants ("Zierpflanzen")
  example_dataset_4 <- data.frame(substance_de = c("Metaldehyd"),
    pNbr = 6142, use_nr = 1, application_area_de = c("Zierpflanzen"),
    culture_de = c("Zierpflanzen allg."), pest_de = c("Ackerschnecken/Deroceras Arten") )

  result_4 <- resolve_cultures(example_dataset_4, srppp_test_1)
  expect_equal(nrow(result_4), 28)

  # Repeat with test data in the updated format
  result_4_2 <- resolve_cultures(example_dataset_4, srppp_test_2)
  expect_equal(nrow(result_4_2), 30)

  # Illustrate the resolution of the culture "allg."
  example_dataset_5 <- data.frame(
    substance_de = c("Kupfer (als Oxychlorid)","Metaldehyd","Metaldehyd","Schwefel"),
    pNbr = c(585,1090,1090,38),
    use_nr = c(12,4,4,1),
    application_area_de = c("Weinbau","Obstbau","Obstbau","Beerenbau"),
    culture_de = c("allg.","allg.","allg.","Brombeere"),
    pest_de = c("Graufäule (Botrytis cinerea)","Wegschnecken/Arion Arten",
      "Wegschnecken/Arion Arten","Gallmilben"))

  results_5a <- resolve_cultures(example_dataset_5, srppp_test_1,
    resolve_culture_allg = FALSE)
  expect_equal(results_5a$leaf_culture_de, c(NA, NA, NA, "Brombeere"))
  results_5b <- resolve_cultures(example_dataset_5, srppp_test_1,
    resolve_culture_allg = TRUE)
  expect_equal(nrow(results_5b), 23)

  # Repeat with new format
  results_5a_2 <- resolve_cultures(example_dataset_5, srppp_test_2,
      resolve_culture_allg = FALSE)
  expect_equal(results_5a_2$leaf_culture_de, c(NA, NA, NA, "Brombeere"))
  results_5b_2 <- resolve_cultures(example_dataset_5, srppp_test_2,
    resolve_culture_allg = TRUE)
  expect_equal(nrow(results_5b_2), 23)

  # Illustrate the resolution of "Obstbau allg.", which does not have children in
  # the XML files, but which should have children, because Obstbau allg. is
  # not a leaf culture.
  example_dataset_6 <- data.frame(
    substance_de = c("Schwefel"),
    pNbr = c(3561),
    use_nr = c(4),
    application_area_de = c("Obstbau"),
    culture_de = c("Obstbau allg."),
    pest_de = c("Wühl- oder Schermaus") )

   result_6a <- resolve_cultures(example_dataset_6, srppp_test_1,
     correct_culture_names = FALSE)
   expect_equal(result_6a$leaf_culture_de, NA_character_)
   result_6b <- resolve_cultures(example_dataset_6, srppp_test_1,
     correct_culture_names = TRUE)
   expect_equal(nrow(result_6b), 10)

   # Repeat test with the new format
   result_6a_2 <- resolve_cultures(example_dataset_6, srppp_test_2,
     correct_culture_names = FALSE)
   expect_equal(result_6a_2$leaf_culture_de, NA_character_)
   result_6b_2 <- resolve_cultures(example_dataset_6, srppp_test_2,
     correct_culture_names = TRUE)
   expect_equal(nrow(result_6b_2), 10)

})

