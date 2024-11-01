test_that("Use rates are correctly converted to g/ha", {

  # Halauxifen-methyl in Cerelex in arable crops, e.g. in the PSMV 2024
  use_arable_one_rate_l_ha <- tibble::tibble(
    pNbr = 8550L, use_nr = 1L, substance_de = "Halauxifen-methyl",
    application_area_de = "Feldbau",
    min_dosage = NA,
    max_dosage = NA,
    min_rate = 0.75,
    max_rate = NA,
    units_de = "l/ha",
    percent = 0.68,
    g_per_L = 6.3
  )

  expect_equal(
    application_rate_g_per_ha(use_arable_one_rate_l_ha)$rate_g_per_ha,
    4.725)

  # Seven uses of Alial containing Fosetyl
  uses_fosetyl <- tibble::tibble(
    pNbr = 7877L,
    use_nr = c(1, 2, 3, 4, 5, 6, 7),
    application_area_de = c("Zierpflanzen", "Weinbau", "Obstbau", "Beerenbau", "Gemüsebau", "Gemüsebau", "Zierpflanzen"),
    min_dosage = c(0.5, 0.125, 0.3, 0.75, NA, 0.2, 0.25),
    max_dosage = NA,
    min_rate = c(NA, 2.0, 4.8, 7.5, 2.0, 3.0, NA),
    max_rate = NA,
    units_de = c(NA, rep("kg/ha", 5), NA),
    percent = 80,
    g_per_L = NA)

  expect_equal(
    application_rate_g_per_ha(uses_fosetyl)$rate_g_per_ha,
    c(4000, 1600, 3840, 6000, 1600, 2400, 2000)) # Reference generated in the PAIV indicator project

  # A use of Difcor 250 EC on strawberries
  # sr <- srppphist::srppp_list[["2024"]]
  # sr$products |>
  #   filter(name == "Misto 12") |>
  #   left_join(sr$uses, by = "pNbr") |>
  #   left_join(sr$cultures, by = c("pNbr", "use_nr")) |>
  #   filter(culture_de %in% c("Kernobst")) |>
  #   left_join(sr$ingredients, by = "pNbr", relationship = "many-to-many") |>
  #   filter(type == "ACTIVE_INGREDIENT") |>
  #   select(pNbr, wNbr, use_nr, application_area_de, culture_de,
  #     min_dosage, max_dosage, min_rate, max_rate, units_de, percent, g_per_L)

  uses_Misto_12 <- tibble::tibble(
    pNbr = 1865,
    use_nr = c(2, 4, 7),
    application_area_de = "Obstbau",
    culture_de = "Kernobst",
    min_dosage = c(2, 1, 3.5),
    max_dosage = NA,
    min_rate = NA, # 32, 16, 56, to be reproduced as intermediate result
    max_rate = NA,
    units_de = NA,
    percent = 99.1,
    g_per_L = 830)

  expect_equal(
    application_rate_g_per_ha(uses_Misto_12)$rate_g_per_ha,
    c(32, 16, 56) * 830)

})
