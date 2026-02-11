test_that("Alternative products are found", {

  lambda_delta_gaps <- alternative_products(srppp_test_1, c("Lambda-Cyhalothrin", "Deltamethrin"),
    missing = TRUE, resolve_cultures = FALSE)
  expect_equal(nrow(lambda_delta_gaps), 128L)

  lambda_delta_gaps_2 <- alternative_products(srppp_test_2, c("Lambda-Cyhalothrin", "Deltamethrin"),
    missing = TRUE, resolve_cultures = FALSE)
  expect_equal(nrow(lambda_delta_gaps_2), 166L)

  lambda_delta_gaps_resolved <- alternative_products(srppp_test_1, c("Lambda-Cyhalothrin", "Deltamethrin"),
    missing = TRUE, resolve_cultures = TRUE)
  expect_equal(nrow(lambda_delta_gaps_resolved), 110L)

  # Repeat with new format
  lambda_delta_gaps_resolved_2 <- alternative_products(srppp_test_2, c("Lambda-Cyhalothrin", "Deltamethrin"),
    missing = TRUE, resolve_cultures = TRUE)
  expect_equal(nrow(lambda_delta_gaps_resolved_2), 110L)

  lambda_delta_gaps_it <- alternative_products(srppp_test_1, c("Lambda-Cialotrina", "Deltametrina"),
    missing = TRUE, lang = "it", resolve_cultures = FALSE)
  expect_equal(nrow(lambda_delta_gaps_it), 128L)

  expect_error(alternative_products(srppp_test_1, c("Lambda-Cialotrina", "Deltametrina"),
    missing = TRUE, lang = "it", resolve_cultures = TRUE))

  expect_error(alternative_products(srppp_test_1, c("test"), lang = "at"))
})
