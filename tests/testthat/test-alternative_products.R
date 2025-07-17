test_that("Alternative products are found", {

  lambda_delta_gaps <- alternative_products(srppp_test, c("Lambda-Cyhalothrin", "Deltamethrin"),
    missing = TRUE, resolve_cultures = FALSE)
  expect_equal(nrow(lambda_delta_gaps), 128L)

  lambda_delta_gaps_resolved <- alternative_products(srppp_test, c("Lambda-Cyhalothrin", "Deltamethrin"),
    missing = TRUE, resolve_cultures = TRUE)
  expect_equal(nrow(lambda_delta_gaps_resolved), 110L)

  lambda_delta_gaps_it <- alternative_products(srppp_test, c("Lambda-Cialotrina", "Deltametrina"),
    missing = TRUE, lang = "it", resolve_cultures = FALSE)
  expect_equal(nrow(lambda_delta_gaps_it), 128L)

  expect_error(alternative_products(srppp_test, c("Lambda-Cialotrina", "Deltametrina"),
    missing = TRUE, lang = "it", resolve_cultures = TRUE))

  expect_error(alternative_products(srppp_test, c("test"), lang = "at"))
})
