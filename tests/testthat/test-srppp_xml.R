test_that("We get dm objects with relational integrity", {

  expect_message(
    print(dm::dm_examine_constraints(srppp_test_1, .progress = FALSE)),
    "All constraints satisfied")

  constraints_test_2 <- dm::dm_examine_constraints(srppp_test_2, .progress = FALSE) |>
    as_tibble()

  # Count the number of keys that failed a constraint check
  n_failed <- constraints_test_2 |>
    filter(!is_key) |>
    nrow()
  expect_equal(n_failed, 1)

  # We know there is a problem with the version 2 test data, as three reference
  # products parallel imports are missing
  expect_equal(
    constraints_test_2[[1, "problem"]],
    "values of `parallel_imports$pNbr` not in `pNbrs$pNbr`: 7738 (5), 8332 (1), 9033 (1)")

})

test_that("The substance type of ingredients is correctly obtained", {
  expect_true(
    all(srppp_test_1$ingredients$type %in%
        c("ACTIVE_INGREDIENT", "ADDITIVE_TO_DECLARE", "SAFENER", "SYNERGIST")))

  # In version 2.0.0, the type was not read in correctly, now fixed
  expect_true(
    all(srppp_test_2$ingredients$type %in%
        c("ACTIVE_INGREDIENT", "ADDITIVE_TO_DECLARE", "SAFENER", "SYNERGIST")))
})

test_that("We get dm objects with relational integrity from the srppp URL", {
  # Skip on CRAN to avoid timeouts. Downloading the file does not have to be 
  # tested on so many platforms
  skip_on_cran()
  skip_on_ci()

  srppp_cur <- srppp_dm()

  constraints_test_cur <- dm::dm_examine_constraints(srppp_cur, .progress = FALSE)

  # Count the number of keys that failed a constraint check
  n_failed_cur <- constraints_test_cur |>
    filter(!is_key) |>
    nrow()
  expect_equal(n_failed_cur, 1)

})
