test_that("We get dm objects with relational integrity", {

  expect_message(
    print(dm::dm_examine_constraints(srppp_cur, .progress = FALSE)),
    "All constraints satisfied")

  expect_message(
    print(dm::dm_examine_constraints(srppp_test, .progress = FALSE)),
    "All constraints satisfied")

})
