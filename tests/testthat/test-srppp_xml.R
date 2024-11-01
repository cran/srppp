test_that("We get a dm object with relational integrity", {

  # Read in the current SRPPP from the XML URL
  # Interception of the system message by sink() was taken from https://stackoverflow.com/a/66139071
  nullcon <- file(nullfile(), open = "wb")
  sink(nullcon, type = "message")
  srppp_cur <- srppp_dm()
  sink(type = "message")
  close(nullcon)

  expect_message(
    print(dm::dm_examine_constraints(srppp_cur, .progress = FALSE)),
    "All constraints satisfied")
})
