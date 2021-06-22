# test if working directory is not Documents
test_that("The working directory should not be Documents", {
    expect_false("~" == getwd())
})
