# test if entire file is loaded
test_that("Number of rows are the same for the two files", {
    expect_equal(nrow(read_csv("data/ship data distance.csv")), nrow(read_csv("data/ships_04112020.zip")))
})
