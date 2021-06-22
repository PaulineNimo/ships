# test if csv file exists
test_that("The csv file with distances in meters exists", {
    expect_true(file.exists(paste0(getwd(),"/data/ship_data_distance.zip")))
})
