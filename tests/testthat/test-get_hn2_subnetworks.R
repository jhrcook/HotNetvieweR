test_that("Retrieval of HN2 results works correctly.", {

    # pass a directory that does not exist
    fake_dir <- file.path("doesNot", "exists")
    expect_error(get_hn2_subnetworks(fake_dir),
                 "is not accessible or does not exist")

})
