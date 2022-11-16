test_that("read_gpkg_contents works", {
  expect_s3_class(
    read_gpkg_contents("inst/extdata/empty.gpkg"),
    "data.frame"
  )
})
