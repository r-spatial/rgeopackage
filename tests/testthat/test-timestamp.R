test_that("amend_timestamp works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  withr::with_tempfile(
    "nc.gpkg",
    {
      sf::write_sf(nc, "nc.gpkg")
      expect_message(
        amend_timestamp("nc.gpkg"),
        "gpkg_contents table have been set with timestamp"
      )
    }
  )
})
