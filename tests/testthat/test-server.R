context("app")

testServer(expr = {
  session$setInputs(type = "Cargo")
  session$setInputs(vessel = "3615")
  
  expect_equal(output$distance, "The vessel travelled 14876 meters")
  expect_equal(
    output$dates,
    "The vessel travelled between 2016-12-13 22:31:02 and 2016-12-15 16:49:02"
  )
  
  session$setInputs(type = "Fishing")
  session$setInputs(vessel = "315903")
  
  expect_equal(output$distance, "The vessel travelled 5187 meters")
  expect_equal(
    output$dates,
    "The vessel travelled between 2016-12-15 12:49:02 and 2016-12-17 04:31:02"
  )
  
  session$setInputs(vessel = "315903")
  vessel1 <-
    structure(
      list(
        ship_type = "Fishing",
        SHIP_ID = "315903",
        DATETIME = structure(
          1481949062,
          tzone = "UTC",
          class = c("POSIXct",
                    "POSIXt")
        ),
        LAT = 54.79923,
        LON = 18.95095,
        prevLON = 18.87031,
        prevLAT = 54.79855,
        prevDATE = structure(
          1481806142,
          tzone = "UTC",
          class = c("POSIXct",
                    "POSIXt")
        ),
        dist = 5187
      ),
      row.names = c(NA, -1L),
      groups = structure(
        list(
          SHIP_ID = "315903",
          .rows = structure(
            list(1L),
            ptype = integer(0),
            class = c("vctrs_list_of",
                      "vctrs_vctr", "list")
          )
        ),
        row.names = c(NA, -1L),
        class = c("tbl_df",
                  "tbl", "data.frame"),
        .drop = TRUE
      ),
      class = c("grouped_df",
                "tbl_df", "tbl", "data.frame")
    )
  
  expect_equal(get_vessel_data(), vessel1)
  
  
  session$setInputs(vessel = "3615")
  vessel2 <-
    structure(
      list(
        ship_type = "Cargo",
        SHIP_ID = "3615",
        DATETIME = structure(
          1481820542,
          tzone = "UTC",
          class = c("POSIXct",
                    "POSIXt")
        ),
        LAT = 54.79053,
        LON = 18.89155,
        prevLON = 18.99983,
        prevLAT = 54.6725,
        prevDATE = structure(
          1481668262,
          tzone = "UTC",
          class = c("POSIXct",
                    "POSIXt")
        ),
        dist = 14876
      ),
      row.names = c(NA, -1L),
      groups = structure(
        list(
          SHIP_ID = "3615",
          .rows = structure(
            list(1L),
            ptype = integer(0),
            class = c("vctrs_list_of",
                      "vctrs_vctr", "list")
          )
        ),
        row.names = c(NA, -1L),
        class = c("tbl_df",
                  "tbl", "data.frame"),
        .drop = TRUE
      ),
      class = c("grouped_df",
                "tbl_df", "tbl", "data.frame")
    )
  
  
  expect_equal(get_vessel_data(), vessel2)
  
  
  
  
})
