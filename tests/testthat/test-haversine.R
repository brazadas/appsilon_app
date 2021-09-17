test_that("haversine distance is well calculated", {
  
  lng0 <- 14.99361
  lng1 <- 18.99692
  lat0 <- 57.76542
  lat1 <- 54.77127
  a0 <- (sin((lat1 - lat0) / 2) ^ 2) + (cos(lat0) * cos(lat1) * (sin((lng1 - lng0) / 2) ^ 2))
  a1 <- 2 * atan2(a0 ^ (1/2), (1 - a0) ^ (1/2))
  a2 <- 6378000 * a1
  expect_equal(a2, 16770976)
  
})






