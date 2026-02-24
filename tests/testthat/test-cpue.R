test_that("cpue calculates simple ratio correctly", {
  expect_equal(cpue(catch = 100, effort = 10), 10)
  expect_equal(cpue(catch = 50, effort = 2), 25)
})

test_that("cpue handles vectors of data", {
  catches <- c(100, 200, 300)
  efforts <- c(10, 10, 10)
  expected_results <- c(10, 20, 30)

  expect_equal(cpue(catches, efforts), expected_results)
})

test_that("gear_factor standardization scales correctly", {
  expect_equal(cpue(catch = 100, effort = 10, gear_factor = 0.5), 5)
  expect_equal(
    cpue(catch = 100, effort = 10),
    cpue(catch = 100, effort = 10, gear_factor = 1)
  )
})

test_that("cpue handles zero catch and missing data", {
  expect_equal(cpue(catch = 0, effort = 10), 0)

  expect_true(is.na(cpue(NA_real_, 10)))
  expect_true(is.na(cpue(100, NA_real_)))
})

test_that("cpue works with generated data", {
  data <- generate_fishing_data(n = 5)

  result <- cpue(data$catch, data$effort)

  expect_equal(
    result,
    c(34.053, 9.065, 19.239, 135.640, 6.372),
    tolerance = 1e-3
  )
})

test_that("cpue matches reference data", {
  result <- cpue(reference_data$catch, reference_data$effort)

  expect_equal(result, reference_data$expected_cpue)
})

test_that("cpue returns numeric vector", {
  result <- cpue(c(100, 200), c(10, 20))

  expect_type(result, "double")
  expect_length(result, 2)
})

test_that("cpue provides informative message when verbose", {
  expect_snapshot(
    cpue(c(100, 200), c(10, 20), verbose = TRUE)
  )
})

test_that("cpue is silent by default", {
  expect_no_message(cpue(100, 10))
})

test_that("cpue warns when vector lengths don't match", {
  expect_warning(
    cpue(catch = c(100, 200, 300), effort = c(10, 20)),
    "longer object length is not a multiple of shorter object length"
  )

  expect_no_warning(cpue(100, 10))
})

test_that("cpue error message is informative", {
  expect_snapshot(
    cpue("not a number", 10),
    error = TRUE
  )
})

test_that("cpue produces no warnings with valid input", {
  expect_snapshot(
    cpue(catch = c(100, 200, 300), effort = c(10, 20))
  )

  expect_no_warning(cpue(100, 10))
})
