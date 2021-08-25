test_that("ab() produces matrix if input is matrix", {

  X <- matrix(1:210, ncol = 15)

  expect_equal(is.matrix(ab(X)), TRUE)

})

test_that("ab() produces dataframe if input is dataframe", {

  X <- data.frame(matrix(1:210, ncol = 15))

  expect_equal(is.data.frame(ab(X)), TRUE)

})
