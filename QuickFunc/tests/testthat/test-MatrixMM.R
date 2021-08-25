test_that("MatrixMM Correctly takes Maximum or Minimum, Rows or Columns", {

  X <- matrix(c(7,3,5,4,-2,9), nrow = 2)

  expect_equal( MatrixMM( X, maximum = TRUE, column = TRUE ), c(7,5,9) )
  expect_equal( MatrixMM( X, maximum = FALSE, column = TRUE ), c(3,4,-2) )
  expect_equal( MatrixMM( X, maximum = TRUE, column = FALSE ), c(7,9) )
  expect_equal( MatrixMM( X, maximum = FALSE, column = FALSE ), c(-2,3) )

})
