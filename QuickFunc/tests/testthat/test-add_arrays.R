test_that("A and B are correctly added", {

  A <- array(1:24, dim = c(2,3,4))
  B <- matrix(1:8, nrow = 2)
  C <- 1:3
  v <- c(2,4,4,6,6,8,10,12,12,14,14,16,18,20,20,22,22,24,26,28,28,30,30,32)
  w <- c(2,3,5,6,8,9,8,9,11,12,14,15,14,15,17,18,20,21,20,21,23,24,26,27)

  # Check changing d for adding A and B
  expect_error( add_arrays( A, B) )
  expect_error( add_arrays( A, B, d = 1 ) )
  expect_equal( add_arrays( A, B, d = 2 ), array( v, dim = c(2,3,4) ) )
  expect_error( add_arrays( A, B, d = 3 ) )
  expect_error( add_arrays( A, B, d = 4 ) )
  expect_error( add_arrays( B, A ) )
  expect_error( add_arrays( B, A, d = 1 ) )
  expect_error( add_arrays( B, A, d = 2 ) )
  expect_error( add_arrays( B, A, d = 3 ) )

  # Check the use of a vector C with both A and B.
  expect_equal( add_arrays( A, C, d = c(1,3) ), array( w, dim = c(2,3,4) ) )
  expect_error( add_arrays( B, C ) )
  expect_error( add_arrays( C, A ) )

})
