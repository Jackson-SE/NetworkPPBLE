test_that("test AVEM", {
  A <- array(1:24, dim = c(2,3,4))
  b = 1:4
  expect_equal( AVEM( A, b ), array( c((1:6)*1, (7:12)*2, (13:18)*3, (19:24)*4), dim = dim( A ) ) )
  expect_equal( AVEM( A[,,2], b[2:4] ), array( c(14,16,27,30,44,48), dim = dim( A[,,2] ) ) )
  expect_error( AVEM( A, A ) )
  expect_error( AVEM( b, b ) )
})
