# test for the function euclidean()
context("euclidean")

test_that("User Input Validation",{
 expect_error(euclidean("b", 100))
}
)

test_that("Function Validation",{
	expect_equal(euclidean(100,1000),100)
}
)
