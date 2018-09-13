# test for the function dijkstra()
context("Dijkstras")

test_that("User Input Validation",{
 expect_error(dijkstra("b", 1))
}
)

