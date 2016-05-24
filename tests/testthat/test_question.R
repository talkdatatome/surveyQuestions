library(stringr)
context("Testing generic question methods.")

test_that("Adding data default appends new row", {
    x <- question(1, "default question", list("default 1", "default 2"))
    expect_true(is.null(x$data))
    x <- addResult(x, 1, 1)
    expect_equal(dim(x$data), c(1,2))
})

test_that("Adding an answer outside answerText length errors", {
    x <- question(1, "default question", list("default 1", "default 2"))
    expect_error(addResult(x, 1, 5)) 
})
