
test_that("tables can be sanitised", {
    n_rows <- 10
    sig_figs <- 1
    
    x <- sanitiseTable(iris,maxRows = n_rows,sigFig = sig_figs)
    
    expect_equal(nrow(x),n_rows)
    expect_equal(x$Sepal.Length[1],5)
})
