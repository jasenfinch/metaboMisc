
test_that("tables can be sanitised", {
    n_rows <- 10
    sig_figs <- 1
    max_char <- 3
    
    test_table <- iris %>% 
        mutate(Species = as.character(Species))
    
    x <- sanitiseTable(test_table,
                       maxRows = n_rows,
                       sigFig = sig_figs,
                       maxCharacters = max_char)
    
    expect_equal(nrow(x),n_rows)
    expect_equal(x$Sepal.Length[1],5)
    expect_true(all(str_detect(x$Species,'set...')))
})
