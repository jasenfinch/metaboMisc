test_that("plotRSD works for Binalysis class", {
    pl <- plotRSD(bd)
    
    expect_type(pl,'list')
})

test_that("plotRSD works for MetaboProfile class", {
    skip('Skip tests involving MetaboProfile class')
    
    pl <- plotRSD(lcd)
    
    expect_type(pl,'list')
})
