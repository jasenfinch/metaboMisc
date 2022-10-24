test_that("reduce works", {
    assigned_data <- metabolyseR::analysisData(
        assignedData(assignment),
        tibble::tibble(sample = seq_len(nrow(assignments::feature_data)))
    ) 
    reduced_data <- reduce(assigned_data)
    
    expect_s4_class(reduced_data,'AnalysisData')
})
