
temp_dir <- tempdir()

test_that("export Binalysis works", {
    fp <- export(bd,temp_dir)
    
    expect_identical(basename(fp),c('sample_information.csv',
                                    'accurate_data.csv',
                                    'negative_mode_processed_data.csv',
                                    'positive_mode_processed_data.csv'))
})

test_that("export MetaboProfile works", {
    
    fp <- export(lcd,temp_dir)
    
    expect_identical(basename(fp),c('sample_information.csv',
                                    'peak_info.csv',
                                    'processed_data.csv'))
})

test_that("export Analysis works", {
    fp <- export(a,temp_dir,type = 'pre-treated')
    
    expect_identical(basename(fp),c('pre-treated_sample_information.csv',
                                    'pre-treated_data.csv',
                                    'modelling_performance_metrics.csv',
                                    'modelling_importance_metrics.csv',
                                    'correlations.csv'))
})

test_that("export Assignment works", {
    fp <- export(assignment,temp_dir)
    
    expect_identical(basename(fp),c('assignments.csv',
                                    'assigned_data.csv',
                                    'summarised_assignments.csv'))
})

test_that("export Construction works", {
    fp <- export(structural_classifications,temp_dir)
    
    expect_identical(basename(fp),c('consensus_structural_classifications.csv',
                                    'summarised_consensus_structural_classifications.csv'))
})