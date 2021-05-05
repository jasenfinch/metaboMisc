
temp_dir <- tempdir()

test_that('exportCSV works',{
    
    file_path <- exportCSV(iris,'iris.csv')
    
    expect_match(file_path,'iris.csv')
    expect_snapshot_file('iris.csv',binary = FALSE)
})

test_that("export Binalysis works", {
    fp <- export(bd,temp_dir)
    
    expect_identical(basename(fp),c('sample_information.csv',
                                    'accurate_data.csv',
                                    'negative_mode_processed_data.csv',
                                    'positive_mode_processed_data.csv'))
})

test_that("export MetaboProfile works", {
    skip('Skip tests involving MetaboProfile class')
    
    fp <- export(lcd,temp_dir)
    
    expect_identical(basename(fp),c('sample_information.csv',
                                    'peak_info.csv',
                                    '/1_mode_processed_data.csv'))
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
