
temp_dir <- tempdir()

test_that('exportCSV works',{
    
    file_path <- paste0(temp_dir,'/iris.csv')
    exported_file_path <- exportCSV(iris,file_path)
    
    expect_match(exported_file_path,file_path)
    expect_snapshot_file(exported_file_path)
})

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
