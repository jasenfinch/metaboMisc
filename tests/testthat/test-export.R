
temp_dir <- tempdir()

test_that("export Binalysis works", {
    export(bd,temp_dir)
    
    expect_true(file.exists(str_c(temp_dir,'/exports/negative_mode_raw_data.csv')))
    expect_true(file.exists(str_c(temp_dir,'/exports/positive_mode_raw_data.csv')))
    expect_true(file.exists(str_c(temp_dir,'/exports/raw_sample_info.csv')))
})

test_that("export MetaboProfile works", {
    skip('Skip tests involving MetaboProfile class')
    
    export(lcd,temp_dir)
    
    expect_true(file.exists(str_c(temp_dir,'/exports/1_mode_raw_data.csv')))
    expect_true(file.exists(str_c(temp_dir,'/exports/raw_sample_info.csv')))
})

test_that("export Analysis works", {
    export(a,temp_dir)
    
    expect_true(file.exists(str_c(temp_dir,'/exports/pre-treated_data.csv')))
    expect_true(file.exists(str_c(temp_dir,'/exports/pre-treated_sample_info.csv')))
})

test_that("export Assignment works", {
    export(assignment,temp_dir)
    
    expect_true(file.exists(str_c(temp_dir,'/exports/molecular_formula_assignments.csv')))
    expect_true(file.exists(str_c(temp_dir,'/exports/summarised_molecular_formula_assignments.csv')))
})
