
pp <- metabolyseR::analysisParameters('pre-treatment')
metabolyseR::parameters(pp,
                        'pre-treatment') <- metabolyseR::preTreatmentParameters(
                            list(
                                occupancyFilter = 'maximum',
                                impute = 'all',
                                transform = 'TICnorm'
                            )
                        )
metabolyseR::changeParameter(pp,'parallel') <- 'no'

test_that("preTreatModes works for Binalysis class", {
    pd <- preTreatModes(bd,pp)
    
    expect_s4_class(pd,'Analysis')
})
