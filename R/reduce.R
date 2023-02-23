#' Reduce an *m/z* features by removing isotopic, adduct or unknown features
#' @rdname reduce
#' @description Reduce *m/z* features in an analysis that includes putative molecular formula assignments by removing isotopic, adduct or unknown features.
#' @param x an object of S4 class `Analysis` or `AnalysisData`
#' @param isotopes TRUE/FALSE remove isotopic features.
#' @param adducts TRUE/FALSE remove features that are multiple adducts of the same molecular formula. The adduct with the highest intensity is retained for each assigned molecular formula.
#' @param unknowns TRUE/FALSE remove unaOssigned *m/z* features.
#' @param isotopic_adducts a vector of additional isotopic adducts to remove if argument `isotopes = TRUE`
#' @return An object of S4 class `Analysis` or `AnalysisData` with reduced *m/z* features.
#' @details If argument `isotopes = TRUE`, all isotopic features are removed. If argument `adducts = TRUE`, the feature with the maximum intensity for each molecular formula is retained.
#' @examples 
#' ## Assign molecular formulas
#' p <- assignments::assignmentParameters('FIE')
#' 
#' assignment <- assignments::assignMFs(assignments::feature_data,p)
#' 
#' ## Retrieve assigned data
#' assigned_data <- metabolyseR::analysisData(
#'  assignments::assignedData(assignment),
#'  tibble::tibble(sample = seq_len(nrow(assignments::feature_data)))
#'  )
#' 
#' reduced_data <- metaboMisc::reduce(assigned_data)
#' 
#' reduced_data
#' @export

setGeneric('reduce',
           function(x, 
                    isotopes = TRUE, 
                    adducts = TRUE, 
                    unknowns = FALSE,
                    isotopic_adducts = c('[M+Cl37]1-','[M+K41]1+'))
               standardGeneric('reduce'))

#' @rdname reduce

setMethod('reduce',signature = 'Analysis',
          function(x,
                   isotopes = TRUE, 
                   adducts = TRUE, 
                   unknowns = FALSE,
                   isotopic_adducts = c('[M+Cl37]1-','[M+K41]1+')){
              preTreated(x) <- x %>%
                  preTreated() %>%
                  reduce(isotopes = isotopes,
                         adducts = adducts,
                         unknowns = unknowns,
                         isotopic_adducts = isotopic_adducts)
              
              return(x)
          }
)

#' @rdname reduce
#' @importFrom metabolyseR dat<-
#' @importFrom dplyr group_by summarise all_of
#' @importFrom stringr str_split_fixed
#' @importFrom metabolyseR preTreated preTreated<-
#' @importFrom tidyr separate

setMethod('reduce',signature = 'AnalysisData',
          function(x,
                   isotopes = TRUE, 
                   adducts = TRUE, 
                   unknowns = FALSE,
                   isotopic_adducts = c('[M+Cl37]1-','[M+K41]1+')
                   ){
              d <- x %>%
                  dat()
              
              feat <-  d %>%
                  rowid_to_column(var = 'Sample') %>%
                  gather('Feature','Intensity',-Sample) %>%
                  group_by(Feature) %>%
                  summarise(Intensity = mean(Intensity)) %>%
                  separate(Feature,
                           into = c('m/z','MF','Isotope','Adduct'),
                           sep = ' ',
                           remove = FALSE,
                           fill = 'right')
              
              feat[feat == ''] <- NA
              
              unknown_features <- feat %>%
                  filter(is.na(MF))
              
              feat <- feat %>%
                  filter(!is.na(MF))
              
              if (isTRUE(isotopes)) {
                  feat <- feat %>%
                      filter(is.na(Isotope),
                             !(Adduct %in% isotopic_adducts))
              }
              
              if (isTRUE(adducts)) {
                  feat <- feat %>%
                      group_by(MF) %>% 
                      filter(Intensity == max(Intensity))
              }
              
              if (isFALSE(unknowns)) {
                  feat <- feat %>%
                      bind_rows(unknown_features)
              }
              
              d <- d %>%
                  select(all_of(feat$Feature))
              
              dat(x) <- d
              
              return(x)
          }
)
