#' reduce
#' @rdname reduce
#' @description reduce an analysis by removing isotopic, adduct or unknown features.
#' @param x S4 class of class Analysis
#' @param isotopes TRUE/FALSE remove isotopic features.
#' @param adducts TRUE/FALSE remove multiple adduct features.
#' @param unknowns TRUE/FALSE remove unassigned features.
#' @details Isotope and adduct features are filtered based on the maximum intensity peak for each molecular formulas.
#' @importFrom dplyr group_by summarise
#' @importFrom stringr str_split_fixed
#' @importFrom metabolyseR preTreated preTreated<-
#' @export

setMethod('reduce',signature = 'Analysis',
          function(x,isotopes = T, adducts = T, unknowns = F){
              preTreated(x) <- x %>%
                  preTreated() %>%
                  reduce(isotopes = isotopes,adducts = adducts,unknowns = unknowns)
              
              return(x)
          }
)

#' @rdname reduce
#' @importFrom metabolyseR dat<-
#' @export

setMethod('reduce',signature = 'AnalysisData',
          function(x,isotopes = T, adducts = T, unknowns = F){
              d <- x %>%
                  dat()
              
              feat <-  d %>%
                  rowid_to_column(var = 'Sample') %>%
                  gather('Feature','Intensity',-Sample) %>%
                  group_by(Feature) %>%
                  summarise(Intensity = mean(Intensity)) %>%
                  mutate(MF = str_split_fixed(Feature,' ',4)[,2],
                         Isotope = str_split_fixed(Feature,' ',4)[,3],
                         Adduct = str_split_fixed(Feature,' ',4)[,4])
              
              feat[feat == ''] <- NA
              
              uks <- feat %>%
                  filter(is.na(MF))
              
              feat <- feat %>%
                  filter(!is.na(MF))
              
              if (isTRUE(isotopes)) {
                  feat <- feat %>%
                      split(str_c(.$MF,.$Adduct)) %>%
                      map(~{
                          d <- .
                          d %>%
                              filter(Intensity == max(Intensity))
                      }) %>%
                      bind_rows()
              }
              
              if (isTRUE(adducts)) {
                  feat <- feat %>%
                      split(.$MF) %>%
                      map(~{
                          d <- .
                          d %>%
                              filter(Intensity == max(Intensity))
                      }) %>%
                      bind_rows()
              }
              
              if (isFALSE(unknowns)) {
                  feat <- feat %>%
                      bind_rows(uks)
              }
              
              d <- d %>%
                  select(feat$Feature)
              
             dat(x) <- d
             
             return(x)
          }
)