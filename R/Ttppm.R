#' Calculation of ppm or ppb of a solid
#'
#' The function calculates proportion of element contained in a solid, express as mass units of element per mass units of sample (g, ug, mg, Kg), after being processed through a solution, particularly useful after reading absorbance AA, ICP, etc.
#'
#' @param solid mass of the solid processed in a solution
#' @param solution volume of the solution where the solid was processed
#' @param concentration concentration obtained after the lecture
#' @param solid.u units in which the solid is expressed ("g", "mg", "ug", "Kg")
#' @param solution.u units in which the solution is expressed ("l", "ml", "ul")
#' @param concentration.u units of mass per Liter, in which the concentratoin is expressed ("g", "mg", "ug", "Kg", "M", "mM", "uM")
#' @param mol if some units are expressed as mol, molar weght should be specified
#' @param result.units mass units of element per mass units of sample. See details
#'
#' @return returns the proportion of element contained in a solid, expressed in terms specified under "result.units"
#' 
#' @details Units of mass can be expressed as proportions of grams ('ug', 'mg', 'g', 'Kg') or mol ('M', 'mM', 'uM'). Units of volume as proportions of liter ('l','ml','ul'). This apply for both parameters, units ("solid.u", "solution.u" and "concentration.u") and "result.units" (units to express the results).
#' @details "result.units" is of class lists. If only one unit is changed, all the other parameters must be enter again as part of the list
#' @details Make sure that "concentration" is given as mass unit per liter. The function can addapt mass units, but is not addapted to change liter.
#'
#' @export
#' @examples
#' # Proline in plants
#' # 0.1014g of plant were processed in 7.5ml of water
#' # The laboratory results give 13.009 uM/L proline (molar weight 75.066)
#' # We want to express the results as mg of proline per Kg of plant
#' Tt.ppm(solid=0.1014,solution=7.5,concentration=13.009,mol=75.066,solid.u='g',solution.u='ml',concentration.u='uM', result.units=list(element='ug',sample='g'))
#'
#' # Soil was digested in aqua regia, and diluted at 25ml for ICP
#' soil <- c(0.266,0.271,0.277,0.243,0.257) # weight of samples of soil for digestion
#' As <- c(0.054968685,0.056348685,0.090648685,0.057058685,0.070963685) # results from ICP for As
#' Pb <- c(0.837786373333334,0.916599706666667,0.886459706666667,0.86488304,0.981849706666667) # ICP Pb
#' icp <- data.frame(As,Pb)
#' Tt.ppm(soil,25,icp)
Tt.ppm <- function(solid,solution,concentration,mol=NULL,solid.u='g',solution.u='ml',concentration.u='mg', result.units=list(element='mg',sample='Kg'))
{
### Tranforming units
    ## Solid sample
    solid.g=switch(solid.u,
                   'g'=solid,
                   'mg'=solid/1000,
                   'ug'=solid/1000000,
                   'Kg'=solid*1000)
    ## Solution of the samples
    solution.l=switch(solution.u,
                      'l'=solution,
                      'ml'=solution/1000,
                      'ul'=solution/1000000)
    ## Units of the concentration (transform to g/liter)
    concentration.g=switch(concentration.u,
                           'g'=concentration,
                           'mg'=concentration/1000,
                           'ug'=concentration/1000000,
                           'Kg'=concentration*1000,
                           'M'=concentration*mol,
                           'mM'=(concentration*mol)/1000,
                           'uM'=(concentration*mol)/1000000)
### Proportion in g of soild sample per liter of solution
    proportion <- solid.g/solution.l
### Units to express the results
    ## Concentration of element in g/L to:
    concentration.g=switch(result.units[["element"]],
                           'g'=concentration.g,
                           'mg'=concentration.g*1000,
                           'ug'=concentration.g*1000000,
                           'Kg'=concentration.g/1000,
                           'M'=concentration/mol,
                           'mM'=(concentration/mol)*1000,
                           'uM'=(concentration/mol)*1000000)
    ## Concentration of sample in g/L to:
    proportion.g=switch(result.units[["sample"]],
                        'g'=proportion,
                        'mg'=proportion*1000,
                        'ug'=proportion*1000000,
                        'Kg'=proportion/1000)
### Calculate final expression
    results <- concentration.g/proportion.g
    results
}
