#' Calculation of ppm or ppb of a solid
#'
#' The function calculates proportion of element contained in a solid, express as mass units of element per mass units of sample (g, ug, mg, Kg), after being processed through a solution, particularly useful after reading absorbance AA, ICP, etc.
#'
#' @param solid mass of the solid processed in a solution
#' @param solution volume of the solution where the solid was processed
#' @param concentration concentration obtained after the lecture
#' @param units a list containing units of the solid, solution and concentration. See details
#' @param mol if some units are expressed as mol, molar weght should be specified
#' @param result.units mass units of element per mass units of sample
#'
#' @return returns the proportion of element contained in a solid, expressed in terms specified under "result.units"
#' 
#' @details Units of mass can be expressed as proportions of grams ('ug', 'mg', 'g', 'Kg') or mol ('M', 'mM', 'uM'). Units of volume as proportions of liter ('l','ml','ul'). This apply for both parameters, "units" (units of the input data) and "result.units" (units to express the results).
#' @details "units" and "result.units" are both of class lists. If only one unit is changed, all the other parameters must be enter again as part of the list (see below example of proline, where only mass was changed for "uM")
#' @details Make sure that "concentration" is given as mass unit per liter. The function can addapt mass units, but is not addapted to change liter.
#'
#' @export
#' @examples
#' # Proline in plants
#' # 0.1014g of plant were processed in 7.5ml of water
#' # The laboratory results give 13.009 uM/L proline (molar weight 75.066)
#' # We want to express the results as mg of proline per Kg of plant
#' Tt.ppm(solid=0.1014,solution=7.5,concentration=13.009,mol=75.066,units=list(solid='g',solution='ml',concentration='uM'), result.units=list(element='mg',sample='Kg'))
#'
#' # Soil was digested in aqua regia, and diluted at 25ml for ICP
#' soil <- c(0.266,0.271,0.277,0.243,0.257) # weight of samples
#' As <- c(0.054968685,0.056348685,0.090648685,0.057058685,0.070963685) # results from ICP
#' Pb <- c(0.837786373333334,0.916599706666667,0.886459706666667,0.86488304,0.981849706666667)
#' icp <- data.frame(As,Pb)
#' Tt.ppm(soil,25,icp)
Tt.ppm <- function(solid,solution,concentration,mol=NULL,units=list(solid='g',solution='ml',concentration='mg'), result.units=list(element='mg',sample='Kg'))
{
### Tranforming units
    ## Solid sample
    if (units[["solid"]]=='g'){solid <- solid}
    else if (units[["solid"]]=='mg'){solid <- solid/1000}
    else if (units[["solid"]]=='ug'){solid <- solid/1000000}
    else if (units[["solid"]]=='Kg'){solid <- solid*1000}
    else {stop("Solid units (sample) should be either mg, ug, g or Kg")}
    ## Solution of the samples
    if (units[["solution"]]=='l'){solution <- solution}
    else if (units[["solution"]]=='ml'){solution <- solution/1000}
    else if (units[["solution"]]=='ul'){solution <- solution/1000000}
    else {stop("Solution units should be either l, ml or ul")}
    ## Units of the concentration (mass/liter)
    if (units[["concentration"]]=='g'){concentration <- concentration}
    else if (units[["concentration"]]=='mg'){concentration <- concentration/1000}
    else if (units[["concentration"]]=='ug'){concentration <- concentration/1000000}
    else if (units[["concentration"]]=='Kg'){concentration <- concentration*1000}
    else if (units[["concentration"]]=='M'){concentration <- concentration*mol}
    else if (units[["concentration"]]=='mM'){concentration <- (concentration*mol)/1000}
    else if (units[["concentration"]]=='uM'){concentration <- (concentration*mol)/1000000}
    else {stop("Concentrations should be expressed in terms of grams (mg, ug, g or Kg) or mol (M, mM or uM). Considering that concentration is given as mass unit per liter of solution.")}    
### Calculate the proportion in g of soild sample per liter of solution
    proportion <- solid/solution
### Units to express results
    if (result.units[["element"]]=='g'){concentration <- concentration}
    else if (result.units[["element"]]=='mg'){concentration <- concentration*1000}
    else if (result.units[["element"]]=='ug'){concentration <- concentration*1000000}
    else if (result.units[["element"]]=='Kg'){concentration <- concentration/1000}
    else {stop("Concentrations should be expressed as mg, ug, g or Kg. Considering that concentration is given as mass unit per liter of solution")}
    if (result.units[["sample"]]=='g'){proportion <- proportion}
    else if (result.units[["sample"]]=='mg'){proportion <- proportion*1000}
    else if (result.units[["sample"]]=='ug'){proportion <- proportion*1000000}
    else if (result.units[["sample"]]=='Kg'){proportion <- proportion/1000}
    else {stop("Concentrations should be expressed as mg, ug, g or Kg. Considering that concentration is given as mass unit per liter of solution")}
### Proportion based on the desired units
    results <- concentration/proportion
    results
}
