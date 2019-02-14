#' Calculates the weight of the solute in grams per liter of a given chemical formula
#'
#' A chemical formula is provided in a specific format in order to calculate the weight of solute necesary to produce a solution with given molarity
#' 
#' @param elements.list Vector containing all the chemical elements (symbols) in the formula
#' @param atoms Vector containing the number of molecules of each element. Must match with "elements.list".
#' @param molarity Molarity of the solution. If not specified, default is 1M.
#' @param details If false, the result will be only the value as numeric. Otherwise default is set as true, which will provide detailed information as character.
#'
#' @return If "details=T" (default) the result will be a vector of characters providing the chemical formula used, molarity and weight of the solute in grams per liter. If "details=F" the result will be only the weight of the solute (g/L) as numeric.
#' @keywords molarity elements molar-weight
#' @export
#' @examples 
#' Tt.mol(c('Ca','Cl'),c(1,2),molarity=0.1,details=F) # Weight (g/L) of calcium chloride to create 1L of solution at 0.1M
#' formula <- c('Ca','N','O') # Elements in Calcium nitrate
#' molecules <- c(1,2,6) # Molecules of each elements in formula (Ca(NO3)2)
#' Tt.mol(formula,molecules)

Tt.mol <- function(elements.list,atoms,molarity=1,details=TRUE)
{
    data(elements)
    mol = 0
    for (i in 1:length(elements.list))
    {
        weight <- elements[which(elements$Symbol == elements.list[i]),4]*atoms[i]
        mol <- mol+weight
    }
    if(details==TRUE)
    {
        formula=c()
        for (n in 1:length(elements.list))
        {
            value <- paste(elements.list[n],atoms[n],sep="")
            formula <- paste(formula,value,sep="")
            formula
        }
        detail <- c(formula,paste(molarity,'M'),mol*molarity)
        detail
    }
    else{mol*molarity}
}
