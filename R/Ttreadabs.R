#' Calculation of a linear model to read and interprete absorbance
#'
#' The function creates the linear model, based on the absorbance of samples with known concentration. If added, the function can also calculate the concentrations of further samples based on the absorbance
#'
#' @param model.concentrations a vector containing the known concentrations of the standards (the order must match with that of absorbance)
#' @param model.absorbance a vector containing the absorbance read on the standards (the order must match with that of concentrations)
#' @param predict a vector containing absorbance, in order to predict concentrations
#' @param show.plot if TRUE, the data is plotted showing the model
#'
#' @return The function returns a list containing the details of the model and the predicted values if added:
#' \enumerate{
#' \item model: the summary of the linear model, calculated with the function "lm(model.concentrations~model.absorbance)"
#' \item Pr: probability of the model
#' \item R.sq: Adjusted R squared of the model
#' \item formula: Formula of the model
#' \item predicted.concentrations: Vector containing the concentrations calculated with the formula obtained from the model
#' }
#'
#' @keywords lm, absorbance
#' @export
Tt.read.abs <- function(model.concentrations, model.absorbance, predict=NULL, show.plot=TRUE){
### ERRORS
    if(is.vector(model.concentrations)){model.concentrations}
    else(stop("ERROR: model.concentrations must be a numeric vector containing the concentrations of the standards"))
    if(is.vector(model.absorbance)){model.absorbance}
    else(stop("ERROR: model.absorbance must be a numeric vector containing the absorbance of the standards"))
### Linear model
    modelo <- lm(model.concentrations~model.absorbance)
    resumen <- summary(modelo)
    Pr <- resumen$coefficients["model.absorbance", "Pr(>|t|)"] # Probability of the model
    R.sq <- resumen$adj.r.squared
    intercept <- resumen$coefficients["(Intercept)","Estimate"]
    terms <- resumen$coefficients["model.absorbance","Estimate"]
    formula <- paste("y=",round(terms, digits=2),"x +",round(intercept, digits=5))
### PLOT
    if (show.plot==TRUE){
        plot(Concentration ~ ABS, data=curve)
        abline(lm(Concentration ~ ABS, data=curve))
        text(curve$ABS[2],curve$Concentration[length(curve$Concentration)-1],paste(formula,"\n","R\u00B2=" ,round(resumen$adj.r.squared, digits=4), "\n","Pr",ifelse(Pr<0.001,"< 0.001",paste("=",round(Pr,digits=4)))),cex=0.8)
    }
    else{
        print(formula)
        print(paste("Adjusted R squared=",R.sq))
    }
### Predicted values
    if(missing(predict)){
        predicted.concentrations='MESSAGE: You can enter absorbance directly as a vector to predict concentrations'
    }
    else if(is.vector(predict))
        {
            predicted.concentrations <- (predict*terms)+intercept
            predicted.concentrations
        }
        else
        {
            stop("ERROR: Absorbance to predict values must be vector")
        }
    lista <- list(model=resumen,Pr=Pr, R.sq=R.sq, formula=formula,predicted.concentrations=predicted.concentrations)
}
