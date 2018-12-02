#' Direct estimation of 1 or 2 way anova
#'
#' Performs the analysis of variance using the functions aov and anova. Additionally, creates a table with mean values, standard deviation and size of sample per treatment. If p>0.05, a Tukey test is also performed.
#' 
#' @param vx dependent variable
#' @param vtreat independent variable(s). If vtreat2 exist, the function will perform a 2-way anova
#'
#' @return aov.output a list with the following objects
#' \enumerate{
#' \item anova : Which contains the details of the analysis
#' \item stats : mean, sd and size of sample (n)
#' \item p : value of p from anova, for a quick answer
#' \item Tukey : If p>0.05, tukeyHSD is performed
#' }
#'
#' @keywords anova TukeyHSD
#' @export
#' @examples 

Tt.aov <- function(vx, vtreat1,vtreat2)
{
    if(missing(vtreat2)){
        VarAov <- aov(vx~vtreat1)
    }
    else{
        VarAov <- aov(vx~vtreat1*vtreat2)
    }
    
    VarAnova <- anova(VarAov)
    if(missing(vtreat2)){
        p <- VarAnova$Pr[1]
        stats <- aggregate(vx, by=list (Treatment=vtreat1),
                           FUN=function(x) c(mean=mean(x, na.rm=T),
                                             sd=sd(x,na.rm=T), n=length(x)))
    }
    else{
        p <- VarAnova$Pr[3]
        stats <- aggregate(vx, by=list (Treatment=vtreat1, Treatment2=vtreat2),
                           FUN=function(x) c(mean=mean(x, na.rm=T),
                                             sd=sd(x, na.rm=T), n=length(x)))
    }
    if(p<0.05) {Tuk <- TukeyHSD(VarAov)}
    else{Tuk <- 'No significant differences'}
    aov.output <- list(anova=VarAnova,stats=stats,p=p, Tukey=Tuk)
    }
