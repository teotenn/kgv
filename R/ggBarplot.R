#' Bar chart with standard error lines using ggplot
#' 
#' @param aov.output is a data frame containing the statistics information with the following data. Column names must match with the described below:
#' \describe{
#' \item{Treatment(s)}{The vector with the independent variables. Should be stored in the first columns of the data frame (1 or 2). No name matching is necessary.}
#' \item{Mean}{Vector with mean values}
#' \item{SD}{vector with standard deviation}
#' \item{n}{vector with the sample sizes}
#' }
#' The object \emph{stats} obtained from the function \code{Tt.aov()} can be used directly, as it has the specified format.
#' @param treat2=FALSE plots 1-way anova. If a second independent variable exists, change to TRUE
#' 
#' @return gBarPlo An object for ggplot graphics
#' @keywords anova ggplot barplot
#' @export
#' @examples
#' Zn <- Tt.aov(compost$Pb,compost$Treatment,compost$Plant) # 2-way anova
#' plot <- Tt.barplot(Zn$stats, treat2=T)
#' plot + labs(title="Zn in plant tissue", x='Species', y='mg/Kg')+
#' scale_fill_brewer(palette=3)+
#' guides(fill=guide_legend(title='Treatment'))

Tt.barplot <- function(aov.output, treat2=FALSE)
{
    require(ggplot2)
    stats <- do.call(data.frame,aov.output)
    stats$x.se <- stats$SD/sqrt(stats$n)
    limits <- aes(ymax=stats$Mean+stats$x.se, ymin=stats$Mean-stats$x.se)
    Treatment=names(stats)[1]    
     if(treat2==FALSE){
         gBarPlo <- ggplot(data=stats, aes(x=factor(eval(parse(text=Treatment))), y=Mean, fill=factor(eval(parse(text=Treatment)))))+
             geom_bar(stat='identity', position= position_dodge(0.5), colour='black', width=0.4) +
             geom_errorbar(limits, position=position_dodge(0.5), width=0.2) +
             theme_bw() +
             theme(plot.title=element_text(family="Helvetica", face="bold", size=(15), hjust=0.5),
                   text=element_text(size=15), legend.title=element_text(face="bold", size=(13)),
                   legend.text=element_text(size=(13)))+
             guides(fill=guide_legend(title=Treatment))+
             labs(x=Treatment, y=NULL)
    }
     else{
        Treatment2=names(stats)[2]
        gBarPlo <- ggplot(data=stats, aes(x=factor(eval(parse(text=Treatment2))), y=Mean, fill=factor(eval(parse(text=Treatment))))) +
            geom_bar(stat='identity', position= position_dodge(0.5), colour='black', width=0.4) +
            geom_errorbar(limits, position=position_dodge(0.5), width=0.2) +
            theme_bw() +
            theme(plot.title=element_text(family="Helvetica", face="bold", size=(15), hjust=0.5),
                  text=element_text(size=15), legend.title=element_text(face="bold", size=(13)),
                  legend.text=element_text(size=(13)))+
            guides(fill=guide_legend(title=Treatment))+
            labs(x=Treatment2, y=NULL)
        }
    gBarPlo
}
