#' Bar chart with standard error lines using ggplot
#' 
#' @param aov.output is a data frame containing the statistics information with the following as column names.
#' \describe{
#' \item{Treatment}{The vector with the independent variables}
#' \item{x.mean}{Vector with mean values}
#' \item{x.sd}{vector with standard deviation}
#' \item{x.n}{vectore with the sample sizes}
#' }
#' The object \emph{stats} obtained from the function \code{Tt.aov()} can be used directly, as it has the specified format.
#' @param treat2=FALSE plots 1-way anova. If a second independent variable exists, change to TRUE
#' 
#' @return gBarPlo An object for ggplot graphics
#' @keywords anova ggplot barplot
#' @export
#' @examples
#' plot <- Tt.ggBarplot(data$stats) # loading the plot directly from stats object
#' plot + labs(title="Graphic Title", subtitle='Subtitle', x='labels in X', y='labels in Y') + scale_fill_brewer(palette=3, name='Treatments') # ggplot plotting
Tt.Barplot <- function(aov.output, treat2=FALSE)
{
    require(ggplot2)
    stats <- do.call(data.frame,aov.output)
    stats$x.se <- stats$x.sd/sqrt(stats$x.n)
    limits <- aes(ymax=stats$x.mean+stats$x.se, ymin=stats$x.mean-stats$x.se)
     if(treat2==FALSE){
        gBarPlo <- ggplot(data=stats, aes(x=factor(Treatment), y=x.mean, fill=factor(Treatment))) + geom_bar(stat='identity', position= position_dodge(0.5), colour='black', width=0.4) + geom_errorbar(limits, position=position_dodge(0.5), width=0.2) + theme_bw() + theme(plot.title=element_text(family="Helvetica", face="bold", size=(15), hjust=0.5), text=element_text(size=15), legend.title=element_text(face="bold", size=(13)), legend.text=element_text(size=(13)))
    }
    else{
        gBarPlo <- ggplot(data=stats, aes(x=factor(Treatment2), y=x.mean, fill=factor(Treatment))) + geom_bar(stat='identity', position= position_dodge(0.5), colour='black', width=0.4) + geom_errorbar(limits, position=position_dodge(0.5), width=0.2) + theme_bw() + theme(plot.title=element_text(family="Helvetica", face="bold", size=(15), hjust=0.5), text=element_text(size=15), legend.title=element_text(face="bold", size=(13)), legend.text=element_text(size=(13)))
        }
    gBarPlo
}
