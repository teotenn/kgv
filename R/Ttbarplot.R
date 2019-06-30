#' Bar chart with standard error lines using ggplot
#' 
#' @param aov.output is a data frame containing the statistics information with the following data: mean values, treatments, error lines, and if desired, letters representing statistical difference.
#' The object \emph{stats} obtained from the function \code{Tt.aov()} can be used directly.
#' @param T1 name of the column containing the treatments
#' @param T2 name of the column containing the treatments for 2-factors plot (optional)
#' @param e.lines name of the column containing the data of the error lines to be plotted. Default is set "SE" for standard error created from function \code{Tt.aov}
#' @param dif name of the column with the letters representing statistical differences
#' @param dif.h distance from the uper error line to plot the \code{dif} letters
#' 
#' @return An object for ggplot graphics
#' @keywords anova ggplot barplot
#' @export
#' @examples
#' Zn <- Tt.aov(compost$Pb,compost$Treatment,compost$Plant) # 2-way anova
#' plot <- Tt.barplot(Zn$stats, treat2=T)
#' plot + labs(title="Zn in plant tissue", x='Species', y='mg/Kg')+
#' scale_fill_brewer(palette=3)+
#' guides(fill=guide_legend(title='Treatment'))

Tt.barplot <- function(aov.output, T1, T2=NULL, e.lines="SE",
                         dif=NULL, dif.h=0.5)
{
    require(ggplot2)
    stats <- do.call(data.frame,aov.output)
    if(missing(T2)){treat2=FALSE}
    else {
        treat2=TRUE
        Treatment2 = with(stats, factor(eval(parse(text=T2))))
    }
    Treatment = with(stats, factor(eval(parse(text=T1))))
    error.lines = with(stats, (eval(parse(text=e.lines))))
    limits <- aes(ymax=stats$Mean+error.lines, ymin=stats$Mean-error.lines)
     if(treat2==FALSE){
         gBarPlo <- ggplot(data=stats, aes(x=factor(Treatment),
                                           y=Mean,
                                           fill=factor(Treatment))) +
             geom_bar(stat='identity', position= position_dodge(0.5),
                      colour='black', width=0.4) +
             geom_errorbar(limits, position=position_dodge(0.5), width=0.2) +
             theme_bw() +
             theme(plot.title=element_text(family="Helvetica", face="bold",
                                           size=(15), hjust=0.5),
                   text=element_text(size=15),
                   legend.title=element_text(face="bold", size=(13)),
                   legend.text=element_text(size=(13)))+
             guides(fill=guide_legend(title=Treatment))+
             labs(x=Treatment, y=NULL)
    }
     else{
        gBarPlo <- ggplot(data=stats, aes(x=factor(Treatment2),
                                          y=Mean,
                                          fill=factor(Treatment))) +
            geom_bar(stat='identity', position= position_dodge(0.5),
                     colour='black', width=0.4) +
            geom_errorbar(limits, position=position_dodge(0.5), width=0.2) +
            theme_bw() +
            theme(plot.title=element_text(family="Helvetica", face="bold", size=(15),
                                          hjust=0.5),
                  text=element_text(size=15),
                  legend.title=element_text(face="bold", size=(13)),
                  legend.text=element_text(size=(13)))+
            guides(fill=guide_legend(title=Treatment))+
            labs(x=Treatment2, y=NULL)
    }
    if (missing(dif)){
        gBarPlo
    }
    else {
        dif.labels <- geom_text(data=stats,
                                  aes(y=Mean+error.lines,
                                      label=factor(eval(parse(text=dif)))),
                                size=4,vjust=-dif.h,
                                position=position_dodge(0.5))
        gBarPlo <- gBarPlo+dif.labels
    }
    gBarPlo
}
