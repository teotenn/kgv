#' Direct estimation of 1 or 2 way anova
#'
#' Performs the analysis of variance using the functions aov and anova. Additionally, creates a table with mean values, standard deviation and size of sample per treatment. If p>0.05, a Tukey test is also performed.
#' 
#' @param vx dependent variable
#' @param vtreat independent variable(s). If vtreat2 exists, the function will perform a 2-way anova
#' @param p.lim p-value for the limit of ANOVA to perform Tukey test
#' @param Tuk.labs If TRUE, it will print the differences by TukeyHSD by different letters (requires package 'multcompView')
#'
#' @return aov.output a list with the following objects
#' \enumerate{
#' \item anova : Which contains the details of the analysis
#' \item stats : mean, sd, size of sample (n) and, if requested, Tukey HSD differences
#' \item p : value(s) of p from anova, for a quick answer
#' \item Tukey.interaction : If p>p.lim, TukeyHSD is performed. In the 2-ways anova, Tukey test will be perform for the interaction between treatments, as well as per-treatment, being 'Tukey.treatment1' and 'Tukey.treatment2' the results in the list.
#' }
#'
#' @keywords anova TukeyHSD
#' @export
#' @examples
#' data('compost')
#' Pb <- Tt.aov(compost$Pb, compost$Treatment, p.lim=0.001) # 1-way anova
#' Pb
#'
#' Zn <- Tt.aov(compost$Pb,compost$Treatment,compost$Plant) # 2-way anova
#' Zn$anova # Results from anova
#' Zn$stats # Mean, SD and n values

Tt.aov <- function(vx, vtreat1,vtreat2=NULL,p.lim=0.05, Tuk.labs=TRUE)
{    
    vx <- abs(vx)
    treatment1 = paste(deparse(substitute(vtreat1)))
    treatment2 = paste(deparse(substitute(vtreat2)))           
    if(missing(vtreat2))
    { 
        VarAov <- aov(vx~vtreat1) # 1-way anova
    }
    else
    {
        VarAov <- aov(vx~vtreat1*vtreat2) # 2-way anova
        VarAov.t1 <- aov(vx~vtreat1)
        VarAov.t2 <- aov(vx~vtreat2)
    }    
    VarAnova <- anova(VarAov)
    ## Writing stats (mean, sd and n)
    Tuk.t1=NULL
    Tuk.t2=NULL
    if(missing(vtreat2))
    {
        p <- VarAnova$Pr[1]
        if(p<p.lim) {
            Tuk.int <- TukeyHSD(VarAov)
            if(Tuk.labs==TRUE) {
                require(multcompView)
                Tuk.levels <- Tuk.int[["vtreat1"]][,4]
                Tuk.labels <- data.frame(multcompLetters(Tuk.levels)['Letters'])
                Tuk.labels$treatment1 <- row.names(Tuk.labels)
            }
        }
        else{Tuk.int <- 'No significant differences'}
        stats <- aggregate(vx, by=list (Treats=vtreat1),
                           FUN=function(x) c(mean=mean(x, na.rm=T),
                                             sd=sd(x,na.rm=T), n=length(x)))
        stats<-do.call(data.frame,stats)
        stats$x.se <- stats$SD/sqrt(stats$n)
        if (Tuk.int != 'No significant differences' && Tuk.labs==TRUE){
            stats <- merge(stats, Tuk.labels, by.x="Treats",
                         by.y="treatment1")
            stats=stats[order(stats$Treats),]
        }
        if (length(names(stats))==4){
            names(stats) <- c(treatment1,"Mean", "SD","n")}
        else {names(stats) <- c(treatment1,"Mean", "SD","n","HSD")}
    }
    else
    {
        p <- data.frame(interaction=VarAnova$Pr[3], treat1=VarAnova$Pr[1],
                        treat2=VarAnova$Pr[2], row.names="P values")
        ## Tukey test between and within treatments
        if(p[1,1]<p.lim) {
            Tuk.int <- TukeyHSD(VarAov)
            if(Tuk.labs==TRUE) {
                require(multcompView)
                Tuk.levels <- Tuk.int[[paste("vtreat1","vtreat2",sep=":")]][,4]
                Tuk.labels <- data.frame(multcompLetters(Tuk.levels)['Letters'])
                Tuk.labels$treatment1 <- sapply(strsplit(row.names(Tukey.labels), ":"), "[",1)
                Tuk.labels$treatment2 <- sapply(strsplit(row.names(Tukey.labels), ":"), "[",2)
            }
        }
        else{Tuk.int <- 'No significant differences for the interaction of treatments'}
        if(p[1,2]<p.lim) {Tuk.t1 <- TukeyHSD(VarAov.t1)}        
        else{Tuk.t1 <- paste('No significant differences for the effect of', treatment1)}        
        if(p[1,3]<p.lim) {Tuk.t2 <- TukeyHSD(VarAov.t2)}        
        else{Tuk.t2 <- paste('No significant differences for the effect of', treatment2)}      
        names(p) <- c("Interaction",treatment1,treatment2)
        stats <- aggregate(vx, by=list (Treats1=vtreat1, Treats2=vtreat2),
                           FUN=function(x) c(mean=mean(x, na.rm=T),
                                             sd=sd(x, na.rm=T), n=length(x)))
        stats<-do.call(data.frame,stats)
        stats$x.se <- stats$SD/sqrt(stats$n)
        if (Tuk.int != 'No significant differences for the interaction of treatments'
            && Tuk.labs==TRUE){
            stats <- merge(stats, Tuk.labels, by.x=c("Treats1","Treats2"),
                         by.y=c("treatment1","treatment2"))
            stats=stats[order(stats$Treats1, stats$Treats2),]
        }
        if (length(names(stats))==5){
            names(stats) <- c(treatment1,treatment2,"Mean", "SD","n")}
        else {names(stats) <- c(treatment1,treatment2,"Mean", "SD","n","HSD")}
    }
    ## Output
    aov.output <- list(
        anova=VarAnova,
        stats=stats,
        p=p,
        Tukey.interaction=Tuk.int,
        Tukey.treatment1=Tuk.t1,
        Tukey.treatment2=Tuk.t2)
}
