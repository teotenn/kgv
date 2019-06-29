#' Plot spectra for EDS analysis
#'
#' The function uses basic R plot for plotting spectra. However, if elements.list is provided, it will provide labels for the elements, based on the energy (KeV) table of the elements for EDS analysis.
#'
#' @param energy vector containing the values for the x axe. Typically in KeV for EDS analysis
#' @param counts vector containing the values for the y axe
#' @param elements.list a vector containing the simbol of the elements to be displayed in the graphic
#' @param lab.size size of the labels. Note that this size is considerably different for ggplot2 and basic R
#' @param y.lim vector containing limits for y axe
#' @param x.lim vector containing limits for x axe
#' @param as.ggplot creates the plot using 'ggplot2' and 'scales' packages. If false, it will use base R packages
#'
#' @return spectra plot
#' @keywords spectra EDS
#' @export
#' @examples
#' Tt.spectra(spectrum$Energy, spectrum$Counts,elements.list=c('Pb','Zn','P'), y.lim=c(0,4000), x.lim=c(0,11), as.ggplot=F) # Simple plot with personilised limits
#' Tt.spectra(spectrum$Energy, spectrum$Counts,elements.list=c('Ca'),as.ggplot=F) # When there is overlap, as Ca and Sb, is best to add second element manually
#' text(3.5, 500, 'Prueba', cex = 0.8)
#' Tt.spectra(spectrum$Energy, spectrum$Counts,elements.list=c('Ca')) # However this has been fixed using 'ggplot2'
Tt.spectra <- function(energy, counts, elements.list=NULL, lab.size=1,
                       y.lim=c(0,max(counts, na.rm=T)),
                       x.lim=c(0,max(energy, na.rm=T)),
                       as.ggplot=TRUE)
{
    ## as ggplot2 object
    if (as.ggplot==T){
        require(ggplot2)
        require(scales)
        xy.gg <- data.frame(energy=energy,counts=counts)
        spectra <- ggplot(data=xy.gg, aes(x=energy, y=counts))+
            geom_line()+
            scale_y_continuous(limits=y.lim, oob=rescale_none)+
            scale_x_continuous(limits=x.lim, oob=rescale_none)+
            theme_bw()
        spectra
    }
    ## Using basic R graphics
    else{
        plot(energy,counts,type='l',
             ylim=y.lim,
             xlim=x.lim,
             xlab='Energy (keV)',
             ylab='Counts', frame.plot=F)
    }
    data(elements) # loads chemical info
    xy.elements <- data.frame()
    xy.elements2 <- data.frame()
    for (element in elements.list)
    {
        eds <- elements[which(elements$Symbol == element),24]  # extracts eds coords
        eds2 <- elements[which(elements$Symbol == element),25]
        ## Formatting numbers to plot
        energy <- format(round(energy, 2), nsmall = 2)
        eds <- as.numeric(format(round(eds, 2), nsmall = 2))
        eds2 <- as.numeric(format(round(eds2, 2), nsmall = 2))
        energy <- as.numeric(energy)
        ## new local data.frames
        xy <- data.frame(energy=energy,counts=counts)
        xy.eds <- xy[xy$energy==eds,]
        xy.eds2 <- xy[xy$energy==eds2,]
        if (as.ggplot!=T){
            text(eds,xy.eds$counts[1]+200,element,cex=lab.size)
            text(eds2,xy.eds2$counts[1]+200,element,cex=lab.size)
        }
        else {
            xy.labs <- data.frame(x=eds,y=xy.eds$counts[1]+200,
                                  element=element)
            xy.elements <- rbind(xy.elements,xy.labs)
            xy.labs2 <- data.frame(x=eds2,y=xy.eds2$counts[1]+200,
                                  element=element)
            xy.elements2 <- rbind(xy.elements2,xy.labs2)
        }
    }
    if (as.ggplot==T && missing(elements.list)){spectra}
    else if (as.ggplot==T){
        gg.labs <- geom_label(data=xy.elements,
                              aes(x=x,y=y,label=element),
                              size=lab.size,
                              position=position_jitter(width=0))
        gg.labs2 <- geom_label(data=xy.elements2,
                              aes(x=x,y=y,label=element),
                              size=lab.size,
                              position=position_jitter(width=0))
        spectra+gg.labs+gg.labs2
    }
}
