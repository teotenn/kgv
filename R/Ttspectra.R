#' Plot spectra for EDS analysis
#'
#' The function uses basic R plot for plotting spectra. However, if elements.list is provided, it will provide labels for the elements, based on the energy (KeV) table of the elements for EDS analysis.
#'
#' @param energy vector containing the values for the x axe. Typically in KeV for EDS analysis
#' @param counts vector containing the values for the y axe
#' @param elements.list a vector containing the simbol of the elements to be displayed in the graphic
#' @param y.lim vector containing limits for y axe
#' @param x.lim vector containing limits for x axe
#'
#' @return spectra plot
#' @keywords spectra EDS
#' @export
#' @examples
#' Tt.spectra(spectrum$Energy, spectrum$Counts,elements.list=c('Pb','Zn','P'), y.lim=c(0,4000), x.lim=c(0,11)) # Simple plot with personilised limits
#' Tt.spectra(spectrum$Energy, spectrum$Counts,elements.list=c('Ca')) # When there is overlap, as Ca and Sb, is best to add second element manually
#' text(3.5, 500, 'Prueba', cex = 0.8)
Tt.spectra <- function(energy, counts, elements.list, y.lim=c(0,max(counts, na.rm=T)),x.lim=c(0,max(energy, na.rm=T)))
{
    data(elements)    
    plot(energy,counts,type='l', ylim=y.lim,xlim=x.lim, xlab='Energy (keV)',ylab='Counts', frame.plot=F)
    for (element in elements.list)
    {
        eds <- elements[which(elements$Symbol == element),24]
        eds2 <- elements[which(elements$Symbol == element),25]
        energy <- format(round(energy, 2), nsmall = 2)
        eds <- as.numeric(format(round(eds, 2), nsmall = 2))
        eds2 <- as.numeric(format(round(eds2, 2), nsmall = 2))
        energy <- as.numeric(energy)
        xy <- data.frame(energy=energy,counts=counts)
        xy.eds <- xy[xy$energy==eds,]
        xy.eds2 <- xy[xy$energy==eds2,]
        text(eds,xy.eds$counts[1]+200,element,cex=0.8)
        text(eds2,xy.eds2$counts[1]+200,element,cex=0.8)
    }
}