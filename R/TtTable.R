#' Writes a table with mean +/- SD, all as character
#'
#' @param frames.list list containing the data frame with the values. The data frame "stats" obtained from function "Tt.aov" can be used directly here. Otherwise, make sure to name "x.mean" and "x.sd" your columns with mean and standard deviation values, respectively.
#' @param row.list vector containing the names of the rows (must match with row length)
#' @param col.list vector containing the names of the columns (must match with columns length)
#' @param decimals amount of decimals after point. Default is 2
#' @param separator symbol to separate values. Default is "\u00B1"
#'
#' @return a data frame with mean and standard deviation values, set together in a single row as character. Useful to export.
#' @keywords export table mean standard-deviation
#' @export
Tt.table <- function(frames.list,row.list,col.list,decimals=2,separator="\u00B1")
{
    data <- data.frame()
    dat <- data.frame()
    for (i in frames.list)
    {
        i <- as.data.frame(i)
        mean <- round(i$x.mean, decimals)
        sd <- round(i$x.sd,decimals)
        variable <- paste(as.character(mean),as.character(sd),sep=separator)
        dat <- rbind(variable)
        data <- rbind(data,dat)
    }
    rownames(data)=row.list
    colnames(data)=col.list
    data
}
