## http://blog.revolutionanalytics.com/2014/06/reading-data-from-the-new-version-of-google-spreadsheets.html
## Code from Andrei de Vries

library(XML)
cleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
        if(!is.data.frame(dat)){
                dat <- dat[[table]]
        }
        if(is.na(dropFirstCol)) {
                firstCol <- na.omit(dat[[1]])
                if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
                        dat <- dat[, -1]
                }
        } else if(dropFirstCol) {
                dat <- dat[, -1]
        }
        if(skip > 0){
                dat <- dat[-seq_len(skip), ]
        }
        if(nrow(dat) == 1) return(dat)
        if(nrow(dat) >= 2){
                if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
        }
        if(header && nrow(dat) > 1){
                header <- as.character(dat[1, ])
                names(dat) <- header
                dat <- dat[-1, ]
        }
        # Keep only desired columns
        if(!is.na(ncols)){
                ncols <- min(ncols, ncol(dat))
                dat <- dat[, seq_len(ncols)]
        }
        # Keep only desired rows
        if(nrows > 0){
                nrows <- min(nrows, nrow(dat))
                dat <- dat[seq_len(nrows), ]
        }
        # Rename rows
        rownames(dat) <- seq_len(nrow(dat))
        dat
}