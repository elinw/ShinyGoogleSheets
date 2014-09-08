library(httr)
## http://blog.revolutionanalytics.com/2014/06/reading-data-from-the-new-version-of-google-spreadsheets.html
## Code from Andrei de Vries
readGoogleSheet <- function(url, na.string="", header=TRUE){
        stopifnot(require(XML))

        # Suppress warnings because Google docs seems to have incomplete final line
        suppressWarnings({
                doc <- content(GET(url), as="text")
                ##doc <- paste(readLines(url), collapse=" ")
        })
        if(nchar(doc) == 0) stop("No content found")

      htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)   
        
        ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
        lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}
