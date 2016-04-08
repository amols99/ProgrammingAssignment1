corr <- function(directory, threshold=0) {
    
    if (!dir.exists(directory)){
        print(directory)
        stop( "directory does not exist")
    }
    allfiles <- list.files(directory)
    allcorr = vector(mode="numeric")
    for(fl in allfiles){
        corr <- NULL
        file <- paste(directory , "/" , fl , sep = "")
        mdata <- read.csv(file , colClasses = c("character" ,"numeric" , "numeric" , "integer"))
        count <- nrow(mdata[ !is.na(mdata[,"sulfate"]) & !is.na(mdata[ ,"nitrate"]), ])
        if (count > threshold){
            corr <- cor(mdata[,"sulfate"], mdata[,"nitrate"],use ="pairwise.complete.obs")
        }
        if (!is.null(corr))
        {
            allcorr <- append(allcorr,round(corr, 5))
        }
     
    }

    # Return vector of correlations
    allcorr
}  ## end function