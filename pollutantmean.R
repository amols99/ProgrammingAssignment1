## calculate sum of a pollutant in a single monitorfile and return result
processmonitor <- function(directory1, pollutant1, id1)
{
    paddedid <- sprintf("%03d", id1)
    file <- paste(directory1 , "/" , paddedid , ".csv" , sep = "")
    monitordata <- read.csv(file , colClasses = c("character" ,"numeric" , "numeric" , "integer"))
    monitordata[,pollutant1]
}

pollutantmean <- function(directory, pollutant, id = 1:332){
    
    ## 'directory' is a character vector of length 1 indicating 
    ## the location of the csv file
    if (!dir.exists(directory)){
        print(directory)
        stop( "directory does not exist")
    }
    
    validpollutant = c("sulfate" ,"nitrate")
    if(!is.na(pollutant) && !is.null(pollutant) )
    {
        if(!is.element(pollutant , validpollutant)){ stop("pollutant variable is not sulfate or nitrate")}
    }
    else
    {
        stop("pollutant variable not set")
    }
    
    
    
    # use the lapply functions
    #polmean <- processmonitor(directory , pollutant, id )
    allmeans <- sapply(id, processmonitor,directory1=directory, pollutant1=pollutant)
       
    
    reducevect <- Reduce(c, allmeans)
    #totalmean
    totalmean <- mean(reducevect, na.rm = TRUE)
    totalmean
    
 
    ## Note : Do not round the result
}   ## end function pollutantmean

