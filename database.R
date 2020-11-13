##--------------------------------
## Create the database 
## PS
## Notes: 
##--------------------------------


library(XML)

data <-xmlParse("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=0")
xml_data <- xmlToList(data)

stock_names <- unlist(lapply(xml_data, FUN = function(z){z$StockKeyLabel}))


## name change

names  <- read.csv("StockKeyOldNew.csv")

database <- list()

for (l in 1:nrow(names)){
data <- list()
data <- xmlParse("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=0")
xml_data <- xmlToList(data)
if( length(xml_data)>1){
stock_names <- unlist(lapply(xml_data, FUN = function(z){z$StockKeyLabel}))
name  <- as.character(names$Old[l, drop = TRUE])

newname  <- names$New[names$Old==name, drop = TRUE]


idx <- which(stock_names == name)
idxn  <- which(stock_names == newname)

idx <- c(idxn,idx)

akey <- sapply(idx, function(z){xml_data[[z]]$AssessmentKey})

res <- list()

## extract the stockeylabel, assessmentyear, F, FMSY, SSB, MSYBtrigger

for(i in 1:length(akey)){
    print(i)
    file <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getStockDownloadData?AssessmentKey=", akey[i])
    data <- xmlParse(file)
    xml_data <- xmlToList(data)
    if( length(xml_data)>1){
    res_yr <- NULL
    for(j in 1:length(xml_data)){
        tmp <- data.frame(
            StockKeyLabel = xml_data[[j]]$StockKeyLabel,
            AssessmentYear = as.numeric(xml_data[[j]]$AssessmentYear),
            Year = as.numeric(xml_data[[j]]$Year),
            F = ifelse(!is.null(xml_data[[j]]$FishingPressure),
                       as.numeric(xml_data[[j]]$FishingPressure),
                       NA),
            FMSY = ifelse(!is.null(xml_data[[j]]$FMSY),
                          as.numeric(xml_data[[j]]$FMSY), NA),
        
        FishingPressureDescription = ifelse(!is.null(xml_data[[j]]$FishingPressureDescription),
                                            as.character(xml_data[[j]]$FishingPressureDescription), NA),
    
          FishingPressureUnits =  ifelse(!is.null(xml_data[[j]]$FishingPressureUnits),
                                            as.character(xml_data[[j]]$FishingPressureUnits), NA),

            SSB = ifelse(!is.null(xml_data[[j]]$StockSize),
                       as.numeric(xml_data[[j]]$StockSize),
                       NA),
        StockSizeDescription =  ifelse(!is.null(xml_data[[j]]$StockSizeDescription),
                       as.character(xml_data[[j]]$StockSizeDescription),
                       NA),
        
             Low_Recruitment = ifelse(!is.null(xml_data[[j]]$Low_Recruitment),
                       as.numeric(xml_data[[j]]$Low_Recruitment),
                       NA),
             Recruitment = ifelse(!is.null(xml_data[[j]]$Recruitment),
                       as.numeric(xml_data[[j]]$Recruitment),
                       NA),
             High_Recruitment = ifelse(!is.null(xml_data[[j]]$High_Recruitment),
                       as.numeric(xml_data[[j]]$High_Recruitment),
                       NA),
            
            Blim = ifelse(!is.null(xml_data[[j]]$Blim),
                          as.numeric(xml_data[[j]]$Blim), NA),
        
            MSYBtrigger = ifelse(!is.null(xml_data[[j]]$MSYBtrigger),
                                 as.numeric(xml_data[[j]]$MSYBtrigger), NA) ,
        
        Report = ifelse(!is.null(xml_data[[j]]$Report),
                                 as.character(xml_data[[j]]$Report), NA))
        res_yr <- rbind(res_yr, tmp)
    }
    res <- rbind(res, res_yr)
}

}
#############  DATABASE  ############################
res <- cbind(newname, res)


database <- rbind(database, res)
}}

write.csv(database, "database.csv")

