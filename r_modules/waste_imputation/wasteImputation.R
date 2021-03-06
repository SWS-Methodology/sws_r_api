suppressMessages({
    library(igraph)
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(faoswsProductionImputation)
})

## NOTE (Michael): The hard coded element codes need to be replaced
##                 with element table. The production and trade
##                 element changes by item.

verbose = FALSE

if(verbose){
    startingTime = Sys.time()
    currentTime = startingTime
}


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",        
        token = "97afe028-f181-4b6c-8933-9fc7fa98c71a"
        )
    R_SWS_SHARE_PATH = getwd()
    verbose = TRUE
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
}


## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
## TODO (Michael): Change the item and element variable names after
##                 Nick has corrected the name in the database.
itemVar = "measuredItemSuaFbs"
elementVar = "measuredElementSuaFbs"
## NOTE (Michael): This module is mis-named, the estimation should
##                 corresponds to "losses". However, need to double
##                 check which element code corresponds to real
##                 losses. 
requiredElements = c("5510", "5600", "5712", "5120")
names(requiredElements) = c("production", "import", "stockWithdrawl", "loss")
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"
## NOTE (Michael): The years were selected by Klaus but we reduced it for testing.
## selectedYear = as.character(1970:2013)
selectedYear = as.character(2000:2013)

## Function to obtain all CPC item 
getFBSmeasuredItemCPC = function(){
    itemEdgeList =
        adjacent2edge(
            GetCodeTree(domain = "lossWaste",
                        dataset = "loss",
                        dimension = itemVar)
        )
    itemEdgeGraph = graph.data.frame(itemEdgeList)
    itemDist = shortest.paths(itemEdgeGraph, v = "0", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
    fbsItemCodes
}

requiredItems = getFBSmeasuredItemCPC()

## Obtain all the countries in the losses domain
requiredCountries =
    GetCodeList(domain = "lossWaste",
                dataset = "loss",
                dimension = areaVar)[type == "country", code]



## Define required functions
## ---------------------------------------------------------------------


## Function to get the 2 external world bank data sets and merge them
getLossWorldBankData = function(){
   
    infrastructureKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_infrastructure",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = requiredCountries),
                           Dimension(name = "wbIndicator",
                                     keys = "IS.ROD.PAVE.ZS"),
                           Dimension(name = "timePointYears",
                                     keys = selectedYear)
                       )
                   )

    climateKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_climate",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = requiredCountries),
                           Dimension(name = "wbIndicator",
                                     keys = c("SWS.FAO.PREC",
                                         "SWS.FAO.TEMP")),
                           Dimension(name = "timePointYears",
                                     keys = as.character(1969:2013))
                       )
                   )
    
    gdpKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_ecogrw",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = requiredCountries),
                           Dimension(name = "wbIndicator",
                                     keys = c("NY.GDP.MKTP.PP.KD",
                                         "NY.GDP.PCAP.KD")),
                           Dimension(name = "timePointYears",
                                     keys = as.character(1969:2013))
                       )
                   )

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "wbIndicator", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE)
    )

    base =
        data.table(geographicAreaM49 = character(),
                   wbIndicator = character(),
                   timePointYears = character(),
                   Value = numeric())

    merged =
        Reduce(f = function(base, key){
            rbind(base, GetData(key, pivoting = newPivot))
        }, x = list(climateKey, infrastructureKey, gdpKey), init = base)
    
    casted =
        dcast.data.table(merged,
                         geographicAreaM49 + timePointYears ~ wbIndicator,
                         value.var = "Value")
    setnames(casted,
             old = c("IS.ROD.PAVE.ZS", "NY.GDP.MKTP.PP.KD",
                 "NY.GDP.PCAP.KD", "SWS.FAO.PREC", "SWS.FAO.TEMP"),
             new = c("sharePavedRoad", "gdpPPP", "gdpPerCapita",
                 "precipitation", "temprature"))
    casted
}


imputePavedRoadData = function(lossWorldBankData){
    setkeyv(lossWorldBankData, cols = c(areaVar, yearVar))
    lossWorldBankData[, sharePavedRoad := defaultNaive(sharePavedRoad),
                      by = areaVar]
    lossWorldBankData
}

## Function to load the loss food group classification
getLossFoodGroup = function(){
    lossFoodGroup = GetTableData(schemaName = "ess", tableName = "loss_food_group")
    setnames(lossFoodGroup, old = colnames(lossFoodGroup),
             new = c("measuredItemFS", "measuredItemNameFS", "foodGroupName",
                 "foodGroup", "foodGeneralGroup", "foodPerishableGroup",
                 "measuredItemCPC"))
    lossFoodGroup[, list(measuredItemCPC, foodGroupName,
                         foodGroup, foodGeneralGroup, foodPerishableGroup)]
    lossFoodGroup
}

## Function to load the loss region classification
getLossRegionClass = function(){
    regionMapping =
        GetTableData(schemaName = "ess", tableName = "loss_region_mapping")
    setnames(regionMapping, old = colnames(regionMapping),
             new = c("geographicAreaM49", "lossRegionClass"))    
    regionMapping
}

## Function to load the national fbs dataset
getNationalFbs = function(){
    nationalFbs = GetTableData(schemaName = "ess", tableName = "national_fbs")
    setnames(nationalFbs, old = colnames(nationalFbs),
             new = c("geographicAreaM49", "timePointYears",
                 "Value_measuredElement_5510", "Value_measuredElement_5600",
                 "Value_measuredElement_5910", "Value_measuredElement_5712",
                 "Value_measuredElement_5015", "Value_measuredElement_5525",
                 "measuredItemCPC"))
    nationalFbs[, timePointYears := as.character(timePointYears)]
    nationalFbs
}

## Function to obtain the official loss data required for loss estimation
getOfficialLossData = function(){
    ## Set up the query
    dimensions =
        list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = itemVar, keys = requiredItems),
            Dimension(name = elementVar, keys = requiredElements[["loss"]]),
            Dimension(name = yearVar, keys = selectedYear)
        )

    newDataKey =
        DatasetKey(domain = "lossWaste",
                   dataset = "loss",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    query = GetData(
        key = newDataKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
    )
    ## query[, timePointYears := as.numeric(timePointYears)]

    ## TODO (Michael): Remove this when the names has been changed in
    ##                 the database.
    tmp = grep("measuredElementSuaFbs", colnames(query), value = TRUE)
    setnames(query,
             old = c("measuredItemSuaFbs", tmp),
             new = c("measuredItemCPC",
                 gsub("measuredElementSuaFbs", "measuredElement", tmp)))
    query[flagObservationStatus_measuredElement_5120 == "", ]
    
    ## NOTE (Michael): Only return official figures for estimation
    query[query[[paste0(flagObsPrefix, elementVar, "_5120")]] == "", ]
}

getAllLossData = function(){
    ## Set up the query
    dimensions =
        list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = itemVar, keys = requiredItems),
            Dimension(name = elementVar, keys = requiredElements[["loss"]]),
            Dimension(name = yearVar, keys = selectedYear)
        )

    newDataKey =
        DatasetKey(domain = "lossWaste",
                   dataset = "loss",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    query = GetData(
        key = newDataKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
    )
    ## query[, timePointYears := as.numeric(timePointYears)]

    ## TODO (Michael): Remove this when the names has been changed in
    ##                 the database.
    tmp = grep("measuredElementSuaFbs", colnames(query), value = TRUE)
    setnames(query,
             old = c("measuredItemSuaFbs", tmp),
             new = c("measuredItemCPC",
                 gsub("measuredElementSuaFbs", "measuredElement", tmp)))

    query
}

## Function to get the yield formula triplets
getYieldFormula = function(itemCode){
    condition =
        paste0("WHERE cpc_code IN (",
               paste0(shQuote(as.character(itemCode)),
                      collapse = ", "), ")")
    yieldFormula =
        GetTableData(schemaName = "ess",
                     tableName = "item_yield_elements",
                     whereClause = condition)
    setnames(yieldFormula,
             old = c("cpc_code", "element_31", "element_41",
                 "element_51", "factor"),
             new = c(itemVar, "input", "productivity",
                 "output", "unitConversion")
             )
    yieldFormula
}


getProductionElement = function(measuredItemCPC){
    condition =
        paste0("WHERE cpc_code IN (",
               paste0(shQuote(as.character(measuredItemCPC)),
                      collapse = ", "), ")")
    yieldFormula =
        GetTableData(schemaName = "ess",
                     tableName = "item_yield_elements",
                     whereClause = condition)
    yieldFormula
}

productionElements = getProductionElement(requiredItems)


## Get production data

getProductionData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    productionKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = "measuredElement",
                      keys = unique(productionElements$element_51)),
            Dimension(name = "measuredItemCPC",
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    productionPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
    )

    ## Query the data
    productionQuery = GetData(
        key = productionKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = productionPivot
    )


    ## Convert time to numeric
    ## productionQuery[, timePointYears := as.numeric(timePointYears)]
    productionQuery
}



getConsolidatedImportData = function(){
    ## We only take import quantity for the waste module
    tradeKey = DatasetKey(
        domain = "trade",
        dataset = "total_trade_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = "measuredElementTrade",
                      keys = "5600"),
            Dimension(name = "measuredItemCPC",
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeQuery = GetData(
        key = tradeKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradePivot
    )

    ## NOTE (Michael): The unit for trade is in kg while for
    ##                 production is ton, so we divide the trade by
    ##                 1000 to match production.
    tradeQuery[, Value_measuredElementTrade_5600 :=
                   computeRatio(Value_measuredElementTrade_5600, 1000)]
    
    setnames(tradeQuery,
             old = grep("measuredElementTrade",
                 colnames(tradeQuery), value = TRUE),
             new = gsub("measuredElementTrade", "measuredElement",
                 grep("measuredElementTrade",
                      colnames(tradeQuery), value = TRUE)))


    ## Convert time to numeric
    ## tradeQuery[, timePointYears := as.numeric(timePointYears)]
    tradeQuery

}


## Function to merge the national fbs data to the loss data
mergeNationalFbs = function(lossData, nationalFbs){
    lossWithNationalFbs = rbind(lossData, nationalFbs, fill = TRUE)
    lossWithNationalFbs[, fromNationalFbs :=
                            c(rep(0, NROW(lossData)), rep(1, NROW(nationalFbs)))]
    lossWithNationalFbs
}
    

## Function to merge all the required data
mergeAllLossData = function(lossData, lossWorldBankData, lossFoodGroup,
    lossRegionClass, ...){
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all.x = TRUE)
    },
           x = list(lossWorldBankData, lossFoodGroup, lossRegionClass, ...),
           init = lossData
           )
}


## Fill in non-classified food group and region.
##
## TODO (Michael): Should update the commodities which does not have a
##                 classified food group.
fillUnclassifiedFoodGroup = function(data,
    foodGroupClassification = c("foodGroupName", "foodGeneralGroup",
        "foodPerishableGroup")){
    sapply(foodGroupClassification,
           FUN = function(x){
               data[is.na(data[[x]]), `:=`(c(x), "unclassified")]
               invisible()
           }
           )
    data
}

## Fill in undefined region
##
## TODO (Michael): All countries which does not have a classified
##                 region should be mapped.
fillUnclassifiedRegion = function(data, regionClassification = "lossRegionClass"){
    sapply(regionClassification,
           FUN = function(x){
               data[is.na(data[[x]]), `:=`(c(x), "unclassified")]
               invisible()
           }
           )
    data    
}

## This function fills in data requirments which are not currently in the data
dataHack = function(data){
    ## importVar =
    ##     paste0(valuePrefix, elementVar, "_", requiredElements[["import"]])
    ## prodVar =
    ##     paste0(valuePrefix, elementVar, "_", requiredElements[["production"]])

    ## TODO (Michael): This has to be changed when the naming are
    ## corrected.
    prodVar = "Value_measuredElement_5510"
    importVar = "Value_measuredElement_5600"
    
    
    ## HACK (Michael): This is a hack to simulate trade data
    data[, `:=`(c(importVar),
                abs(rnorm(.N,
                          mean(data[[prodVar]], na.rm = TRUE), 
                          sd(data[[prodVar]], na.rm = TRUE)))),
         by = c("geographicAreaM49", "measuredItemCPC")]
    ## HACK (Michael): Since trade and stock variation data are not
    ##                 available, we simulate the loss rate data as well.
    data[sample(1:NROW(data), NROW(data) * 0.5),
         lossRatio := runif(.N, min = 0.1, max = 0.9)]
    data[!is.finite(lossRatio) | lossRatio < 0, lossRatio := NA]
    data
}


## Function to calculate the ratio
##
calculateLossRatio = function(data,
    productionVar =
        paste0(valuePrefix, "_", requiredElements["production"]),
    importVar =
        paste0(valuePrefix, elementVar, "_", requiredElements["import"]),
    stockWithdrawlVar =
        paste0(valuePrefix, elementVar, "_", requiredElements["stockWithdrawl"]),
    lossVar = paste0(valuePrefix, elementVar, "_", requiredElements["loss"])){

    data[, lossBase :=  rowSums(.SD[, c(productionVar, importVar), with = FALSE],
                        na.rm = TRUE)]

    data[, lossRatio := computeRatio(data[[lossVar]], lossBase)]
    data
}



## NOTE (Michael): Try to remove the hard code
##
## Function to perform final manipulation
##
preEstimationProcessing = function(data){
    ## Convert variables to factor for modelling
    
    ## NOTE (Michael): Stock variation is assumed to be zero.
    data[, Value_measuredElement_5712 := as.numeric(0)]

    ## NOTE (Michael): Need to change year to numeric here.
    data[, timePointYears := as.numeric(timePointYears)]

    factorVariables = c("geographicAreaM49", "measuredItemCPC", "foodGroupName",
                  "foodGeneralGroup", "foodPerishableGroup", "lossRegionClass")
    
    data[, `:=`(c(factorVariables),
                lapply(data[, factorVariables, with = FALSE], as.factor))]


    ## TODO (Michael): This need to be reverted when the name is changed
    ## productionVar =
    ##     paste0(valuePrefix, elementVar, "_", requiredElements["production"])    
    ## importVar =
    ##     paste0(valuePrefix, elementVar, "_", requiredElements["import"])
    productionVar = "Value_measuredElement_5510"
    importVar = "Value_measuredElement_5600"
    
    ## If import and production are missing, then assume they are zero.
    data[is.na(data[[importVar]]), `:=`(c(importVar), 0)]
    data[is.na(data[[productionVar]]), `:=`(c(productionVar), 0)]

    
    ## Compute import to production ratio.
    ##
    ## NOTE (Michael): Have no idea why the function fails when I
    ##                 change the "Value_measuredElement_5510" to
    ##                 productionVar.
    ##

    data[, importToProductionRatio :=
             computeRatio(.SD[["Value_measuredElement_5600"]],
                          .SD[["Value_measuredElement_5510"]])]
    
    ## NOTE (Michael): I don't know why the time should be scaled.
    data[, scaledTimePointYears := timePointYears - 1960]
    ## data[is.na(fromNationalFbs), fromNationalFbs := 0]

    ## NOTE (Klaus): GDP per capita over 25000 are truncated and
    ##               assume it does not have any relevant effects on
    ##               the changes in losses.
    data[gdpPerCapita > 25000, gdpPerCapita := 25000]

    ## NOTE (Klaus): Assume the food group level of meat is the same as
    ##               meat and fishes.
    levels(data$foodGroupName) =
        with(data,
             ifelse(levels(foodGroupName) == "meat", "meat and fish",
                    levels(foodGroupName)))

    data
}


## Function to create the desired estimation sample
##
## NOTE (Michael): This is currently obsolete
splitLossData = function(data, estimationSubset){

    ## NOTE (Michael): This is hard coded selection by Klaus
    if(missing(estimationSubset))
        estimationSubset =
            expression(which(timePointYears > 1969 & 
                             importToProductionRatio < 1 &
                             lossRatio != 0 &
                             geographicAreaM49 != "170" &
                             foodGeneralGroup == "primary" &
                             !is.na(gdpPerCapita) &
                             !is.na(sharePavedRoad)))
   
    estimationSubsetIndex = eval(substitute(estimationSubset), data)

    estimationData = data[estimationSubsetIndex, ]
    predictionData = data[-estimationSubsetIndex, ]
    list(estimationData = estimationData, predictionData = predictionData)
}




## Function to estimate the loss regression
lossRegression = function(estimationData){

    ## REGESSION (1): Item-specific dummies

    itemSpecificLoss.lm =
        lm(I(log(lossRatio+0.05)) ~ measuredItemCPCFactor + lossRegionClassFactor +
           scaledTimePointYears + foodPerishableGroupFactor + sharePavedRoad +
           sharePavedRoad:foodPerishableGroupFactor + I(log(gdpPerCapita)) +
           I(log(gdpPerCapita)^2) +
           I(log(gdpPerCapita)):foodPerishableGroupFactor +
           I(log(gdpPerCapita)^2):foodPerishableGroupFactor +
           fromNationalFbs, data = estimationData)


    ## REGESSION (2): No item-specific dummies.
    ##
    ## This regression is performed for impute losses for commodities
    ## for which no (or very few) observations are available.
    ##
    ## Use item group-specific dummies. (Both, item and group dummies,
    ## cannot be used at the same scaledTimePointYears.)

    foodGroupLoss.lm =
        lm(I(log(lossRatio + 0.05)) ~ foodGroupNameFactor + lossRegionClassFactor +
           scaledTimePointYears + foodPerishableGroupFactor + sharePavedRoad +
           sharePavedRoad:foodPerishableGroupFactor + I(log(gdpPerCapita)) +
           I(log(gdpPerCapita)^2) +
           I(log(gdpPerCapita)):foodPerishableGroupFactor +
           I(log(gdpPerCapita)^2):foodPerishableGroupFactor +
           fromNationalFbs, data = estimationData)

    list(itemSpecificModel = itemSpecificLoss.lm,
         foodGroupModel = foodGroupLoss.lm)
}


## Function to take the model and make imputation
lossModelPrediction = function(model, predictionData, lossRatio){
    imputedData = copy(predictionData)

    ## TODO (Michael): Need impose the criteria where at least 3
    ##                 official observation for losses are required
    ##                 for imputation.
    
    ## Split the data for prediction
    splitedData = split(imputedData, imputedData[["measuredItemCPC"]])

    predicted =
        lapply(splitedData,
               FUN = function(x){
                   pred = copy(x)
                   ## Prediction based on  REGRESSION (1)
                   prediction1 = try(predict(model[[1]], newdata = x),
                                     silent = TRUE)
                   if(!inherits(prediction1, "try-error")){
                       pred[, itemPredict := prediction1]
                   } else {
                       pred[, itemPredict := NA]
                   }
                   ## Prediction based on  REGRESSION (2)
                   prediction2 = try(predict(model[[2]], newdata = x),
                                     silent = TRUE)
                   if(!inherits(prediction2, "try-error")){
                       pred[, groupPredict := prediction2]
                   } else {
                       pred[, groupPredict := NA]
                   }
                   pred[, finalPredict := as.numeric(itemPredict)]
                   pred[is.na(finalPredict),
                        finalPredict := as.numeric(groupPredict)]
                   pred
               })
    imputed = Reduce(f = rbind, x = predicted)
    imputed[, `:=`(c(paste0(valuePrefix, "measuredElement_",
                                requiredElements[["loss"]])),
                   finalPredict * lossBase)]
    imputed
}
           
## Function to select the required variable and dimension for the
## estimation of the model.
selectRequiredVariable = function(data){
    data[foodGeneralGroup == "primary",
         list(geographicAreaM49, measuredItemCPC, timePointYears,
              Value_measuredElement_5120,
              Value_measuredElement_5510,
              Value_measuredElement_5600,
              flagObservationStatus_measuredElement_5120,
              flagMethod_measuredElement_5120, gdpPerCapita,
              gdpPPP,
              sharePavedRoad, lossBase, lossRatio, 
              measuredItemCPC, foodGroupName, foodGeneralGroup,
              foodPerishableGroup, lossRegionClass,
              scaledTimePointYears)]
}

selectSaveData = function(data, rawLossData){
    saveSelection =
        data[, intersect(colnames(data), colnames(rawLossData)), with = FALSE]
    saveSelection[, geographicAreaM49 := as.character(geographicAreaM49)]
    saveSelection[, measuredItemCPC := as.character(measuredItemCPC)]
    saveSelection[, timePointYears := as.character(timePointYears)]

    ## TODO (Michael): This is a hack, change the name back later
    setnames(saveSelection, old = "measuredItemCPC", new = "measuredItemSuaFbs")
    setnames(saveSelection,
             old = grep("measuredElement", colnames(saveSelection), value = TRUE),
             new = gsub("measuredElement", "measuredElementSuaFbs",
                 grep("measuredElement", colnames(saveSelection), value = TRUE)))
    saveSelection
}

## Function to save the data back
##
SaveLossData = function(data){
    ## SaveData(domain = "lossWaste", dataset = "loss",
    ##          data = data, normalized = FALSE)
    ##
    ## Using the temporary SaveData function to save back large
    ## datasets.
    SaveData(domain = "lossWaste", dataset = "loss",
             data = data, normalized = FALSE)    
}


## The following function just takes the model which were already
## estimated by Klaus
lossItemImputation = function(data, model){
    predictIndex =
        with(data,
             which(as.character(measuredItemCPC) %in%
                   model$xlevels$measuredItemCPC &
                       as.character(lossRegionClass) %in%
                   model$xlevel$lossRegionClass  &
                       as.character(foodPerishableGroup) %in%
                   model$xlevel$foodPerishableGroup)
             )
    predictedLossRatio = rep(NA, NROW(data))
   
    predictedLossRatio[predictIndex] =
        exp(predict(model, data[predictIndex, ])) - 0.05
    predictedLossRatio
}

lossGroupImputation = function(data, model){
    predictIndex =
        with(data,
             which(as.character(foodGroupName) %in%
                   model$xlevels$foodGroupName &
                       as.character(lossRegionClass) %in%
                   model$xlevel$lossRegionClass  &
                       as.character(foodPerishableGroup) %in%
                   model$xlevel$foodPerishableGroup)
             )
    predictedLossRatio = rep(NA, NROW(data))
   
    predictedLossRatio[predictIndex] =
        exp(predict(model, data[predictIndex, ])) - 0.05
    predictedLossRatio
}

lossImputation = function(data, itemModel, foodGroupModel){
    prediction = copy(data)
    
    prediction$itemPredictedLoss =
        lossItemImputation(prediction, itemModel)

    prediction$groupPredictedLoss =
        lossGroupImputation(prediction, foodGroupModel)


    prediction[, finalPrediction :=
                   ifelse(!is.na(itemPredictedLoss),
                          itemPredictedLoss/100 * lossBase,
                          groupPredictedLoss/100 * lossBase)]
    prediction[finalPrediction < 0, finalPrediction := NA]
        
    prediction[is.na(Value_measuredElement_5120) & !is.na(finalPrediction),
               `:=`(c("Value_measuredElement_5120",
                      "flagObservationStatus_measuredElement_5120",
                      "flagMethod_measuredElement_5120"),
                    list(finalPrediction, "I", "e"))]

    ## Also impute for those that were previously estimated or imputed.
    prediction[flagObservationStatus_measuredElement_5120 %in% c("I", "E", "T") &
               !is.na(finalPrediction),
               `:=`(c("Value_measuredElement_5120",
                      "flagObservationStatus_measuredElement_5120",
                      "flagMethod_measuredElement_5120"),
                    list(finalPrediction, "I", "e"))]
    prediction
}

## The full waste estimation, imputation process.
## ---------------------------------------------------------------------
wasteImputation =
    try(
        {
    ## Build the final data set
    finalLossData =
        {
            if(verbose){
                cat("Extracting raw data\n")
                currentTime = Sys.time()
            }
            ## lossData <<- getOfficialLossData()
            lossData <<- getAllLossData()
            productionData <<- getProductionData()
            consolidatedImportData <<- getConsolidatedImportData()
    
            ## NOTE (Michael): We don't take data from national FBS
            ##                 anymore, it does not help with prediction
            ##                 and also the data is questionable.
            
            ## nationalFbs <<- getNationalFbs()
            ## lossDataWithNationalFbs <<- mergeNationalFbs(lossData, nationalFbs)
            lossWorldBankData <<-
                getLossWorldBankData() %>%
                    imputePavedRoadData(lossWorldBankData = .)
            lossFoodGroup <<- getLossFoodGroup()
            lossRegionClass <<- getLossRegionClass()
            gc()
            list(lossData = lossData,
                 productionData = productionData,
                 ## consolidatedImportData = consolidatedImportData,
                 lossWorldBankData = lossWorldBankData,
                 lossFoodGroup = lossFoodGroup,
                 lossRegionClass = lossRegionClass)
        } %>%
            {
                if(verbose){
                    endTime = Sys.time()
                    timeUsed = endTime - currentTime
                    cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
                    currentTime = endTime
                    cat("Merge All Loss Required Data\n")
                }
                with(.,
                     mergeAllLossData(lossData, lossWorldBankData, lossFoodGroup,
                                      lossRegionClass, productionData,
                                      consolidatedImportData)
                     )
            }
    
    ## Build the data
    if(verbose){
        endTime = Sys.time()
        timeUsed = endTime - currentTime
        cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
        currentTime = endTime
        cat("Process Merged Data\n")
    }
    trainPredictData =
        copy(finalLossData) %>%
        fillUnclassifiedFoodGroup %>%
        fillUnclassifiedRegion %>%
        preEstimationProcessing %>%
        calculateLossRatio(data = .,
                           productionVar = "Value_measuredElement_5510",
                           importVar = "Value_measuredElement_5600",
                           stockWithdrawlVar = "Value_measuredElement_5712",
                           lossVar = "Value_measuredElement_5120") %>%
        selectRequiredVariable
    
    ## Predict and save the predicted data back
    if(verbose){
        endTime = Sys.time()
        timeUsed = endTime - currentTime
        cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
        currentTime = endTime
        cat("Impute Loss\n")
    }
    predictedLossData = 
        copy(trainPredictData) %T>%
            {
                ## Load the model
                itemModelPath = paste0(R_SWS_SHARE_PATH, "/itemModel")
                foodGroupModelPath = paste0(R_SWS_SHARE_PATH, "/foodGroupModel")
                ## Here we read the reconstructed model of Klaus
                itemModel <<- readRDS(itemModelPath)
                foodGroupModel <<- readRDS(foodGroupModelPath)
            } %>%
        lossImputation(data = .,
                       itemModel = itemModel,
                       foodGroupModel = foodGroupModel) %<>%
        selectSaveData(data = ., rawLossData = lossData)
    
    if(verbose){
        endTime = Sys.time()
        timeUsed = endTime - currentTime
        cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
        currentTime = endTime
        cat("Save Imputed Loss Data Back\n")
    }
    
    predictedLossData %>%
        SaveLossData(data = .)
    
    if(verbose){
        endTime = Sys.time()
        timeUsed = endTime - currentTime
        cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
        currentTime = endTime
    }
})

if(inherits(wasteImputation, "try-error")){
    print("Imputation Module Failed")
} else {
    print("Imputation Module Executed Successfully")
}
