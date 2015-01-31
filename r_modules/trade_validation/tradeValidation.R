## NOTE (Michael): Need to request flag for raw data, the flag are missing.

suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
})

## Year should be a paramameter selected.
selectedYear = "2010"


## Setting up variables
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"
reverseTradePrefix = "reverse_"


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "978d8d5f-94b0-43f5-9979-b77485afbdfb"
        )
}

## Get all reporting country codes
allReportingCountryCode =
    GetCodeList(domain = "trade",
                dataset = "ct_raw_tf",
                dimension = reportingCountryVar)$code

## Get all partner country codes
allPartnerCountryCode =
    GetCodeList(domain = "trade",
                dataset = "ct_raw_tf",
                dimension = partnerCountryVar)$code


## Get relevant element codes and set names
##
## NOTE (Michael): Lets work with set elements first, then expand them
##                 when we have the formula table.

allElementTable =
    GetCodeList(domain = "trade",
                dataset =  "ct_raw_tf",
                dimension = "measuredElementTrade")
## elementCode = c("5600", "5612", "5621", "5622", "5630", "5900", "5912", "5921",
##     "5922", "5930")
## elementCodeName = c("importQuantity", "reimportQuantity", "importValue",
##     "reimportValue", "importUnitValue", "exportQuantity", "reexportQuantity",
##     "exportValue", "reexportValue", "exportUnitValue")

## NOTE (Michael): This table is for cereal only, need Nick to provide
##                 the formula table.
elementTable =
    data.frame(type = c("quantity", "value", "unit_value"),
               import = c("5600", "5621", "5630"),
               reimport = c("5612", "5622", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5922", NA))


assignElementName = function(elementTable){
    meltedElementTable = na.omit(melt(elementTable, id.vars = "type"))
    elementName = with(meltedElementTable, paste(variable, type, sep = "_"))
    elementCode = paste0(valuePrefix, elementVar, "_", meltedElementTable$value)
    mapply(FUN = function(name, colname){
        assign(x = name, value = colname, envir = .GlobalEnv)
        assign(x = name, value = colname, envir = .GlobalEnv)        
        assign(x = paste0(reverseTradePrefix, name),
               value = paste0(reverseTradePrefix, colname),
               envir = .GlobalEnv)        
    }, name = elementName, colname = elementCode)

    ## mapply(FUN = function(name, colname){
    ##     assign(x = paste0(reverseTradePrefix, name),
    ##            value = paste0(reverseTradePrefix, colname),
    ##            envir = .GlobalEnv)
    ## }, name = elementName, colname = elementCode)        
}

assignElementName(elementTable)


## assign name globally for convinience
## mapply(FUN = function(name, colname){
##     assign(x = name, value = colname, envir = .GlobalEnv)
## }, name = elementCodeName,
##        colname = paste0(valuePrefix, elementVar, "_", elementCode))

## TODO (Michael): Need to do one for reverse trade


getComtradeRawData = function(measuredItemHSCode){
    dimensions =
        list(Dimension(name = "reportingCountryM49",
                       keys = allReportingCountryCode),
             Dimension(name = "partnerCountryM49", keys = allPartnerCountryCode),
             Dimension(name = "measuredItemHS",
                       keys = measuredItemHSCode),
             Dimension(name = "measuredElementTrade",
                       keys = with(elementTable,
                           unique(na.omit(c(import, reimport,
                                            export, reexport))))),
             Dimension(name = "timePointYears",
                       keys = selectedYear))
    
    newKey = DatasetKey(domain = "trade", dataset = "ct_raw_tf",
        dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "reportingCountryM49", ascending = TRUE),
        Pivoting(code = "partnerCountryM49", ascending = TRUE),
        Pivoting(code = "measuredItemHS", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    comtradeRaw = GetData(key = newKey, normalized = FALSE, pivoting = newPivot)
    comtradeRaw[,timePointYears := as.numeric(timePointYears)]
    setkeyv(comtradeRaw, cols = c("reportingCountryM49", "partnerCountryM49",
                             "measuredItemHS", "timePointYears"))
    ## NOTE (Michael): The meta data shows which procedure was done to
    ##                 obtain the value. This is replaced by flag

    comtradeRaw[, `:=`(c(grep(valuePrefix, colnames(comtradeRaw), value = TRUE)),
                       lapply(c(grep(valuePrefix, colnames(comtradeRaw),
                                     value = TRUE)),
                              FUN = function(x) as.numeric(.SD[[x]])))]
    comtradeRaw
}

addRetradeToTrade = function(data, importQuantity, reimportQuantity,
    exportQuantity, reexportQuantity, importValue, reimportValue,
    exportValue, reexportValue){
    missingCol =
        setdiff(c(importQuantity, reimportQuantity, exportQuantity,
                  reexportQuantity, importValue, reimportValue, exportValue,
                  reexportValue),
                colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    data[, `:=`(c(importQuantity, exportQuantity, importValue, exportValue),
                list(ifelse(apply(is.na(.SD[, c(importQuantity, reimportQuantity),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(importQuantity, reimportQuantity),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(exportQuantity, reexportQuantity),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(exportQuantity, reexportQuantity),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(importValue, reimportValue),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(importValue, reimportValue),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(exportValue, reexportValue),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(exportValue, reexportValue),
                                        with = FALSE], na.rm = TRUE))))]
    data[, `:=`(c(reimportQuantity, reexportQuantity,
                  reimportValue, reexportValue), NA)]
    ## NOTE (Michael): What do we do with the retrades in the data base?
    data
}


removeSelfTrade = function(data, reportingCountry, partnerCountry){
    noSelfTrade = data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
    noSelfTrade
}


removeInconsistentQuantityValue = function(data, quantity, value){
    data[data[[quantity]] == 0 & data[[value]] != 0,
         `:=`(c(quantity), NA)]
    data[data[[quantity]] != 0 & data[[value]] == 0,
         `:=`(c(value), NA)]
    data
}


mirrorTrade = function(data, reportingCountry, partnerCountry, reverseTradePrefix,
                       valueColumns, flagColumns){
    base = copy(data)
    tmp = data[, !flagColumns, with = FALSE]

    ## create reversion of the reporting and partner country, and
    ## create the reverse trade flow for mirroring process.
    reverseReportingName = paste0(reverseTradePrefix, reportingCountry)
    reversePartnerName = paste0(reverseTradePrefix, partnerCountry)
    setnames(tmp, old = c(reportingCountry, partnerCountry),
             new = c(reverseReportingName, reversePartnerName))    
    base[, `:=`(c(reverseReportingName, reversePartnerName),
                list(.SD[[partnerCountry]], .SD[[reportingCountry]]))]
    setnames(tmp, old = valueColumns,
             new = paste0(reverseTradePrefix, valueColumns))

    ## Merge the two dataset to obtain the full global trade flow
    mirroredTrade = merge(base, tmp,
        by = intersect(colnames(base), colnames(tmp)), all = TRUE)
    ## TODO (Michael): Values should be filled here to ensure the base
    ##                 is symmetrical for validation.
    mirroredTrade[is.na(mirroredTrade[[reportingCountry]]),
                `:=`(c(reportingCountry), .SD[[reversePartnerName]])]
    mirroredTrade[is.na(mirroredTrade[[partnerCountry]]),
                `:=`(c(partnerCountry), .SD[[reverseReportingName]])]

    ## Mirror the value based on the element table
    ##
    ## TODO (Michael): Assign flag when mirroring
    mirroredTrade[is.na(mirroredTrade[[import_quantity]]) &
                  !is.na(mirroredTrade[[reverse_export_quantity]]),
                  `:=`(c(import_quantity,
                         gsub(valuePrefix, flagPrefix, import_quantity)),
                       list(.SD[[reverse_export_quantity]], "mr"))]
    mirroredTrade[is.na(mirroredTrade[[import_value]]) &
                  !is.na(mirroredTrade[[reverse_export_value]]),
                  `:=`(c(import_value,
                         gsub(valuePrefix, flagPrefix, import_value)),
                       list(.SD[[reverse_export_value]], "mr"))]

    mirroredTrade[is.na(mirroredTrade[[export_quantity]]) &
                  !is.na(mirroredTrade[[reverse_import_quantity]]),
                  `:=`(c(export_quantity,
                         gsub(valuePrefix, flagPrefix, export_quantity)),
                       list(.SD[[reverse_import_quantity]], "mr"))]
    mirroredTrade[is.na(mirroredTrade[[export_value]]) &
                  !is.na(mirroredTrade[[reverse_import_value]]),
                  `:=`(c(export_value,
                         gsub(valuePrefix, flagPrefix, export_value)),
                       list(.SD[[reverse_import_value]], "mr"))]

    
    ## NOTE (Michael): Flags are not needed for the reverse trade
    mirroredTrade[is.na(mirroredTrade[[reverse_import_quantity]]) &
                  !is.na(mirroredTrade[[export_quantity]]),
                  `:=`(c(reverse_import_quantity), list(.SD[[export_quantity]]))]
    mirroredTrade[is.na(mirroredTrade[[reverse_import_value]]) &
                  !is.na(mirroredTrade[[export_value]]),
                  `:=`(c(reverse_import_value), list(.SD[[export_value]]))]

    
    mirroredTrade[is.na(mirroredTrade[[reverse_export_quantity]]) &
                  !is.na(mirroredTrade[[import_quantity]]),
                  `:=`(c(reverse_export_quantity), list(.SD[[import_quantity]]))]
    mirroredTrade[is.na(mirroredTrade[[reverse_export_value]]) &
                  !is.na(mirroredTrade[[import_value]]),
                  `:=`(c(reverse_export_value), list(.SD[[import_value]]))]
    mirroredTrade
}


calculateUnitValue = function(data, importUnitValue, importTradeValue,
    importTradeQuantity, exportUnitValue, exportTradeValue, exportTradeQuantity){
    missingCol = setdiff(c(importUnitValue, importTradeValue, importTradeQuantity,
        exportUnitValue, exportUnitValue, exportTradeQuantity), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    
    data[, `:=`(c(importUnitValue, exportUnitValue),
                list(computeRatio(get(importTradeValue),
                                  get(importTradeQuantity)),
                computeRatio(get(exportTradeValue),
                             get(exportTradeQuantity))))]
    data
}



validationByMirrorValue = function(value, mirrorValue, ratioBoundary,
    log = TRUE, plot = FALSE){
    if(log){
        value = log(value)
        mirrorValue = log(mirrorValue)
    }
    pr = prcomp(~value + mirrorValue)
    slope = pr$rotation[2,1] / pr$rotation[1,1]
    intercept = pr$center[2] - slope*pr$center[1]
    valueRatio = (mirrorValue - intercept)/value

    ## Ratios which are outside the range.
    badRatio = which(valueRatio > slope * ratioBoundary |
                     valueRatio < slope/ratioBoundary)
    svec = c(1, slope)
    ## projection on to the orthorgonal line
    valueBasedOnExpectedRatio  =
        ((matrix(c(value, mirrorValue - intercept), nc = 2) %*% svec)/
            c(svec %*% svec)) %*% matrix(svec, nc = 2)
    ## Assing the new values to those that lies out side the ratio range
    newValue = value
    newMirrorValue = mirrorValue
    newValue[badRatio] = valueBasedOnExpectedRatio[badRatio, 1]
    newMirrorValue[badRatio] = valueBasedOnExpectedRatio[badRatio, 2] + intercept
    if(plot){
        plot(value, mirrorValue, pch = 19, col = "red")
        abline(a = intercept, b = slope)
        abline(a = intercept, b = slope * ratioBoundary, col = "red")
        abline(a = intercept, b = slope/ratioBoundary, col = "red")        
        points(newValue, newMirrorValue, col = "blue", pch = 19)
    }
    if(log){
        newValue = exp(newValue)
        newMirrorValue = exp(newMirrorValue)
    }
    ## list(intercept = intercept, slope= slope, newValue = newValue,
    ##      newMirrorValue = newMirrorValue)
    list(newValue, newMirrorValue)
}

validationByRange = function(value, log = TRUE){
    if(log){
        newValue = log(value)
    } else {
        newValue = value
    }
    q = quantile(value, probs = c(0.25, 0.75), na.rm = TRUE)
    min = q[1] - 1.5 * diff(q)
    max = q[2] + 1.5 * diff(q)
    badValue = which(value > max | value < min)
    newValue[badValue] = median(newValue[-badValue], na.rm = TRUE)
    ## list(value = value, newValue = newValue)
    if(log){
        return(exp(newValue))
    } else {
        return(newValue)
    }
}

## TODO (Michael): Replace only values that are difference, and also
##                 assign flag.
validation = function(data, value, mirrorValue, ratioBoundary = 3,
    log = TRUE, plot = FALSE){
    valid = copy(data)
    valid[, `:=`(c(value, mirrorValue),
                 validationByMirrorValue(.SD[[value]], .SD[[mirrorValue]],
                                         ratioBoundary = ratioBoundary,
                                         log = log,
                                         plot = plot))]
    valid[, `:=`(c(value), validationByRange(value = .SD[[value]], log = log))]
    valid[, `:=`(c(mirrorValue),
                 validationByRange(value = .SD[[mirrorValue]], log = log))]
    valid
}

## TODO (Michael): assign flag.
imputeUnitValue = function(data, unitValue, mirrorUnitValue){
    imputed = copy(data)
    imputed[is.na(imputed[[unitValue]]) & !is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(unitValue), .SD[[mirrorUnitValue]])]
    imputed[is.na(imputed[[mirrorUnitValue]]) & !is.na(imputed[[unitValue]]),
            `:=`(c(mirrorUnitValue), .SD[[unitValue]])]
    imputed[is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(mirrorUnitValue), median(.SD[[mirrorUnitValue]], na.rm = TRUE))]
    imputed[is.na(imputed[[unitValue]]),
            `:=`(c(unitValue), median(.SD[[unitValue]], na.rm = TRUE))]
    imputed
}

## TODO (Michael): Assign flag.
updateTradeQuantity = function(data, unitValue, value, quantity){
    updatedTradeQuantity = copy(data)
    updatedTradeQuantity[!is.na(updatedTradeQuantity[[quantity]]),
                    `:=`(c(quantity), get(value)/get(unitValue))]
    updatedTradeQuantity    
}


calculateReliability = function(data, import, export, reverseImport,
    reverseExport, reportingCountry, partnerCountry, pctTolerance){

    ## TODO (Michael): Discard worst discrepancy
    reliability =
        data[, (sum(abs(.SD[[import]] - .SD[[reverseExport]])/
                        .SD[[import]] <= pctTolerance, na.rm = TRUE) +
               (sum(abs(.SD[[export]] - .SD[[reverseImport]])/
                        .SD[[export]] <= pctTolerance, na.rm = TRUE)))/
                            (2 * .N),
             by = reportingCountry]
    setnames(reliability, old = "V1", new = "reportingReliability")
    reportingReliability = merge(data, reliability, by = reportingCountry,
                                 all.x = TRUE)
    setnames(reliability, old = c(reportingCountry, "reportingReliability"),
             new = c(partnerCountry, "partnerReliability"))
    finalReliability = merge(reportingReliability, reliability,
        by = partnerCountry, all.x = TRUE)
    finalReliability
}



balanceTradeQuantity = function(data, import, export, reverseImport, reverseExport,
    reportingReliability, partnerReliability, pctTolerance){
    balanced = copy(data)
    balanced[reportingReliability < partnerReliability &
             (abs(balanced[[import]] - balanced[[reverseExport]])/
                  balanced[[import]] > pctTolerance),
             `:=`(c(import), get(reverseExport))]
    balanced[reportingReliability < partnerReliability &
             (abs(balanced[[export]] - balanced[[reverseImport]])/
                  balanced[[export]]> pctTolerance),
             `:=`(c(export), get(reverseImport))]
    balanced
}



updateTradeValue = function(data, unitValue, value, quantity){
    updatedTradeValue = copy(data)
    updatedTradeValue[!is.na(updatedTradeValue[[value]]),
                    `:=`(c(value), get(quantity) * get(unitValue))]
    updatedTradeValue    
}


## TODO (Michael): Need to change the dataset name
saveMirroredTradeData = function(data, originalData){
    mirroredData = data[, colnames(originalData), with = FALSE]
    mirroredData[, timePointYears := as.character(timePointYears)]
    SaveData(domain = "trade", dataset = "completed_tf",
             data = mirroredData, normalized = FALSE)
}

## TODO (Michael): Need to change the dataset name
saveValidatedTradeData = function(data, originalData){
    validatedData = data[, colnames(originalData), with = FALSE]
    validatedData[, timePointYears := as.character(timePointYears)]
    SaveData(domain = "trade", dataset = "completed_tf",
             data = validatedData, normalized = FALSE)
}


selectedItems = swsContext.datasets[[1]]@dimensions$measuredItemHS@keys
i = "1001"
for(i in selectedItems){
    rawData =
        getComtradeRawData(measuredItemHSCode = i) %>%
        removeSelfTrade(data = ., reportingCountry = reportingCountryVar,
                        partnerCountry = partnerCountryVar) %>%
        removeInconsistentQuantityValue(data = ., quantity = import_quantity,
                                        value = import_value) %>%
        removeInconsistentQuantityValue(data = ., quantity = export_quantity,
                                        value = export_value) %>% 
        addRetradeToTrade(data = .,
                          importQuantity = import_quantity,
                          reimportQuantity = reimport_quantity,
                          exportQuantity = export_quantity,
                          reexportQuantity = reexport_quantity,
                          importValue = import_value,
                          reimportValue = reimport_value,
                          exportValue = export_value,
                          reexportValue = reexport_value)

    ## Mirrored the data and calculate unit value
    mirroredData =
        rawData %>%
        mirrorTrade(data = .,
                    reportingCountry = reportingCountryVar,
                    partnerCountry = partnerCountryVar,
                    reverseTradePrefix = reverseTradePrefix,
                    valueColumns = grep(valuePrefix, colnames(.), value = TRUE),
                    flagColumns = grep(flagPrefix, colnames(.), value = TRUE)) %>%
        calculateUnitValue(data = .,
                           importUnitValue = import_unit_value,
                           importTradeValue = import_value,
                           importTradeQuantity = import_quantity,
                           exportUnitValue = export_unit_value,
                           exportTradeValue = export_value,
                           exportTradeQuantity = export_quantity) %>%
        calculateUnitValue(data = .,
                           importUnitValue = reverse_import_unit_value,
                           importTradeValue = reverse_import_value,
                           importTradeQuantity = reverse_import_quantity,
                           exportUnitValue = reverse_export_unit_value,
                           exportTradeValue = reverse_export_value,
                           exportTradeQuantity = reverse_export_quantity)

    ## Save mirrored data back
    mirroredData %>%
        saveMirroredTradeData(data = ., originalData = rawData)

    ## Validated the data
    validatedData =
        mirroredData %>%
        validation(data = .,
                   value = import_unit_value,
                   mirrorValue = reverse_export_unit_value,
                   ratioBoundary = 1.05, log = TRUE, plot = TRUE) %>%
        validation(data = .,
                   value = export_unit_value,
                   mirrorValue = reverse_import_unit_value,
                   ratioBoundary = 1.05, log = TRUE, plot = TRUE) %>%
        imputeUnitValue(data = .,
                        unitValue = import_unit_value,
                        mirrorUnitValue = reverse_export_unit_value) %>%
        imputeUnitValue(data = .,
                        unitValue = export_unit_value,
                        mirrorUnitValue = reverse_import_unit_value)

    
    ## Balance the data
    balancedData =
        validatedData %>%
        updateTradeQuantity(data = .,
                            unitValue = import_unit_value,
                            value = import_value,
                            quantity = import_quantity) %>%
        updateTradeQuantity(data = .,
                            unitValue = export_unit_value,
                            value = export_value,
                            quantity = export_quantity) %>%
        updateTradeQuantity(data = .,
                            unitValue = reverse_import_unit_value,
                            value = reverse_import_value,
                            quantity = reverse_import_quantity) %>%
        updateTradeQuantity(data = .,
                            unitValue = reverse_export_unit_value,
                            value = reverse_export_value,
                            quantity = reverse_export_quantity) %>%
        calculateReliability(data = .,
                             import = import_quantity,
                             export = export_quantity,
                             reverseImport = reverse_import_quantity,
                             reverseExport = reverse_export_quantity,
                             reportingCountry = "reportingCountryM49",
                             partnerCountry = "partnerCountryM49",
                             pctTolerance = 0.05) %>% 
        balanceTradeQuantity(data = .,
                             import = import_quantity,
                             export = export_quantity,
                             reverseImport = reverse_import_quantity,
                             reverseExport = reverse_export_quantity,
                             reportingReliability = "reportingReliability",
                             partnerReliability = "partnerReliability",
                             pctTolerance = 0.05) %>%
        updateTradeValue(data = .,
                         unitValue = import_unit_value,
                         value = import_value,
                         quantity = import_quantity) %>%
        updateTradeValue(data = .,
                         unitValue = export_unit_value,
                         value = export_value,
                         quantity = export_quantity) %>%
        ## NOTE (Michael): Calculation of the revser is probably not
        ##                 required, since they will be discarded.
        updateTradeValue(data = .,
                         unitValue = reverse_import_unit_value,
                         value = reverse_import_value,
                         quantity = reverse_import_quantity) %>%
        updateTradeValue(data = .,
                         unitValue = reverse_export_unit_value,
                         value = reverse_export_value,
                         quantity = reverse_export_quantity)

    ## Save the data back to the database
    balancedData %>%
        saveValidatedTradeData(data = ., originalData = rawData)
}



## Checks
## ---------------------------------------------------------------------

par(mfrow = c(2, 2))
with(mirrorData,
     plot(log(Value_measuredElementTrade_5630),
          log(reverse_Value_measuredElementTrade_5930)))
with(mirrorData[log(Value_measuredElementTrade_5630) != 
                log(reverse_Value_measuredElementTrade_5930), ],
     points(log(Value_measuredElementTrade_5630),
            log(reverse_Value_measuredElementTrade_5930), col = "red", pch = 19))

with(mirrorData,
     plot(log(Value_measuredElementTrade_5930),
          log(reverse_Value_measuredElementTrade_5630)))
with(mirrorData[log(Value_measuredElementTrade_5930) != 
                log(reverse_Value_measuredElementTrade_5630), ],
     points(log(Value_measuredElementTrade_5930),
            log(reverse_Value_measuredElementTrade_5630), col = "red", pch = 19))


test = copy(mirrorData)
setnames(test,
         old = colnames(test),
         new = gsub("Value_measuredElementTrade_", "", colnames(test)))
numCol = names(which(sapply(test, typeof) == "double" &
                     sapply(test, function(x) !all(is.na(x)))))
plot(log(test[, numCol, with = FALSE]))

numCol = names(which(sapply(rawValues, typeof) == "double" &
                     sapply(rawValues, function(x) !all(is.na(x)))))
plot(log(rawValues[, numCol, with = FALSE]))



## Have a look at the network.
raw.dt = copy(rawValues)
raw.dt[, imported :=
           any(c(!is.na(Value_measuredElementTrade_5612),
                 !is.na(Value_measuredElementTrade_5600),
                 !is.na(Value_measuredElementTrade_5630)))]

imported.dt = raw.dt[raw.dt$imported, ]

plot(graph.data.frame(imported.dt[, list(reportingCountryM49, partnerCountryM49)]),
     vertex.size = 0.5)


with(mirrorData[flagTrade_measuredElementTrade_5600 != "mr" |
                flagTrade_measuredElementTrade_5621 != "mr", ],
     plot(log(Value_measuredElementTrade_5630),
          log(reverse_Value_measuredElementTrade_5930), col = "red"))


with(mirrorData,
     plot(log(Value_measuredElementTrade_5930),
          log(reverse_Value_measuredElementTrade_5630)))

with(mirrorData,
     plot(log(Value_measuredElementTrade_5930),
          log(reverse_Value_measuredElementTrade_5930)))

with(mirrorData,
     plot(log(Value_measuredElementTrade_5630),
          log(reverse_Value_measuredElementTrade_5630)))



checkBalancedData = copy(balancedData)
setnames(checkBalancedData,
         old = colnames(checkBalancedData),
         new = gsub("measuredElementTrade_", "",
             colnames(checkBalancedData)))
write.csv(checkBalancedData, file = "~/Desktop/balanced_trade_check.csv",
          row.names = FALSE, na = "")


checkValidatedData = copy(validatedData)
setnames(checkValidatedData,
         old = colnames(checkValidatedData),
         new = gsub("measuredElementTrade_", "",
             colnames(checkValidatedData)))
write.csv(checkValidatedData, file = "~/Desktop/validated_trade_check.csv",
          row.names = FALSE, na = "")

## Probably easier to debug by changing the name first, then change
## back when saving.



