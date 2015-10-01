## This module is designed to estimate the production of a derived commodity 
## using the supply quantity of the corresponding primary product.  It uses the 
## original AUPUS logic, or at least a small part of the logic, to convert 
## production primary -> processing primary -> input to processing -> production
## of derived (where the arrows are defined by multiplying by the corresponding
## ratio, share, and extraction rate, respectively).

## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Define directories
    if(Sys.info()[7] == "josh")
        apiDirectory = "~/Documents/Github/faoswsAupus/R"
    if(Sys.info()[7] == "rockc_000")
        apiDirectory = "~/Github/faoswsAupus/R"

    ## Get SWS Parameters
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "cc35be87-64c4-4915-bb72-65b1e0933d05"
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "3242f11d-28b2-4429-86b0-6fab97cb50bb"
    )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
}

derivedCrops = c("51", "60", "162", "165", "237", "244", "252", "256", "257",
                 "258", "261", "268", "271", "281", "290", "306", "307", "329",
                 "331", "334", "564", "767", "1242")
derivedAnimals = c("885", "886", "887", "888", "889", "890", "891", "894",
                   "895", "896", "897", "898", "899", "900", "901", "904",
                   "905", "952", "953", "955", "983", "984", "1021", "1043",
                   "1186", "1225", "1745", "1809", "1811", "1816")
derivedCodes = c(derivedCrops, derivedAnimals)
derivedCPC = fcl2cpc(formatC(as.numeric(derivedCodes), width = 4, flag = "0"))

###############################################################################
## 1. Get ratios, shares, and extraction rates (specific to country and year).
###############################################################################

param = defaultAupusParam()
param$areaCode = m492fs(swsContext.datasets[[1]]@dimensions[[areaVar]]@keys)
# param$itemCode = cpc2fcl(swsContext.datasets[[1]]@dimensions[[itemVar]]@keys,
#                          returnFirst = TRUE)
# param$itemCode = param$itemCode[!is.na(param$itemCode)]
# param$itemCode = as.character(as.numeric(param$itemCode))
param$itemCode = GetCodeList(domain = "faostat_one",
                             dataset = "aupus_share_fs",
                             dimension = "measuredItemParentFS")[, code]
## 41 = Extraction rate, 131 = Proportion to allocate to processing
param$elementCode = c("41", "131")
param$year = swsContext.datasets[[1]]@dimensions[[yearVar]]@keys
## don't check aupus param; checks require only one country
aupusParameterEnsured = TRUE
ratio = getRatioData(aupusParam = param, database = "new")
ratio = collapseSpecificData(aupusParam = param, listData = ratio)
share = getShareData(aupusParam = param, database = "new")
share = collapseShare(aupusParam = param, shareData = share,
                      shares = "Value_share")

parentMap = data.frame(child = derivedCodes)
parentMap = merge(parentMap, unique(share[, c("measuredItemParentFS",
                                              "measuredItemChildFS"),
                                          with = FALSE]), by.x = "child",
                  by.y = "measuredItemChildFS", all.x = TRUE)
parentMap = data.table(parentMap)
multipleParents = parentMap[, 


# key = DatasetKey(domain = "faostat_one", dataset = "aupus_ratio_fs",
#     dimensions = list(
#         geographicAreaFS = Dimension(name = "geographicAreaFS",
#                                      keys = GetCodeList("faostat_one", "aupus_ratio_fs",
#                                                         "geographicAreaFS")[type == "country", code]),
#                      measuredElementFS = Dimension(name = "measuredElementFS", keys = "131"),
#                      measuredItemFS = Dimension(name = "measuredItemFS",
#                                                 keys = GetCodeList("faostat_one", "aupus_ratio_fs",
#                                                                    "measuredItemFS")[, code]),
#                      timePointYearsSP = Dimension(name = "timePointYearsSP",
#                                                   keys = as.character(c(0, 1961:2013)))
#                  ))
# temp = GetData(key)
# parents = share[measuredItemChildFS %in% derivedCodes,
#                 unique(measuredItemParentFS)]
# temp[measuredItemFS %in% parents, unique(measuredItemFS)]
# temp[measuredItemFS %in% parents & Value != 0, unique(measuredItemFS)]

############################################################################### 
## 2. Get list of commodities I need (primary that are parents of processed,
## and derived). 
###############################################################################
cpcCodes = swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys
fclCodes = cpc2fcl(cpcCodes, returnFirst = TRUE)
fclCodes = fclCodes[!is.na(fclCodes)]
usedCodes = as.numeric(fclCodes)
oldLength = 0
while(oldLength < length(usedCodes)){
    oldLength = length(usedCodes)
    usedCodes = unique(c(usedCodes, share[measuredItemChildFS %in% usedCodes,
                                          measuredItemParentFS]))
}
ratio = ratio[measuredItemFS %in% usedCodes, ]
share = share[measuredItemChildFS %in% usedCodes, ]

## Convert codes to M49/CPC
ratio[, measuredItemCPC := faoswsUtil::fcl2cpc(formatC(measuredItemFS,
                                                       width = 4, flag = "0"))]
ratio[, geographicAreaM49 := faoswsUtil::fs2m49(as.character(geographicAreaFS))]
ratio[, timePointYears := timePointYearsSP]
share[, measuredItemParentCPC := faoswsUtil::fcl2cpc(formatC(
    as.numeric(measuredItemParentFS), width = 4, flag = "0"))]
share[, measuredItemChildCPC := faoswsUtil::fcl2cpc(formatC(
    as.numeric(measuredItemChildFS), width = 4, flag = "0"))]
share[, geographicAreaM49 := faoswsUtil::fs2m49(as.character(geographicAreaFS))]
share[, timePointYears := timePointYearsSP]

###############################################################################
## 3. Get production and stock changes for primary products.
###############################################################################
prodKey = swsContext.datasets[[1]]
## Need to add in production code, and this depends on the element.
prodCodes = unique(getYieldCodes(
    prodKey@dimensions$measuredIasasdasftemCPC@keys)[, production])
prodKey@dimensions$measuredElement@keys = prodCodes
prodData = GetData(prodKey)

###############################################################################
## 4. Get trade for primary products.
###############################################################################
tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_CPC", 
    dimensions = list(
        swsContext.datasets[[1]]@dimensions$geographicAreaM49,
        Dimension(name = "measuredElementTrade", keys = c("5600", "5900")),
        swsContext.datasets[[1]]@dimensions$measuredItemCPC,
        swsContext.datasets[[1]]@dimensions$timePointYears))
tradeData = GetData(tradeKey)
if(nrow(tradeData) == 0){
    
} else {
    tradeData = data.table:::dcast.data.table(tradeData,
        formula = geographicAreaM49 + measuredItemCPC + timePointYears ~
            measuredElementTrade, value.var = "Value")
    tradeData[, netTrade := get("5600") - get("5900")]
}

###############################################################################
## 5. Compute production of derived commodities.
###############################################################################
data = merge(prodData, tradeData
stop("Still need to bring in stock data!!!")
Supply * Ratio_131(primary) * Share(primary) * Ratio_41(derived) to get production derived

###############################################################################
## 6. Get production of derived products currently in the system, and determine
## which should be overwritten.
###############################################################################

###############################################################################
## 7. Write results back to SWS
###############################################################################
