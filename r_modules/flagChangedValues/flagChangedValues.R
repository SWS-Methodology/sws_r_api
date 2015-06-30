## load the library
library(faosws)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
mergeKey = c("geographicAreaM49", "measuredElement",
             "measuredItemCPC", "timePointYears")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Get SWS Parameters
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "3120d0c5-b5bd-4b24-a197-95559e60ae6a"
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "02ebe8ec-6509-477a-97b2-bf32f7a9afe2"
    )
#     files = dir("~/Documents/Github/sws_r_api/r_modules/flagChangedValues/R",
#                 full.names = TRUE)
#     if(length(files) == 0){
#         stop("No source files found!  Check your directory.")
#     }
#     sapply(files, source)
    
} else {
    cat("Working on SWS...\n")
}

## Get Session Data
sessionData = GetData(swsContext.datasets[[1]])

## Get Database Data
databaseKey = DatasetKey(domain = swsContext.datasets[[1]]@domain,
                         dataset = swsContext.datasets[[1]]@dataset,
                         dimensions = swsContext.datasets[[1]]@dimensions)
databaseData = GetData(databaseKey)

compare = merge(sessionData, databaseData, by = mergeKey,
                suffixes = c(".session", ".dataset"))
compare[Value.session != Value.dataset, ]

compare[, Severity := 0]
compare[Value.session != Value.dataset, Severity := 5]
compare[, Description := ""]
compare[Value.session != Value.dataset, Description := "Doesn't match database"]
compare = compare[, c(mergeKey, "Severity", "Description"), with = FALSE]

cat("Attempting to save validation data to database...\n")

SaveValidation(domain = "agriculture", dataset = "agriculture",
               validation = compare)

paste(sum(compare[, Severity > 0]), "differences found!")
