library(RJDBC)
library(data.table)
drv <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
            classPath = "~")
conn <- dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
                  user = "demo", password = "demo")
uniqueCountry = dbGetQuery(conn = conn, statement = "
    select distinct area
    from   TS_ICS_WORK_YR
    where  ele = '61' or ele = '91'")

areaData = lapply(uniqueCountry[1:10, 1], function(area){
    imports = dbGetQuery(conn = conn, statement = sprintf("
        select
            *
        from
            TS_ICS_WORK_YR
        where
                ele = '61'
            and area = %s", area))
    exports = dbGetQuery(conn = conn, statement = sprintf("
        select
            *
        from
            TS_ICS_WORK_YR
        where
                ele = '91'
            and area = %s", area))
})