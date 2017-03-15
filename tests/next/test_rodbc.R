## using odbc w/o loading the package
rodb <-RODBC::odbcConnect("Gandalf", uid = "AReddy", pwd = "fzzlpass")
query <- paste0("SELECT TOP 5 * FROM tbllinRegr")
RODBC::sqlQuery(rodb, query)


## Steps Taken:

remove.packages("AdapteR")
devtools::install_github("sanchitchip/AdapteR", "RODBC")
require(AdapteR)
connection <- flConnect(odbcSource = "Gandalf", database = "FL_DEMO", platform = "TD", pkg = "dbc")
