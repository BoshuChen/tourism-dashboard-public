## Integrate script for the NZ Tourism Dashboard shiny app
##
## Purpose: Prepares the data for the shiny app and (optionally) deploys it
##
## Author: Jimmy Oh
## Includes prior work by: George Fan
##
## Data source:
##
## Peer review:
##

# rm(list=ls())
# gc()

###########
## Setup ##
###########
library(RODBC)
library(mbie)
library(mbieDBmisc)
library(dplyr)
library(tidyr)
## Credentials for SQL
#SQLcreds = list(uid = "analyst", pwd = "")
#TRED = odbcConnect("TRED_Prod", uid = SQLcreds$uid, pwd = SQLcreds$pwd)
## Try odbcConnect to "PlayPen_Prod"
## If this fails, odbcConnect to "PlayPen"
#PlayPen = suppressWarnings(odbcConnect("PlayPen_Prod", uid = SQLcreds$uid, pwd = SQLcreds$pwd))
#if(PlayPen == -1) PlayPen = odbcConnect("PlayPen", uid = SQLcreds$uid, pwd = SQLcreds$pwd)

TRED = odbcConnect("TRED_Prod")
PlayPen = odbcConnect("PlayPen_Prod")

###############
## Variables ##
###############
## Relative path to the data prep directory
prepdir = "prep/"

## Relative path to the shiny app directory
shinydir = "shiny/new"

## suffix appended to "tourism_dashboard_" when deploying shiny app
## e.g. "new"  -> https://mbienz.shinyapps.io/tourism_dashboard_new
##      "prod" -> https://mbienz.shinyapps.io/tourism_dashboard_prod
## If deploysuffix = NULL, shiny app is not deployed
# deploysuffix = "test" # ie test environment.  This used to be called "new"
# deploysuffix = "prod"
deploysuffix = NULL

##########################
## Supporting Functions ##
##########################
sourceprep =
   ## A wrapper for sourcing prep files
   function(fname){
      cat("--source:", fname, "\n")
      source(paste0(prepdir, fname))
   }
addmeta =
   ## Adds metadata to the given data.frame (curdf) as attributes
   ## These are:
   ##  date = Sys.Date (current date when code is run)
   ##  dataset = one or more datasets, per the names defined in `defdata`
   function(curdf, dataset_names){
      attr(curdf, "date") = Sys.Date()
      attr(curdf, "dataset") = dataset_names
      curdf
   }
savepath =
   ## Given the file name to save as, constructs an appropriate file path
   function(fname)
      paste0(shinydir, "/data/", fname, ".rda")
saverda =
   ## A wrapper for `save` that allows passing of the object directly into the function
   ## This object will then be saved as an object with name `dname`
   ##  with a filepath generated using `savepath` with `fname` = `dname`
   ## If dname is NULL, `substitute` is used to generate it.
   function(dat, dname = NULL){
      dlist = list(dat)
      if(is.null(dname))
         dname = as.character(substitute(dat))
      names(dlist) = dname
      save(list = dname, file = savepath(dname), envir = as.environment(dlist))
   }

#####################
## Define Datasets ##
#####################
## In the data prep scripts, each data file (.rda) is given
##  one or more datasets (defined here), from which the data is drawn.
## That metadata, along with the related information given in these definitions
##  are used in the shiny app, to provide data source information.
## Take note of special characters, you may need to encode it with `encodeString`
defdata = list(
   BDS = list(
      long = "NZ Business Demography Statistics",
      provider = "Statistics New Zealand",
      URL = "http://www.stats.govt.nz/browse_for_stats/businesses/business_characteristics/nz-business-demography-statistics-info-releases.aspx"
   ),
   CAM = list(
      long = "Commercial Accommodation Monitor",
      provider = "Statistics New Zealand",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/commercial-accommodation-monitor"
   ),
   CAS = list(
      long = "Convention Activity Survey",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/other-research-and-reports/convention-research-programme/convention-activity-survey"
   ),
   DTS = list(
      long = "Domestic Travel Survey",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/domestic-travel-survey"
   ),
   ITM = list(
      long = "International Travel and Migration",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/international-travel"
   ),
   IVS = list(
      long = "International Visitor Survey",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs"
   ),
   MTAGDP = list(
      long = "Modelled Territorial Authority Gross Domestic Product",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/regions-cities/research/modelled-territorial-authority-gross-domestic-product"
   ),
   RTE = list(
      long = "Regional Tourism Estimates",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/regional-tourism-estimates"
   ),
   RTI = list(
      long = "Regional Tourism Indicators",
      provider = "MBIE",
      URL = "http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/regional-tourism-indicators"
   ),
   TSA = list(
      long = "Tourism Satellite Account",
      provider = "Statistics New Zealand",
      URL = "http://www.stats.govt.nz/browse_for_stats/industry_sectors/Tourism/tourism-satellite-account-info-releases.aspx"
   )
)
## Save the dataset definitions for access in shiny app
saverda(defdata)

###############
## Data Prep ##
###############
## Overview - Economic Contribution
sourceprep("economic_contribution.R")
economic_contribution("TSA")

## Overview - International Visitor Arrivals
## Overview - International Visitor Spend
sourceprep("intl_visitor_arrivals_spend.R")
intl_visitor_arrivals("ITM")
intl_visitor_spend("IVS")

## Regions - Visitor Nights
sourceprep("nights_origin.R")
nights_origin(c("IVS", "DTS"))
nights_region("CAM")

## Regions - Contribution to Regional GDP
sourceprep("GDP_tourism.R")
GDP_tourism("MTAGDP")

## Industry - Accommodation
sourceprep("Accomm_RTO.R")
Accomm_RTO("CAM")

## Industry - Activities
## AM comment:  someone needs to fix the SQL code in ivs_activities.R so that it works in the MBIE environment
sourceprep("ivs_activities.R")
ivs_act("IVS")

## Industry - NZ business demography
sourceprep("business_demography.R")
business_demography("BDS")

## Visitor Markets - Accommodation Used
sourceprep("ivs_accomm_used.R")
ivs_accomm_used("IVS")

##-----------------------------------------------##
## Data that draws from non-TRED/PlayPen sources ##
##-----------------------------------------------##
## RTE data, used for:
##    Regions - Industry Sectors
##    Industry - Spend by Visitor Market
##    Visitor Markets - Compare Origins
##    Visitor Markets - Compare Destinations
## Latest RTE data is not in TRED, thus this script pulls from a csv.gz file
## See: https://github.com/nz-mbie/tourism-dashboard/issues/57
sourceprep("RTE_data.R")
RTE_data("RTE")

## Regions - Regional Summaries
## Uses both RTE and RTI data
## RTE data from same source as above
## RTI data comes from TRED
sourceprep("rs_data.R")
rs_data(c("RTI", "RTE"))

## Industry - Business Events (CAS)
## Comes from a pivot table
## Due to the way the CAS table is generated, best way
##  seems to be to read from the final pivot table
##  rather than reproducing the entire sourcing from TRED steps
## Location of final pivot table should be:
## paste0("P:/OTSP/Convention Research Programme/Convention Activity Survey/CAS_dissemination/outputs/CAS_master_for_web_",currYr,currQtr,".csv")
## As I cannot access P drive, I cannot reliably update this
sourceprep("CAS_data.R")
CAS_data("CAS")

## Create local forex data as fallback
## Used when `getFX` fails for some reason
library(quantmod)
## Currencies to show in forex page
fx_currencies = c(
   "Australian dollar" = "AUD",
   "Chinese yuan" = "CNY",
   "United States dollar" = "USD",
   "Pound sterling" = "GBP",
   "Euro" = "EUR",
   "Japanese yen" = "JPY",
   "Canadian dollar" = "CAD",
   "Indian rupee" = "INR",
   "Brazilian real" = "BRL",
   "Chilean peso" = "CLP",
   "Argentine peso" = "ARS"
)
## Grab currencies for the latest 5 years, starting from January
fx_all_years = as.numeric(format(Sys.time(), "%Y")) - 5:1
fx_date_start = paste0(fx_all_years[1], "-12-31")
## Get data
env_forex_fallback = new.env()
for(curfx in fx_currencies){
   fx_str = paste("NZD", curfx, sep = "/")
   cat("getFX", fx_str, "\n")
   getFX(fx_str, from = fx_date_start, env = env_forex_fallback)
}
save(env_forex_fallback, file = savepath("env_forex_fallback"))


####################################
#  Deployment
##########################

#----------------Save text file versions of data--------------
# this does not do anything needed for the app, but makes it easier
# to use a Git diff to see what has changed data to data
source("test/save_text_versions.R")


#-----------------Save copy for the public version----------
source("prep/copy-to-public.R")
# note - if you wish to update the public version of the source code, you need
# to go to the tourism-dashboard-public repository on the same drive as youe
# clone of the main repository, and push to 
# https://github.com/nz-mbie/tourism-dashboard-public.git

######################
#-------------Deploy shiny app----
######################
if(!is.null(deploysuffix)){
   ## Proxy password to get through MBIE firewall
   ## Note by Jimmy: Copied from old integrate, don't know if this is needed or if it works
   if(!exists("creds")){
       creds <- mbie::AskCreds(Title = "MBIE User Log In Name and Password", startuid = "", returnValOnCancel = "ID_CANCEL")   
       options(RCurlOptions = list(proxy = 'http://proxybcw.wd.govt.nz:8080',
                                   proxyusername = creds$uid, 
                                   proxypassword = creds$pwd))
    }
   
   ## shinyapps renamed to rsconnect recently, some code to load either one
   ## LOGIC:
   ##  IF loading rsconnect fails
   ##  THEN load shinyapps
   ## rsconnect loaded with `require` for a TRUE/FALSE return value (and no error)
   ## shinyapps loaded with `library` so it gives an error if it fails
   isrsc = suppressWarnings(require(rsconnect, quietly = TRUE))
   if(tolower(deploysuffix) == "prod"){
      message("you are about to deploy the production version, please confirm y/n > ", appendLF = FALSE)
      question <- readLines(n = 1)
      
   } else {
      question <- "y"
   }
   
   
   if(!isrsc) library(shinyapps)
   if(question == "y"){
      deployApp(appDir = shinydir,
                appName = paste0("tourism_dashboard_", deploysuffix),
                account = "mbienz")
      
      
   }
}
