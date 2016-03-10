library(shiny)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(dygraphs)
library(ggvis)

source("helper_funcs.R")
source("dygraph-extra-shiny.R")

obsroll = c(
   "Observations" = "Obs",
   "12-month rolling average" = "RA",
   "Both" = "Both"
)
domint = c(Domestic = "dom", International = "int")

## Dataset definitions
load("data/defdata.rda")

## Overview - Economic Contribution
## GDP and Employment contribution
load("data/tourism_economic.rda")
## Intl tourism as % of exports
load("data/tourism_percexports.rda")
## Tourism Expenditure
load("data/tourism_expenditure.rda")

## Overview - International Visitor Arrivals
## Overview - International Visitor Spend
load("data/arriv.rda")
load("data/ivs.rda")
## Interim metadata addition to arriv
attr(arriv, "date") = as.Date("2015-08-15")
attr(arriv, "dataset") = "ITM"
## Fix ivs dates
ivs$TimePeriod = local({
   ty = as.numeric(format(ivs$TimePeriod, "%Y"))
   tm = as.numeric(format(ivs$TimePeriod, "%m"))
   td = as.numeric(format(ivs$TimePeriod, "%d"))
   ty[tm == 12] = ty[tm == 12] + 1
   tm = (tm + 1) %% 12

   as.Date(paste(ty, tm, td, sep = "-")) - 1
})
## Convert characters to factors
for(j in c("Variable", "Dimension", "Type")){
   arriv[[j]] = factor(arriv[[j]])
   ivs[[j]] = factor(ivs[[j]])
}
## input categories
arrivType = structure(c("country", "pov"), .Names = levels(arriv$Type))
arrivVar = levels(arriv$Variable)
ivsVar = rev(levels(ivs$Variable))
ivsIndexBase = as.character(unique(ivs$TimePeriod)[-(1:3)])

## Used to separate Dimension levels to country/pov (purpose of visit)
## Places used (incomplete list):
##   Overview - International Visitor Arrivals
##   Overview - International Visitor Spend
# load("data/all_countries.rda")
## Rearrange order of countries by Total Spend in ivs
## Generating new list from ivs so all_countries.rda not needed
all_countries = local({
   spend_last = ivs %>% filter(TimePeriod == max(TimePeriod),
                               Variable == "Total spend",
                               Type == "Country of residence")
   as.character(spend_last$Dimension[order(spend_last$Value, decreasing = TRUE)])
})
all_countries_no_total = all_countries[-1]
load("data/all_povs.rda")
country_pov = list(
   "country" = all_countries,
   "pov" = all_povs
)

## Regions - Visitor Nights
load("data/nights_origin.rda")
nights_origin = wa2wha(nights_origin)
nights_origin$dom = filter(nights_origin$dom, Date >= "2009-01-01")
nights_origin$int = filter(nights_origin$int, Date >= "2009-01-01")
NightsAreaType = c(
   "Regional Council" = "RC",
   "Territorial Authority" = "TA"
)
NightsAreaList = list()
for(i in 1:length(NightsAreaType)){
   AreaType = NightsAreaType[i]
   AreaTypeName = names(NightsAreaType)[i]
   NightsAreaList[[AreaType]] = c(
      paste0("Total (All ", AreaTypeName, ")"),
      sort(unique(c(as.character(filter(nights_origin$dom, AreaType == AreaTypeName)$Area),
                    as.character(filter(nights_origin$int, AreaType == AreaTypeName)$Area))))
   )
}
# NightsOrigins = list(
   # dom = c("Total (All Regions)", levels(nights_origin$dom$Origin)),
   # int = c("Total (All Countries)", all_countries)
# )
NightsOrigins = list(
   dom = levels(nights_origin$dom$Origin),
   int = all_countries_no_total
)
NightsOriginscats = c("Domestic origin" = "dom", "International origin" = "int")
## CAM data
load("data/nights_region.rda")
nights_region = wa2wha(nights_region)
nights_cam = list()
nights_cam$dom = local({
   cam = nights_region %>%
      filter(TimePeriod >= "2009-06-01",
             CV2 == "Domestic guest nights") %>%
      select(TimePeriod, Region = CV1, Value)
})
nights_cam$int = local({
   cam = nights_region %>%
      filter(TimePeriod >= "2009-06-01",
             CV2 == "International guest nights") %>%
      select(TimePeriod, Region = CV1, Value)
})
NightsCAMArea = levels(nights_cam$dom$Region)
NightsCAMArea = c(NightsCAMArea[which(NightsCAMArea == "New Zealand")],
                  NightsCAMArea[which(NightsCAMArea != "New Zealand")])

## Below is code for trying to match CAM data to IVS data
# nights_cam$dom = local({
   # cam = nights_region %>%
      # filter(CV1 == "New Zealand",
             # CV2 == "Domestic guest nights",
             # TimePeriod >= "1998-01-01" & TimePeriod <= "2012-12-31") %>%
      # mutate(Year = format(TimePeriod, "%Y")) %>%
      # group_by(Year) %>%
      # summarise(CAM = sum(Value, na.rm = TRUE))
   # data.frame(Date = as.Date(paste0(cam$Year, "-12-31")), cam["CAM"])
# })
# nights_cam$int = local({
   # cam = nights_region %>%
      # filter(CV1 == "New Zealand",
             # CV2 == "International guest nights",
             # TimePeriod >= "1997-07-01" & TimePeriod <= "2014-06-30") %>%
      # mutate(Year = format(TimePeriod + 365/2, "%Y")) %>%
      # group_by(Year) %>%
      # summarise(CAM = sum(Value, na.rm = TRUE))
   # data.frame(Date = as.Date(paste0(cam$Year, "-06-30")), cam["CAM"])
# })

## Regions - Contribution to Regional GDP
load("data/GDP_tourism.rda")
GDP_tourism = wa2wha(GDP_tourism)
## Total is treated specially and added to the top of the regional breakdowns
levels(GDP_tourism$Area) = gsub("New Zealand", "Total (All New Zealand)",
                                levels(GDP_tourism$Area), fixed = TRUE)
GDP_AreaType = levels(GDP_tourism$AreaType)[c(1, 2)]
GDP_AreaList = lapply(GDP_AreaType, function(curAreaType)
   c("Total (All New Zealand)", sort(as.character(unique(
      filter(GDP_tourism, AreaType == curAreaType)$Area))))
)
names(GDP_AreaList) = GDP_AreaType

## Industry - Accommodation
load("data/Accomm_RTO.rda")
Accomm_RTO = wa2wha(Accomm_RTO)
## Rearrange order of accommodation types
AccommType = local({
   baselevels = levels(Accomm_RTO$Accommodation_Type)
   toti = grep("^Total", baselevels)
   c(baselevels[toti], baselevels[-toti])
})
## Rearrange order of RTO for Total on top
AccommRTOs = local({
   baselevels = levels(Accomm_RTO$RTO)
   toti = grep("^Total", baselevels)
   c(baselevels[toti], baselevels[-toti])
})
## Variable classes used
AccommClass = c(
   "Guest nights" = "Guest nights, total (number)",
   "Guest arrivals" = "Guest arrivals (number)",
   "Occupancy rate" = "Occupancy rate (percent)",
   "Occupancy count" = "Occupancy, monthly (stay-unit-nights used)",
   "Capacity" = "Capacity, monthly (stay-unit-nights available)",
   "Establishments" = "Establishments (number)"
)
AccommClassType = c(
   "Guest nights" = "number",
   "Guest arrivals" = "number",
   "Occupancy rate" = "percent",
   "Occupancy count" = "number",
   "Capacity" = "number",
   "Establishments" = "number"
)

## Industry - Activities
load("data/ivs_act.rda")
## Produce data source here, before manipulation, since dplyr strips attributes
datasource_ivs_act = datasource2(ivs_act)
ivs_act$ActivityCount = sapply(strsplit(as.character(ivs_act$ActivityConcat), ","), length)
act_counts = sort(unique(ivs_act$ActivityCount))
ivs_values = c(
   "Nights Stayed" = "AverageNights",
   "Spend Per Night" = "SpendPerNight",
   "Spend Per Trip" = "SpendPerTrip",
   "Total Spend" = "TotalSpend"
)
## Remove old data as they seem to be unreliable
ivs_act = ivs_act[as.Date(ivs_act$Date) > as.Date("2005-01-01"),]
## Remove new data that don't have PopNights data
ivs_act = ivs_act[!is.na(ivs_act$PopNights),]
## Generate some help text for activity participation
ivs_act_text = paste0(
   round(with(ivs_act, sum(Pop[ActivityCount == 1])/sum(Pop)), 2) * 100, "% ",
   "of visitors only participated in a single activity category. ",
   round(with(ivs_act, sum(Pop[ActivityCount >= 4])/sum(Pop)), 2) * 100, "% ",
   "of visitors participated in activities coming from 4 or more activity categories."
)
## Pre-compute Total (All Countries)
ivs_act = local({
   ivs_total = ivs_act %>%
      group_by(Date, ActivityConcat, ActivityCount) %>%
      summarise(TotalSpend = sum(TotalSpend),
                Pop = sum(Pop),
                PopNights = sum(PopNights)) %>%
      cbind(Country = "Total (All Countries)")
   suppressWarnings(full_join(ivs_act, ivs_total, by = names(ivs_act)))
})
## Activity category breakdowns
load("data/ivs_actcat.rda")
act_types = names(ivs_actcat)
act_breakdowns = local({
   actul = lapply(ivs_actcat, function(x) do.call(tags$ul, lapply(x, tags$li)))
   for(curact in names(actul))
      actul[[curact]] = tags$div(
         tags$label(curact),
         actul[[curact]]
      )
   # do.call(tagList, actul)
   ## Manual adjustment into columns to look better
   tagList(
      tags$div(class = "float actcat",
         actul[["Places"]]
      ),
      tags$div(class = "float actcat",
         actul[["Entertainment"]], actul[["Maori activities"]]
      ),
      tags$div(class = "float actcat",
         actul[["Pursuits"]]
      ),
      tags$div(class = "float actcat",
         actul[["Museums and art galleries"]]
      ),
      tags$div(class = "float actcat", style = "width: 140px;",
         actul[["Nature"]]
      ),
      tags$div(class = "float actcat", style = "width: 240px;",
         actul[["Transport"]]
      )
   )
})

## Industry - Business demography
load("data/bdem_countsize.rda")
load("data/bdem_birthdeath.rda")
load("data/bdem_btype.rda")
load("data/bdem_overseas.rda")
## Only use All NZ data for now, so filter out breakdowns in bdem_birthdeath
bdem_birthdeath = bdem_birthdeath %>%
   filter(Area == "Total New Zealand (Region)") %>%
   select(-Area)
levels(bdem_birthdeath$Unit) = gsub("Geographic Units", "Enterprises", levels(bdem_birthdeath$Unit), fixed = TRUE)
## Add Birth Per Death data
bdem_birthdeath = bdem_birthdeath %>%
   spread(Type, Value) %>%
   mutate(BirthPerDeath = Birth/Death) %>%
   gather(Type, Value, Birth, Death, BirthPerDeath)
levels(bdem_birthdeath$Type) = gsub("BirthPerDeath", "Births Per Death", levels(bdem_birthdeath$Type))
bdem_birthdeath$Type = factor(bdem_birthdeath$Type, levels =
   c("Births Per Death", "Birth", "Death"))
## Fix order of levels
bdem_countsize$Size = factor(bdem_countsize$Size, levels =
   c("Total", "0", "1 to 5", "6 to 9", "10 to 19", "20 to 49", "50 to 99", "100+"))
bdem_overseas$OverseasEquity = factor(bdem_overseas$OverseasEquity, levels =
   c("Total", "Less than 1%", "1 to 24%", "25 to 49%", "50% or more"))
bdem_btype$BusinessType = local({
   baselevels = levels(bdem_btype$BusinessType)
   toti = grep("^Total", baselevels)
   factor(bdem_btype$BusinessType, levels =
      c(baselevels[toti], baselevels[-toti]))
})
## Remove industry code
levels(bdem_countsize$Industry) = gsub("^[[:alpha:]] ", "", levels(bdem_countsize$Industry))
levels(bdem_birthdeath$Industry) = gsub("^[[:alpha:]] ", "", levels(bdem_birthdeath$Industry))
levels(bdem_btype$Industry) = gsub("^[[:alpha:]] ", "", levels(bdem_btype$Industry))
levels(bdem_overseas$Industry) = gsub("^[[:alpha:]] ", "", levels(bdem_overseas$Industry))
bdem_IndustryLevels = sort(levels(bdem_countsize$Industry))
bdem_IndustryLevels = local({
   toti = grep("^Total", bdem_IndustryLevels)
   toui = grep("^Accommodation and Food Services", bdem_IndustryLevels)
   remi = seq(along = bdem_IndustryLevels)[-c(toui, toti)]
   bdem_IndustryLevels[c(toui, toti, remi)]
})
bdem_UnitLevels = rev(levels(bdem_countsize$Unit))
## Levels for each category
bdem_Data = c(
   "Births & Deaths" = "birthdeath",
   "Size (# of Employees)" = "countsize",
   "Business Type" = "btype",
   "Overseas Equity" = "overseas"
)
bdem_Cats = list(
   "countsize" = levels(bdem_countsize$Size),
   "birthdeath" = levels(bdem_birthdeath$Type),
   "btype" = levels(bdem_btype$BusinessType),
   "overseas" = levels(bdem_overseas$OverseasEquity)
)

## Visitor Markets - Accommodation Used
load("data/ivs_accomm_used.rda")
## Produce data source here, before manipulation, since dplyr strips attributes
datasource_ivs_accomm_used = datasource2(ivs_accomm_used)
## Fix dates
ivs_accomm_used$Date = local({
   ty = as.numeric(format(ivs_accomm_used$Date, "%Y"))
   tm = as.numeric(format(ivs_accomm_used$Date, "%m"))
   td = as.numeric(format(ivs_accomm_used$Date, "%d"))
   ty[tm == 12] = ty[tm == 12] + 1
   tm = (tm + 1) %% 12

   as.Date(paste(ty, tm, td, sep = "-")) - 1
})
ivs_accomm_used = ivs_accomm_used %>%
   mutate(
      ##AverageNights = PopNights/Pop,
      ##SpendPerNight = TotalSpend/PopNights,
      SpendPerTrip = TotalSpend/Pop,
      TotalSpend = TotalSpend/10^6
   )
ivs_accomm_scale = structure(c("rv", "sv"),
   .Names = c("Raw Values", "Proportions"))
ivs_accomm_years = 3
ivs_accomm_dates = as.character(rev(sort(unique(ivs_accomm_used$Date))[-(1:(ivs_accomm_years * 12))]))
## Merge some accomm types
ivs_accomm_used$AccommType = local({
   atype = as.character(ivs_accomm_used$AccommType)
   mergelist = list(
      "Camping ground" = c(
         "Another place where you pay to park a caravan or campervan / motorhome overnight",
         "Other camping ground / holiday park \\(where you can stay in a tent, cabin, caravan, or campervan / motorhome\\)"
      ),
      "National Park / Department of Conservation grounds" = c(
         "Camping at a National Park / Department of Conservation camping ground",
         "In a hut at a National Park / Department of Conservation area"
      ),
      "Free camping" = "Free camping - staying at a place that is NOT an official camp site, in a tent, caravan, campervan / motorhome",
      "Backpackers" = "Youth Hostel, YMCA, YWCA",
      "Hotel" = "Private Hotel / Guesthouse",
      "Not sure" = "NOT SURE"
   )
   for(i in 1:length(mergelist))
      atype = gsub(paste0("(", mergelist[[i]], ")", collapse = "|"), names(mergelist)[[i]], atype)
   ## NA becomes "Not sure"
   atype[is.na(atype)] = "Not sure"
   factor(atype)
})
## Re-order accomm types by Total Pop 
ivs_accomm_types = local({
   dat_summ = ivs_accomm_used %>%
      group_by(AccommType) %>%
      summarise(Pop = sum(Pop, na.rm = TRUE))
   new_order = as.character(dat_summ$AccommType)[order(dat_summ$Pop, decreasing = TRUE)]
   ## Move Aggregate categories to end
   ## Other, Not Sure
   ind_other = grep("Other", new_order)
   ind_notsure = grep("Not sure", new_order)
   c(new_order[-c(ind_other, ind_notsure)],
     new_order[ind_other],
     new_order[ind_notsure])
})
ivs_accomm_used$AccommType = factor(ivs_accomm_used$AccommType, levels = ivs_accomm_types)

## Industry - Business Events (CAS)
load("data/all_cas.rda")

## RTE data, used for:
##    Regions - Industry Sectors
##    Industry - Spend by Visitor Market
##    Visitor Markets - Compare Origins
##    Visitor Markets - Compare Destinations
env_rte = new.env()
load("data/all_rte.rda", envir = env_rte)
load("data/all_rte_levels.rda", envir = env_rte)
with(env_rte, {
   all_rte = wa2wha(all_rte)
   all_rte_levels = wa2wha(all_rte_levels, check.characters = TRUE)
   compcatRTR = structure(c("Region", "RTO", "Territorial_Authority"),
      .Names = c("Region", "RTO", "Territorial authority"))
   catOrigin = structure(c("All", "Domestic", "International"),
      .Names = c("Total (All)", "Domestic origin", "International origin"))
   levelsOrigin = list(Domestic = all_rte_levels$OriginDom,
                       International = all_rte_levels$OriginInt)
   catscale = structure(c("dv", "sv"),
      .Names = c("Dollar figures", "Proportions"))
   
   ## Rearrange order of countries by Total Spend in all_rte
   ## Disabled for now because it messes up colours
   ##  and fixing is non-trivial
   # flist = list(YEMar = max(all_rte$YEMar),
                # Type = "International",
                # Origin = levelsOrigin$International)
   # rte_last = getdat(all_rte, "YEMar", "Origin", "Spend", flist)
   # levelsOrigin$International = as.character(rte_last$Origin)[order(rte_last$Spend, decreasing = TRUE)]
   
   # Origin_levels_reorder = levels(all_rte$Origin)
   # Origin_levels_reorder[Origin_levels_reorder %in% levelsOrigin$International] = levelsOrigin$International
   # all_rte$Origin = factor(all_rte$Origin, levels = Origin_levels_reorder)
})

## Regions - Regional Summaries
load("data/rs_data.rda")
rs_data = wa2wha(rs_data, check.characters = TRUE)
rs_data$areaclass = structure(names(rs_data$arealist),
   .Names = gsub("TA", "Territorial authority", names(rs_data$arealist), fixed = TRUE))
rs_data$allPeriods = as.character(sort(unique(rs_data$RTI$Period)))
rs_data$CurrentYear = max(rs_data$RTE$YEMar)

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
names(fx_currencies) = paste0(fx_currencies, " (", names(fx_currencies), ")")
## Find matching ivs countries to get spend data
fx_currency_to_ivs = list(
   AUD = "Australia",
   CNY = "China",
   USD = "USA",
   GBP = "UK",
   EUR = c("Germany", "Rest of Europe"),
   JPY = "Japan",
   CAD = "Canada",
   INR = "Rest of Asia",
   BRL = "Rest of Americas",
   CLP = "Rest of Americas",
   ARS = "Rest of Americas"
)
## Grab currencies for the latest 5 years, starting from January
fx_all_years = as.numeric(format(Sys.time(), "%Y")) - 5:1
fx_date_start = paste0(fx_all_years[1], "-12-31")
fx_date_leadin = paste0(fx_all_years[1], "-01-01")
## Load fallback data
load("data/env_forex_fallback.rda")
