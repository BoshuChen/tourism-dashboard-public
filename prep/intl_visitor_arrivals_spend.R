## Prep data script for shiny app
## Used for: Overview - International Visitor Arrivals
##           Overview - International Visitor Spend
## Last Updated: 5 November 2015
## Outputs:
## - arriv.rda
## - ivs.rda
## Author: Jimmy Oh
## Peer reviewed by: 

intl_visitor_arrivals = function(dataset_names){
   ###########################################
   ## Code taken from itm_and_ivs_overview.R
   #------inbound visitors-----------------
   arriv1 <- ImportTS2(TRED, "Visitor arrivals by EVERY country of residence and purpose (Monthly)",
                      where = "cc2.ClassificationValue = 'TOTAL ALL TRAVEL PURPOSES'") %>% 
      filter(toupper(CV1) != CV1) %>%
      group_by(TimePeriod, CountryGrouped) %>%
      summarise(Value = sum(Value)) %>%
      mutate(Year = substring(TimePeriod, 1, 4), 
             Variable="Arrivals") %>%
      data.frame() %>%
      mutate(Dimension = as.character(CountryGrouped),
             Type="Country of residence") %>%
      select(-CountryGrouped)

   arriv2 <- ImportTS2(TRED, "Visitor arrivals by EVERY country of residence and purpose (Monthly)",
                      where = "cc2.ClassificationValue != 'TOTAL ALL TRAVEL PURPOSES' and
                      cc1.ClassificationValue = 'TOTAL ALL COUNTRIES OF RESIDENCE'") %>%
      rename(Dimension = CV2) %>%
      mutate(Dimension = rename.levels(Dimension, 
                                       orig=c("Holiday/Vacation", "Conventions/Conferences", 
                                              "Visit Friends/Relatives", "Unspecified/Not Collected", 
                                              "Education", "Other"),
                                       new =c("Holiday / vacation", "Conference / convention", 
                                              "Visiting friends / relatives", 
                                              rep("Other purpose", 3))) %>%
                as.character(),
             Variable = "Arrivals",
             Type = "Purpose of visit",
             Year = substring(TimePeriod, 1, 4)) %>%
      group_by(Dimension, TimePeriod, Type, Variable, Year) %>%
      summarise(Value = sum(Value)) %>%
      data.frame()

   arriv3 <- ImportTS2(TRED, "Average number of visitors in New Zealand by country of residence (Monthly)") %>%
      rename(Dimension = CountryGrouped) %>%
      group_by(Dimension, TimePeriod) %>%
      summarise(Value = sum(Value)) %>%
      data.frame() %>%           # to get rid of grouping
      mutate(Year = substring(TimePeriod, 1, 4),
             Variable = "Number in country",
             Type = "Country of residence",
             Dimension = as.character(Dimension))

   arriv4 <- ImportTS2(TRED, "Average number of visitors in New Zealand by purpose (Monthly)") %>%
      rename(Dimension = CV1) %>%
      mutate(Dimension = rename.levels(Dimension, 
                                       orig=c("Holiday/Vacation", "Conventions/Conferences", 
                                              "Visit Friends/Relatives", "Unspecified/Not Collected", 
                                              "Education", "Other"),
                                       new =c("Holiday / vacation", "Conference / convention", 
                                              "Visiting friends / relatives", 
                                              rep("Other purpose", 3))) %>%
                as.character(),
             Variable = "Number in country",
             Type = "Purpose of visit",
             Year = substring(TimePeriod, 1, 4)) %>%
      group_by(Dimension, TimePeriod, Type, Variable, Year) %>%
      summarise(Value = sum(Value)) %>%
      data.frame()
   
   arriv <- rbind(arriv1, arriv2, arriv3, arriv4)
   
   all_povs <- arriv2$Dimension %>% unique() %>% as.character()
   save(all_povs, file = savepath("all_povs"))
   ## Code taken from itm_and_ivs_overview.R
   ###########################################
   
   arriv %>%
      addmeta(dataset_names) %>%
      saverda("arriv")
}

intl_visitor_spend = function(dataset_names){
   ###########################################
   ## Code taken from itm_and_ivs_overview.R
   ## With modifications to add "Total (All Countries)"
   #-----------------Data of Visitor spending--------------------

   ivs1_raw <- sqlQuery(TRED, "select
                    sum(WeightedSpend * PopulationWeight) as Spend,
                    sum(PopulationWeight) as Pop,
                    sum(PopulationWeight * lengthofstay) as PopNights,
                    CORNextYr as Country,
                    Year,
                    Qtr 
                    from production.vw_IVSSurveyMainHeader
                    group by Year, Qtr, CORNextYr",
                    stringsAsFactors = FALSE) %>%
      mutate(CountryGrouped = as.character(CountryGroup(Country)))
   ## Compute "Total (All Countries)"
   ivs1_total = ivs1_raw %>%
      group_by(Year, Qtr) %>%
      summarise(Spend = sum(Spend),
                Pop = sum(Pop),
                PopNights = sum(PopNights)) %>%
      cbind(CountryGrouped = "Total (All Countries)") %>%
      mutate(CountryGrouped = as.character(CountryGrouped))
   ## Merge and process
   ivs1_merge = full_join(ivs1_raw, ivs1_total, by = names(ivs1_total))
   ivs1 = ivs1_merge %>%
      group_by(CountryGrouped, Year, Qtr) %>%
      summarise(Spend = sum(Spend),
                SpendPerTrip = sum(Spend) / sum(Pop),
                SpendPerNight = sum(Spend) / sum(PopNights),
                Spend = Spend / 1000000) %>%
      mutate(Qtr = substring(Qtr, 6, 6) %>% as.numeric(),
             TimePeriod = as.Date(paste(Year, Qtr * 3, "01", sep="-"))) %>%
      select(-Qtr) %>%
      data.frame() %>%
      mutate(CountryGrouped = as.character(CountryGrouped)) %>%
      gather(Variable, Value, Spend:SpendPerNight) %>%
      mutate(Variable = rename.levels(Variable,
                                      orig=c("Spend", "SpendPerTrip", "SpendPerNight"),
                                      new = c("Total spend", "Spend per trip", "Spend per night")) %>% as.character(),
             Type = "Country of residence") %>%
      rename(Dimension = CountryGrouped) %>%
      arrange(TimePeriod) 

   ivs2 <- sqlQuery(TRED, "select
                    sum(WeightedSpend * PopulationWeight) as Spend,
                    sum(PopulationWeight) as Pop,
                    sum(PopulationWeight * lengthofstay) as PopNights,
                    POV,
                    Year,
                    Qtr 
                    from production.vw_IVSSurveyMainHeader
                    group by Year, Qtr, POV",
                    stringsAsFactors = FALSE) %>%
      group_by(POV, Year, Qtr) %>%
      summarise(Spend = sum(Spend),
                SpendPerTrip = sum(Spend) / sum(Pop),
                SpendPerNight = sum(Spend) / sum(PopNights),
                Spend = Spend / 1000000) %>%
      mutate(Qtr = substring(Qtr, 6, 6) %>% as.numeric(),
             TimePeriod = as.Date(paste(Year, Qtr * 3, "01", sep="-"))) %>%
      select(-Qtr) %>%
      data.frame() %>%
      gather(Variable, Value, Spend:SpendPerNight) %>%
      mutate(Variable = rename.levels(Variable,
                                      orig=c("Spend", "SpendPerTrip", "SpendPerNight"),
                                      new = c("Total spend", "Spend per trip", "Spend per night")) %>% as.character(),
             Type = "Purpose of visit",
             POV = POV %>% factor() %>% rename.levels(orig="Education", new="Other purpose") %>% as.character()) %>%
      rename(Dimension = POV) %>%
      arrange(TimePeriod)

   ivs <- rbind(ivs1, ivs2) 
   ## Code taken from itm_and_ivs_overview.R
   ###########################################
   
   ivs %>%
      addmeta(dataset_names) %>%
      saverda("ivs")
}
