## Prep data script for shiny app
## Used for: Industry - Activities
## Last Updated: 3 November 2015
## Outputs:
## - ivs_accomm_used.rda
## Author: Jimmy Oh
## Peer reviewed by: 

ivs_accomm_used = function(dataset_names){
   ## Set database
   sqlQuery(TRED, "use production")
   
   ## Get data
   ivs_acc_raw = sqlQuery(TRED,
   "select
      main.Year,
      main.Qtr,
      main.CORNextYr as Country,
      main.MainAccommTypeUsed as AccommType,
      sum(main.PopulationWeight) as Pop,
      sum(main.PopulationWeight * main.WeightedSpend) as Spend
   from
      Production.vw_ivssurveymainheader main
   group by
      main.Year,
      main.Qtr,
      main.CORNextYr,
      main.MainAccommTypeUsed")

   ## Use mbie function to group/rename countries
   ivs_acc_raw$Country = CountryGroup(ivs_acc_raw$Country)
   ## Some processing
   ## -On Dates-
   ## Using old code that assumes the dates are month start
   ## But they are probably month end?
   ## So 2015-03-31 instead of 2015-03-01
   ##
   ## -On Averages-
   ## AverageNights, SpendPerNight, SpendPerTrip need to be computed
   ##  at run-time, not here, for correct computation (due to further
   ##  aggregation later).
   ## But code is here (and commented out), to show how it will be
   ##  computed eventually.
   ivs_accomm_used = ivs_acc_raw %>%
      mutate(Date = as.Date(paste(Year,
         as.numeric(substring(Qtr, 6, 6)) * 3, "01", sep = "-"))) %>%
      group_by(Date, Country, AccommType) %>%
      summarise(
         # AverageNights = sum(PopNights)/sum(Pop),
         # SpendPerNight = sum(Spend)/sum(PopNights),
         # SpendPerTrip = sum(Spend)/sum(Pop),
         TotalSpend = sum(Spend),
         Pop = sum(Pop)
         #PopNights = sum(PopNights)
      )
   class(ivs_accomm_used) = "data.frame"
   ivs_accomm_used %>%
      addmeta(dataset_names) %>%
      saverda("ivs_accomm_used")
}
