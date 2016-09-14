## Prep data script for shiny app
## Used for: Industry - Activities
## Last Updated: 11 November 2015
## Outputs:
## - ivs_act.rda
## - ivs_actcat.rda
## Author: Jimmy Oh
## Peer reviewed by: 

ivs_act = function(dataset_names){
   ## Set database
#    sqlQuery(TRED, "use production")
   
   ## Get data
	actcomb <-
		sqlQuery(TRED,"
				select
					surveyresponseID,
					activitycategory
				from
					Production.vw_ivsactivities
            where
               activity not in ('None of these', 'Not sure')
				group by
					surveyresponseID,
					activitycategory
				order by
					activitycategory
					") %>% 
		group_by(surveyresponseID) %>% 
		summarise(activitycategory = paste(activitycategory, collapse = ","))

	main <- sqlQuery(TRED, "SELECT * FROM Production.vw_ivssurveymainheader")

	ivs_act_raw <- sqldf::sqldf(
		"select
			main.Year,
			main.Qtr,
			main.CORNextYr as Country,
			actcomb.activitycategory as ActivityConcat,
			sum(main.PopulationWeight) as Pop,
			sum(main.PopulationWeight * main.WeightedSpend) as Spend,
			sum(main.PopulationWeight * main.lengthofstay) as PopNights
		from
			main
		left join actcomb
				on main.surveyresponseID = actcomb.surveyresponseID
		group by
			main.Year,
			main.Qtr,
			main.CORNextYr,
			actcomb.activitycategory")

   ## Use mbie function to group/rename countries
   ivs_act_raw$Country = CountryGroup(ivs_act_raw$Country)
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
   ivs_act = ivs_act_raw %>%
      mutate(Date = as.Date(paste(Year,
         as.numeric(substring(Qtr, 6, 6)) * 3, "01", sep = "-"))) %>%
      group_by(Date, Country, ActivityConcat) %>%
      summarise(
         # AverageNights = sum(PopNights)/sum(Pop),
         # SpendPerNight = sum(Spend)/sum(PopNights),
         # SpendPerTrip = sum(Spend)/sum(Pop),
         TotalSpend = sum(Spend),
         Pop = sum(Pop),
         PopNights = sum(PopNights)
      )
   class(ivs_act) = "data.frame"
   ivs_act %>%
      addmeta(dataset_names) %>%
      saverda("ivs_act")
   
   ## Generate list of breakdowns of each activity category
   ivs_act_breakdown = sqlQuery(TRED,
   "select
      activity,
      activitycategory
   from
      Production.vw_ivsactivities
   where
      activity not in ('None of these', 'Not sure')
   group by
      activity,
      activitycategory
   order by
      activitycategory,
      activity
   ")
   ivs_actcat = list()
   for(curcat in levels(ivs_act_breakdown$activitycategory))
      ivs_actcat[[curcat]] = with(ivs_act_breakdown, as.character(activity[activitycategory == curcat]))
   ivs_actcat %>%
      saverda("ivs_actcat")
}
