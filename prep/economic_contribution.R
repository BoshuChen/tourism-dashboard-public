## Prep data script for shiny app
## Used for: Overview - Economic Contribution
## Last Updated: 3 November 2015
## Outputs:
## - tourism_economic.rda
## - tourism_percexports.rda
## - tourism_expenditure.rda
## Author: Jimmy Oh
## Peer reviewed by: 

economic_contribution = function(dataset_names){
   ## Value added by tourism
   tourism_value_raw = ImportTS2(TRED, "Tourism expenditure by component (ANZSIC06) (Annual-Mar)")
   tourism_valueadd = tourism_value_raw %>%
      filter(grepl("irect tourism value added$", CV1)) %>%
      select(TimePeriod, Type = CV1, Value) %>%
      mutate(Type = gsub(" tourism value.+$", "", Type)) %>%
      mutate(Value = Value * 10^6)
   ## ValuePerc = value added as a percentage of contribution to GDP
   tourism_valueadd_perc = tourism_value_raw %>%
      filter(grepl("irect tourism value added as a percentage", CV1)) %>%
      select(TimePeriod, Type = CV1, ValuePerc = Value) %>%
      mutate(Type = gsub(" tourism value.+$", "", Type))

   ## % of total employed, employed in tourism
   tourism_employment_raw = ImportTS2(TRED, "Summary of Tourism Employment (ANZSIC06) LEED (Annual-Mar)")
   tourism_employment = tourism_employment_raw %>%
      filter(grepl("irectly employed in tourism$", CV1)) %>%
      select(TimePeriod, Type = CV1, Employment = Value) %>%
      mutate(Type = gsub("(^Number of people )|(ly employed.+$)", "", Type)) %>%
      mutate(Type = gsub("^(\\w)", "\\U\\1", Type, perl = TRUE))
   ## EmployPerc = % of total employment, employed in tourism
   tourism_employment_perc = tourism_employment_raw %>%
      filter(grepl("irectly employed in tourism as a percentage", CV1)) %>%
      select(TimePeriod, Type = CV1, EmploymentPerc = Value) %>%
      mutate(Type = gsub("(^Number of people )|(ly employed.+$)", "", Type)) %>%
      mutate(Type = gsub("^(\\w)", "\\U\\1", Type, perl = TRUE))
   
   ## Merge together
   tourism_valuemerge = merge(tourism_valueadd, tourism_valueadd_perc)
   tourism_employmentmerge = merge(tourism_employment, tourism_employment_perc)
   tourism_economic = merge(tourism_valuemerge, tourism_employmentmerge)
   tourism_economic %>%
      addmeta(dataset_names) %>%
      saverda("tourism_economic")

   ## Intl tourism as % of total exports
   tourism_percexports = ImportTS2(TRED, "Summary of Tourism Expenditure by type of tourist (ANZSIC06) (Annual-Mar)") %>%
      filter(CV1 == "International tourism as a percentage of total exports") %>%
      select(TimePeriod, Value)
   tourism_percexports %>%
      addmeta(dataset_names) %>%
      saverda("tourism_percexports")

   ## Tourism expenditure
   tourism_expenditure = ImportTS2(TRED, "Summary of Tourism Expenditure by type of tourist (ANZSIC06) (Annual-Mar)") %>%
      filter(grepl("^((International)|(Domestic)).+expenditure$", CV1)) %>%
      select(TimePeriod, Type = CV1, Value)
   tourism_expenditure %>%
      addmeta(dataset_names) %>%
      saverda("tourism_expenditure")
}
