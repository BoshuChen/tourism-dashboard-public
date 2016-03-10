## Prep data script for shiny app
## Used for: Regions - Contribution to Regional GDP
## Last Updated: 11 November 2015
## Outputs:
## - GDP_tourism.rda
## Author: Jimmy Oh
## Peer reviewed by: 

GDP_tourism = function(dataset_names){
   GDP_tourism = ImportREAR("International tourism expenditure as a share of regional GDP", PlayPen) %>%
      mutate(Date = as.Date(paste0("31-03-", Year), format = "%d-%m-%Y")) %>%
      select(Date, Area, AreaType, Value)
   GDP_tourism %>%
      addmeta(dataset_names) %>%
      saverda("GDP_tourism")
}

