## Prep data script for shiny app
## Used for: Industry - Accommodation
## Last Updated: 3 November 2015
## Outputs:
## - Accomm_RTO.rda
## Author: Jimmy Oh
## Peer reviewed by: 

Accomm_RTO = function(dataset_names){
   Accomm_RTO = ImportTS2(TRED, "RTO by variables (Monthly)") %>%
      select(TimePeriod, Accommodation_Type = CV1, RTO = CV2, Class = CV3, Value)
   class(Accomm_RTO) = "data.frame"
   Accomm_RTO %>%
      addmeta(dataset_names) %>%
      saverda("Accomm_RTO")
}
