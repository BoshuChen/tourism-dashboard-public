## Prep data script for shiny app
## Used for: Regions - Visitor Nights
## Last Updated: 3 November 2015
## Outputs:
## - nights_origin.rda
## - nights_region.rda
## Author: Jimmy Oh
## Peer reviewed by:

nights_origin = function(dataset_names){
   ## Code should ideally grab YrEndingMth from data to compute Date
   ## But getting the correct end of month day is annoying
   ##  so done manually for now.
   dom_nights_origin =
      ImportREAR("Domestic visitor nights by origin", PlayPen) %>%
      mutate(Date = as.Date(paste0("31-12-", Year), format = "%d-%m-%Y")) %>%
      select(Date, Area, AreaType, Origin = Dimension1, Value) %>%
      filter(AreaType != "Total", grepl(" Region", Origin)) %>%
      mutate(AreaType = factor(AreaType), Origin = factor(Origin))
   int_nights_origin =
      ImportREAR("International visitor nights by origin", PlayPen) %>%
      mutate(Date = as.Date(paste0("30-06-", Year), format = "%d-%m-%Y")) %>%
      select(Date, Area, AreaType, Origin = Dimension1, Value)
   nights_origin = list(dom = dom_nights_origin, int = int_nights_origin)
   nights_origin %>%
      addmeta(dataset_names) %>%
      saverda("nights_origin")
}

nights_region = function(dataset_names){
   nights_region = ImportTS2(TRED, "Guest Nights by Region (Monthly)")
   nights_region %>%
      addmeta(dataset_names) %>%
      saverda("nights_region")
}
