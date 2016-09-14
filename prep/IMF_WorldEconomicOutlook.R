## Prep data script for shiny app
## Used for:
##    Economic Context - Main Market GDP
## Last Updated: 30 August 2016
## Outputs:
## - imf_weo.rda
## Author: Jimmy Oh
## Edited: Talosaga Talosaga 2016-09-12 Change GDP measure to GDP, constant prices in national currency base. Was previously nominal USD$.
## Peer reviewed by: 

## Read from a csv downloaded from the IMF's "World Economic Outlook database"
## Downloading the "By Countries" version from:
##  http://www.imf.org/external/pubs/ft/weo/2016/01/weodata/download.aspx
## For size, it has been gzipped manually, but can use raw csv instead
##  (just need to adjust code and remove `gzfile` call)
## The script then parses in the needed bits of data and saves to rda for use by shiny
IMF_WorldEconomicOutlook = function(dataset_names){
   weo_raw = read.delim(gzfile(paste0(prepdir, "IMF_WorldEconomicOutlook_Apr2016all.csv.gz")), check.names = FALSE, stringsAsFactors = FALSE)
   
   mm_countries = c("New Zealand", "Australia", "China",
                    "United Kingdom", "United States",
                    "Germany", "Korea", "Canada", "Japan")
   
   weo_filter = weo_raw %>%
      filter(Country %in% mm_countries,
             `WEO Subject Code` == "NGDP_R") %>%
      select(Country, EstFrom = `Estimates Start After`, starts_with("2")) %>%
      gather(Year, Value, starts_with("2")) %>%
      mutate(Country = factor(Country),
             EstFrom = as.numeric(EstFrom),
             Year = as.numeric(Year),
             Value = as.numeric(gsub(",", "", Value)))
   
   weo_filter %>%
      addmeta(dataset_names) %>%
      saverda("imf_weo")
}
