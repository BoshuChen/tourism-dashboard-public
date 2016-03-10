## Prep data script for shiny app
## Used for: Industry - NZ business demography
## Last Updated: 11 November 2015
## Outputs:
## - bdem_countsize.rda
## - bdem_birthdeath.rda
## - bdem_btype.rda
## - bdem_overseas.rda
## Author: Jimmy Oh
## Peer reviewed by: 

business_demography = function(dataset_names){
   ## Enterprises by Size
   ## Only want the top-level industry headings + Total
   bdem_countsize = ImportTS2(TRED, "Enterprises by employee count size and industry 2000-14") %>%
      select(TimePeriod, Industry = CV1, Size = CV2, Unit, Value) %>%
      filter(grepl("^(([[:alpha:]])|(Total)) ", Industry)) %>%
      mutate(Industry = factor(Industry)) %>%
      addmeta(dataset_names) %>%
      saverda("bdem_countsize")
   
   ## Births/Deaths by Region/TA
   bdem_birthdeath = ImportTS2(TRED, "Geographic units births and deaths by territorial authority and industry 2001 -14") %>%
      select(TimePeriod, Industry = CV1, Area = CV2, Type = CV3, Unit, Value) %>%
      addmeta(dataset_names) %>%
      saverda("bdem_birthdeath")
   
   ## Enterprises by Business Type
   bdem_btype = ImportTS2(TRED, "Enterprises by industry and business type 2000-14") %>%
      select(TimePeriod, Industry = CV2, BusinessType = CV1, Unit, Value) %>%
      addmeta(dataset_names) %>%
      saverda("bdem_btype")
   
   ## Enterprises by Overseas Equity
   bdem_overseas = ImportTS2(TRED, "Enterprises by industry and overseas equity 2000-14") %>%
      select(TimePeriod, Industry = CV2, OverseasEquity = CV1, Unit, Value) %>%
      addmeta(dataset_names) %>%
      saverda("bdem_overseas")
}
