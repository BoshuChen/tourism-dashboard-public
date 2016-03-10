## Prep data script for shiny app
## Used for:
##    Regions - Regional Summaries
## Last Updated: 20 November 2015
## Outputs:
## - rs_data.rda
## Author: Jimmy Oh
## Peer reviewed by: 

## Regional Summaries requires both RTI and RTE data
## Code for RTI data is adapted from:
##   disseminate_rtes/dissemination_code/regional_summaries/download_RTIs.R
##   RTI-review/regional_summaries/dissemination_code/groom_data.R
## RTE data draws on a csv file of the latest RTE sent by email.
## Eventually, it should be updated to draw from TRED directly by someone who knows how.
rs_data = function(dataset_names){
   #########
   ## RTI ##
   #########
   ## Step 1: Download the concordance we need to be able to concord these
   Conc_TLA <- sqlQuery(TRED, "Select distinct L2Description, L3Description
                           from Classifications.vw_ClassificationLevels cl
                           where cl.L3ClassificationName = 'MBIEInt_TAMOD_1_Geography'  
                           and cl.L2ClassificationName = 'SNZ_TA11_1_Geography'")

   Conc_RTO <- sqlQuery(TRED, "Select distinct L1Description, L2Description
                           from Classifications.vw_ClassificationLevels cl
                           where cl.L2ClassificationName = 'MBIEInt_TAMOD_1_Geography'  
                           and cl.L1ClassificationName = 'MBIEPub_RTO_1_Geography'")

   Conc_RC <- sqlQuery(TRED, "Select distinct L2Description, L3Description
                           from Classifications.vw_ClassificationLevels cl
                           where cl.L3ClassificationName = 'MBIEInt_TAMOD_1_Geography'  
                           and cl.L2ClassificationName = 'MBIEPub_RegCouncil_1_Geography'")
   
   Conc <- merge(Conc_TLA, Conc_RTO, by.x = "L3Description", by.y = "L2Description", all = TRUE)
   Conc <- merge(Conc, Conc_RC, by = "L3Description", all = TRUE)
   names(Conc) <- c("TA_NAME", "SNZ_TA", "RTO_NAME", "REGION_NAME")

   ## Step 2: Download the RTI data (note that later you can bring these two steps together but for now, just do it old school in two steps)
   AllRTIs <- sqlQuery(TRED,
   "select
      main.RecordType,
      main.Year,
      main.MonthNumber,
      spend.Merchant,
      sum(spend.SpendAmount) as Spend
   from
      vw_RTISurveyMainHeader main,
      vw_RTISpend spend
   where
      main.SurveyResponseID = spend.SurveyResponseID
   group by
      main.RecordType,
      main.Year,
      main.MonthNumber,
      spend.Merchant")
   AllRTIs$Period = local({
      ty = as.numeric(AllRTIs$Year)
      tm = as.numeric(AllRTIs$MonthNumber)
      td = "01"
      ty[tm == 12] = ty[tm == 12] + 1
      tm = tm + 1
      tm[tm == 13] = 1

      as.Date(paste(ty, tm, td, sep = "-")) - 1
   })

   ## Merge with RTO, Region and TA names
   RTIs_concorded <- merge(AllRTIs, Conc, 
                     by.x="Merchant", by.y="TA_NAME", all.x=TRUE)

   names(RTIs_concorded) <- gsub("RTO_NAME", "RTO", names(RTIs_concorded))
   names(RTIs_concorded) <- gsub("REGION_NAME", "Region", names(RTIs_concorded))
   names(RTIs_concorded) <- gsub("SNZ_TA", "TA", names(RTIs_concorded))
   
   #########
   ## RTE ##
   #########
   RTEs_concorded = read.csv(gzfile(paste0(prepdir, "RTE_concorded_2015.csv.gz")))
   names(RTEs_concorded) = gsub("Territorial_Authority", "TA", names(RTEs_concorded))

   arealist = list(
      Region = levels(RTEs_concorded$Region),
      RTO = levels(RTEs_concorded$RTO),
      TA = levels(RTEs_concorded$TA)
   )
   ## not enough data for Chatham Islands
   arealist = within(arealist, {
      RTO = RTO[RTO != "Chatham Islands"]
      TA = TA[TA != "Chatham Islands"]
   })

   CurrentYear <- max(RTEs_concorded$YEMar)
   
   ##########
   ## Save ##
   ##########
   rs_data = list(
      RTE = RTEs_concorded,
      RTI = RTIs_concorded[-which(names(RTIs_concorded) %in% c("Merchant", "Year", "MonthNumber"))],
      arealist = arealist
   )
   rs_data %>%
      addmeta(dataset_names) %>%
      saverda("rs_data")
}
