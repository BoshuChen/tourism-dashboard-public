## Prep data script for shiny app
## Used for:
##    Regions - Industry Sectors
##    Industry - Spend by Visitor Market
##    Visitor Markets - Compare Origins
##    Visitor Markets - Compare Destinations
## Last Updated: 20 November 2015
## Outputs:
## - all_rte.rda
## - all_rte_levels.rda
## Author: Jimmy Oh
## Peer reviewed by: 

## This code is essentially the same as disseminate_rtes/dissemination_code/groom_data.R
## It draws on a csv file of the latest RTE sent by email.
## Eventually, it should be updated to draw from TRED directly by someone who knows how.
RTE_data = function(dataset_names){
   all_rte_raw = read.csv(gzfile(paste0(prepdir, "RTE_concorded_2015.csv.gz")))

   ############################
   ## Create Totals for Origin
   ############################
   tot_origin = list(
      all = "Total (All)",
      dom = "Total (Domestic)",
      int = "Total (International)"
   )
   all_vars = colnames(all_rte_raw)
   ind_spend = which(all_vars == "Spend")
   rem_ind = c(which(all_vars == "Type"), which(all_vars == "Origin"))
   rem_vars = all_vars[-c(rem_ind, ind_spend)]
   total_all = all_rte_raw %>%
      group_by_(.dots = rem_vars) %>%
      summarise(Spend = sum(Spend)) %>%
      cbind(Type = "All", Origin = tot_origin$all)
   total_dom = all_rte_raw %>%
      filter(Type == "Domestic") %>%
      group_by_(.dots = rem_vars) %>%
      summarise(Spend = sum(Spend)) %>%
      cbind(Type = "Domestic", Origin = tot_origin$dom)
   total_int = all_rte_raw %>%
      filter(Type == "International") %>%
      group_by_(.dots = rem_vars) %>%
      summarise(Spend = sum(Spend)) %>%
      cbind(Type = "International", Origin = tot_origin$int)
   ## suppress warnings that result from join
   suppressWarnings({
      total_joined = total_all %>%
                     full_join(total_dom) %>%
                     full_join(total_int)
      all_rte = full_join(all_rte_raw, total_joined)
   })

   ## Turn Type back into a factor, but with "All" first
   all_rte$Type = factor(all_rte$Type, levels = c("All", "Domestic", "International"))
   ## Turn Origin back into a factor, but with the levels in a certain order:
   ## 1) Total (All)
   ## 2) Domestic origins, with Total first
   ## 3) International origins, with Total first and "Rest of"|"Africa and Middle East" last
   dom_levels = unique(as.character(all_rte_raw$Origin[all_rte_raw$Type == "Domestic"]))
   int_levels = unique(as.character(all_rte_raw$Origin[all_rte_raw$Type == "International"]))
   ind_rest = grep("(^Rest of)|(^Africa and Middle East)", int_levels)
   dom_levels = c(tot_origin$dom, dom_levels)
   int_levels = c(tot_origin$int, c(int_levels[-ind_rest], int_levels[ind_rest]))
   all_rte$Origin = factor(all_rte$Origin, levels = c(tot_origin$all, dom_levels, int_levels))

   all_rte %>%
      addmeta(dataset_names) %>%
      saverda("all_rte")

   ## Also save just the levels for faster access later
   all_levels = lapply(all_rte, levels)
   all_levels$OriginDom = dom_levels
   all_levels$OriginInt = int_levels
   ## Add Totals to levels
   all_levels$Product = c("Total (All Products)", all_levels$Product)
   all_levels$RTO = c("Total (All RTOs)", all_levels$RTO)
   all_levels$Region = c("Total (All Regions)", all_levels$Region)
   all_levels$Territorial_Authority = c("Total (All Territorial Authorities)", all_levels$Territorial_Authority)
      
   all_levels %>%
      addmeta(dataset_names) %>%
      saverda("all_rte_levels")
}
