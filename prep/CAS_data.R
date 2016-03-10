## Prep data script for shiny app
## Used for: Industry - Business Events (CAS)
## Last Updated: 3 November 2015
## Outputs:
## - all_cas.rda
## Author: Jimmy Oh
## Peer reviewed by: 

## This code is essentially the same as CAS/dissemination_code/groom_data.R
## It draws on a pivot table downloaded manually from the internet.
## It should be updated to draw from TRED directly by someone who knows how.
CAS_data = function(dataset_names){
   cas_raw = read.csv(gzfile(paste0(prepdir, "CAS_Pivot_YEMar_2015.csv.gz")),
                      header = FALSE, stringsAsFactors = FALSE)

   ## Remove empty columns
   cas_work = cas_raw[,apply(cas_raw, 2, function(x) !all(is.na(x)))]

   ## Separate out shared columns
   cas_dates = cas_work[[1]][-(1:2)]
   cas_dates = gsub(" Q1", "/03/31", cas_dates)
   cas_dates = gsub(" Q2", "/06/30", cas_dates)
   cas_dates = gsub(" Q3", "/09/30", cas_dates)
   cas_dates = gsub(" Q4", "/12/31", cas_dates)
   cas_share = data.frame(Period = as.Date(cas_dates, format = "%Y/%m/%d"),
                          Region = cas_work[[2]][-(1:2)])

   ## Get the parent headings, excluding shared
   cas_data = cas_work[,-(1:2)]
   parent_start = which(cas_data[1,] != "")
   parent_end = c(parent_start[-1] - 1, ncol(cas_data))
   parent_cols = list()
   for(i in 1:length(parent_start))
      parent_cols[[i]] = parent_start[i]:parent_end[i]

   ## Split each section into its own list element
   all_cas = list()
   for(i in 1:length(parent_cols)){
      curpart = data.frame(lapply(cas_data[-c(1:2),parent_cols[[i]],drop=FALSE], as.numeric))
      names(curpart) = cas_data[2,parent_cols[[i]]]
      all_cas[[i]] = cbind(cas_share, curpart)
      names(all_cas)[i] = cas_data[1,parent_cols[[i]][1]]
   }

   ## Tidy Names
   tofix = list(
      c("Delegate capacity", " for the region", ""),
      c("Customer type", "^ Events run for (\\w)", "For \\U\\1"),
      c("Total delegates by origin", "^ (\\w)", "\\U\\1"),
      c("Delegate days by event type", " delegate days", ""),
      c("Total delegates by event type", " delegates", ""),
      c("Duration of conferences/conventions", " conferences/conventions", ""),
      c("Delegates at single/multi day events", " $", ""),
      c("Delegates at single/multi day conferences", " $", ""),
      c("Event size for different event types", "^ (\\w)", "\\U\\1")
   )
   for(i in 1:length(tofix))
      names(all_cas[[tofix[[i]][1]]]) =
         gsub(tofix[[i]][2],
              tofix[[i]][3],
              names(all_cas[[tofix[[i]][1]]]), perl = TRUE)
   
   all_cas %>%
      addmeta(dataset_names) %>%
      saverda("all_cas")
}
