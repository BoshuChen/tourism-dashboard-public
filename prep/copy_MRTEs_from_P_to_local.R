## purpose: this script copy the most recent MRTEs data MVdata.rda from P drive to the local data drive
##          whoever runs the MRTEs process will produce the MVdata.rda and save it in the this designated folder

copy_MRTEs_from_P_to_local = function(dataset_names){
   # 1. copy the most recent MRTEs data from P: drive
   MRTEs_file_on_P <- 'P:/OTSP/Regional Estimation/Regional Estimates/MRTEs_to_tourism_dashboard/MVdata.rda'

   load(MRTEs_file_on_P)
   MVdata %>%
      addmeta(dataset_names) %>%
      saverda("MVdata")
}
