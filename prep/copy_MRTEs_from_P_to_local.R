## purpose: this script copy the most recent MRTEs data MVdata.rda from P drive to the local data drive
##          whoever runs the MRTEs process will produce the MVdata.rda and save it in the this designated folder

# 1. copy the most recent MRTEs data from P: drive
MRTEs_file_on_P <- 'P:/OTSP/Regional Estimation/Regional Estimates/MRTEs_to_tourism_dashboard/MVdata.rda'
MRTEs_file_on_local <- paste0(getwd(),'/shiny/new/data/')
file.copy( MRTEs_file_on_P, MRTEs_file_on_local, overwrite = TRUE )
