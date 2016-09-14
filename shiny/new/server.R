source("shared_load.R")
source("plots_funcs.R")
source("server_pack.R")

shinyServer(function(input, output, session){
   env_serv = environment()
   ###########
   ## Front ##
   ###########
   frontp(env_serv)
   
   ##############
   ## Overview ##
   ##############
   ## Economic Contribution
   oeco(env_serv)
   
   ## International Visitor Arrivals
   oiva(env_serv)
   ## International Visitor Spend
   oivs(env_serv)
   ## International Visitor Arrivals vs Spend
   oivas(env_serv)
   
   #############
   ## Regions ##
   #############
   ## Regional Summary
   rs_all(env_serv)
   
   ## Accommodation
   racc(env_serv)
   
   ## Industry Sectors
   rins(env_serv)
   
   ## Contribution to Regional GDP
   rgdp(env_serv)
   
   ##############
   ## Industry ##
   ##############
   ## Accommodation
   iacc(env_serv)
   
   ## Attractions and Activities
   iact(env_serv)
   
   ## Business Demography
   ibdem(env_serv)
   
   ## Business Events (CAS)
   icas(env_serv)
   
   ## Spend by Visitor Market
   isvm(env_serv)
   
   #####################
   ## Visitor Markets ##
   #####################
   vmorigin(env_serv)
   vmdest(env_serv)
   
   ## Accommodation Used
   vmacc(env_serv)
   
   ######################
   ## Economic Context ##
   ######################
   ## Exchange Rates
   gcforex(env_serv)
   
   ## Tourism-related Share Prices
   ecshare(env_serv)
   
   ## Economic Growth Forecasts
   ecgrof(env_serv)
})
