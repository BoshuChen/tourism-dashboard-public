frontp = function(env_serv) with(env_serv, local({
   ## Front page demo plots
   demo_type = "country"
   demo_typename = "Country of residence"
   demo_variable = "Total spend"
   demo_dimensions = c("China", "Japan")
   dat_spread = ivs %>%
      filter(Type == demo_typename,
             Dimension %in% demo_dimensions,
             Variable == demo_variable,
             !is.na(Value)) %>%
      spread(Dimension, Value)
   pal = paldy(demo_dimensions, country_pov[[demo_type]])
   curliststr = liststr(demo_dimensions)
   curtitlestr = paste("from", curliststr)
   curtitle = paste(demo_variable, curtitlestr, "(Quarterly)")
   curdat = list(date = dat_spread$TimePeriod, num = dat_spread[demo_dimensions])
   observe({
      curylab = obsrollylab(input$frontdyobsroll, demo_variable)
      output$frontdyout = dyquick(curdat, input$frontdyobsroll, 4, "dollar-m",
                                  pal, curylab, curtitle, "frontdyLegend")
   })
   ## Handle drawing and download of Data Table
   tabdat = cbind(Date = curdat$date, curdat$num)
   output$frontTable = renderDataTable(tabdat,
      options = list(order = list(c("0", "desc")), pageLength = list(10)))
   output$frontTableDown = tableComboDown(tabdat, curtitle)
}))

oeco = function(env_serv) with(env_serv, {
   ## Overview - Economic Contribution
   pal2 = paldy(unique(tourism_economic$Type), unique(tourism_economic$Type))
   pal1 = c(Domestic = mbie.cols2(4), International = mbie.cols2(3))
   
   ## GDP contribution as %
   dat_spread = tourism_economic %>%
      select(TimePeriod, Type, ValuePerc) %>%
      spread(Type, ValuePerc)
   curtitle = paste(
      "Value added by tourism as a percentage of GDP",
      "(year ended March)"
   )
   curdat = list(date = dat_spread$TimePeriod,
                 num = dat_spread[c("Indirect", "Direct")])
   output$oecogdpperc = dyquick(curdat, "Obs", 1, "percent-dp1",
                           pal2, NULL, curtitle, "oecogdplabel",
                           labelsSeparateLines = FALSE, stacked = TRUE)
   
   ## GDP contribution
   dat_spread = tourism_economic %>%
      select(TimePeriod, Type, Value) %>%
      mutate(Value = Value/10^6) %>%
      spread(Type, Value)
   curtitle = paste(
      "Value added by tourism",
      "(year ended March)"
   )
   curdat = list(date = dat_spread$TimePeriod,
                 num = dat_spread[c("Indirect", "Direct")])
   output$oecogdp = dyquick(curdat, "Obs", 1, "dollar-m",
                           pal2, NULL, curtitle, "oecogdplabel",
                           labelsSeparateLines = FALSE, stacked = TRUE)
   
   ## Employment contribution as %
   dat_spread = tourism_economic %>%
      select(TimePeriod, Type, EmploymentPerc) %>%
      spread(Type, EmploymentPerc)
   curtitle = paste(
      "Tourism employment as a percentage of total employment",
      "(year ended March)"
   )
   curdat = list(date = dat_spread$TimePeriod,
                 num = dat_spread[c("Indirect", "Direct")])
   output$oecoempperc = dyquick(curdat, "Obs", 1, "percent-dp1",
                           pal2, NULL, curtitle, "oecoemplabel",
                           labelsSeparateLines = FALSE, stacked = TRUE)
   
   ## Employment contribution
   dat_spread = tourism_economic %>%
      select(TimePeriod, Type, Employment) %>%
      mutate(Employment = Employment/10^3) %>%
      spread(Type, Employment)
   curtitle = paste(
      "Tourism employment (in 000s)",
      "(year ended March)"
   )
   curdat = list(date = dat_spread$TimePeriod,
                 num = dat_spread[c("Indirect", "Direct")])
   output$oecoemp = dyquick(curdat, "Obs", 1, "number",
                           pal2, NULL, curtitle, "oecoemplabel",
                           labelsSeparateLines = FALSE, stacked = TRUE)
   
   ## Tourism as % of Exports
   dat_spread = tourism_percexports
   curtitle = paste(
      "International tourism as a percentage of total exports",
      "(year ended March)"
   )
   curdat = list(date = dat_spread$TimePeriod,
                 num = data.frame(International = dat_spread$Value))
   output$oecoexpperc = dyquick(curdat, "Obs", 1, "percent-dp1",
                           pal1, NULL, curtitle, "oecoexplabel",
                           labelsSeparateLines = FALSE, stacked = TRUE)
   
   ## Total Intl Tourism Spend
   dat_spread = tourism_expenditure %>%
      mutate(Type = factor(gsub(" tourism expenditure", "", Type))) %>%
      spread(Type, Value)
   curtitle = paste(
      "Tourism expenditure",
      "(year ended March)"
   )
   curdat = list(date = dat_spread$TimePeriod,
                 num = dat_spread[c("International", "Domestic")])
   output$oecospend = dyquick(curdat, "Obs", 1, "dollar-m",
                           pal1, NULL, curtitle, "oecoexplabel",
                           labelsSeparateLines = FALSE, stacked = TRUE)
})

oiva = function(env_serv) with(env_serv, {
   ## Overview - International Visitor Arrivals
   observe({
      arrivName = names(arrivType[arrivType == input$oivatype])
      oivadimension = condInputServer(input, "oivadimension", arrivType, cond_simple = "oivatype")
      if(length(oivadimension) > 0){
         flist = list(Type = arrivName, Dimension = oivadimension, Variable = input$oivavariable)
         dat_spread = getdat(arriv, "TimePeriod", "Dimension", "Value", flist) %>%
            filter(!is.na(Value)) %>%
            spread(Dimension, Value)
         ## Some data may not exist, so exclude those
         oivareal = oivadimension[oivadimension %in% names(dat_spread)]
         ## Draw plot
         if(length(oivareal) > 0){
            pal = paldy(oivareal, country_pov[[input$oivatype]])
            curylab = obsrollylab(input$oivaobsroll, tolower(input$oivavariable))
            curtitlestr = switch(input$oivatype,
               country = paste("from", liststr(oivareal)),
               pov = paste("for", liststr(tolower(oivareal)))
            )
            curtitle = paste(input$oivavariable, curtitlestr, "(Monthly)")
            curdat = list(date = dat_spread$TimePeriod, num = dat_spread[oivareal])
            output$oivady = dyquick(curdat, input$oivaobsroll, 12, "number",
                                    pal, curylab, curtitle, "oivadyLegend",
                                    dateWindow = isolate(input$oivady_date_window))
      
            ## Handle drawing and download of Data Table
            tabdat = cbind(Date = curdat$date, curdat$num)
            output$oivaTable = renderDataTable(tabdat,
               options = list(order = list(c("0", "desc"))))
            output$oivaTableDown = tableComboDown(tabdat, curtitle)
         }
      }
   })
   
   ## checkboxCombo
   condComboServer(input, session, "oivadimension",
                   cats = arrivType,
                   choices = country_pov)
})

oivs = function(env_serv) with(env_serv, {
   ## Overview - International Visitor Spend
   observe({
      arrivName = names(arrivType[arrivType == input$oivstype])
      oivsdimension = condInputServer(input, "oivsdimension", arrivType, cond_simple = "oivstype")
      if(length(oivsdimension) > 0){
         flist = list(Type = arrivName, Dimension = oivsdimension, Variable = input$oivsvariable)
         dat_spread = getdat(ivs, "TimePeriod", "Dimension", "Value", flist) %>%
            filter(!is.na(Value)) %>%
            spread(Dimension, Value)
         ## Some data may not exist, so exclude those
         oivsreal = oivsdimension[oivsdimension %in% names(dat_spread)]
         ## Draw plot
         if(length(oivsreal) > 0){
            pal = paldy(oivsreal, country_pov[[input$oivstype]])
            curylab = obsrollylab(input$oivsobsroll, tolower(input$oivsvariable))
            curtitlestr = switch(input$oivstype,
               country = paste("from", liststr(oivsreal)),
               pov = paste("for", liststr(tolower(oivsreal)))
            )
            curtitle = paste(input$oivsvariable, curtitlestr, "(Quarterly)")
            curdat = list(date = dat_spread$TimePeriod, num = dat_spread[oivsreal])
            curtype = if(input$oivsvariable == "Total spend") "dollar-m" else "dollar"
            output$oivsdy = dyquick(curdat, input$oivsobsroll, 4, curtype,
                                    pal, curylab, curtitle, "oivsdyLegend",
                                    dateWindow = isolate(input$oivsdy_date_window))
      
            ## Handle drawing and download of Data Table
            tabdat = cbind(Date = curdat$date, curdat$num)
            output$oivsTable = renderDataTable(tabdat,
               options = list(order = list(c("0", "desc"))))
            output$oivsTableDown = tableComboDown(tabdat, curtitle)
         }
      }
   })
   
   ## checkboxCombo
   condComboServer(input, session, "oivsdimension",
                   cats = arrivType,
                   choices = country_pov)
})

oivas = function(env_serv) with(env_serv, {
   ## Overview - International Visitor Arrivals vs Spend
   observe({
      arrivName = names(arrivType[arrivType == input$oivastype])
      oivasdimension = condInputServer(input, "oivasdimension", arrivType, cond_simple = "oivastype")
      
      flist = list(Type = arrivName, Dimension = oivasdimension)
      ## Spend data
      flist$Variable = "Total spend"
      dat_spend = getdat(ivs, "TimePeriod", "Dimension", "Value", flist) %>%
         filter(!is.na(Value)) %>%
         spread(Dimension, Value)
      colnames(dat_spend) = gsub(oivasdimension, "Total Spend", colnames(dat_spend), fixed = TRUE)
      
      ## Arrival data
      flist$Variable = "Arrivals"
      dat_arriv = getdat(arriv, "TimePeriod", "Dimension", "Value", flist) %>%
         filter(!is.na(Value)) %>%
         spread(Dimension, Value)
      colnames(dat_arriv) = gsub(oivasdimension, "Arrivals", colnames(dat_arriv), fixed = TRUE)
      
      ## Adjust Arrival (monthly data) to Quarterly
      ## First, trim the much longer arrival data down to spend data dates
      qdate_min = min(dat_spend$TimePeriod)
      qdate_max = max(dat_spend$TimePeriod)
      qdate_monthly = as.Date(paste(
         format(qdate_min, "%Y"),
         as.numeric(format(qdate_min, "%m")) - 2,
         "01", sep = "-"))
      arriv_trim = filter(dat_arriv, TimePeriod > qdate_monthly, TimePeriod <= qdate_max)
      ## Next create a "marker" that is used to convert the monthly dates into quarterly
      qdate_marker = rep(dat_spend$TimePeriod, each = 3)
      if(length(qdate_marker) > nrow(arriv_trim))
         qdate_marker = qdate_marker[1:nrow(arriv_trim)]
      arriv_quarterly = arriv_trim %>%
         mutate(TimePeriod = qdate_marker) %>%
         group_by(TimePeriod) %>%
         summarise(Arrivals = sum(Arrivals))
      
      ## Merge data
      dat_merge = merge(arriv_quarterly, dat_spend)
      varNames = c("Arrivals", "Total Spend")
      curdat = list(date = dat_merge$TimePeriod, num = dat_merge[varNames])
      
      ## Convert to index
      baserow = which(curdat$date == input$oivasBase)
      curdat$num = data.frame(rebaseIndex(curdat$num, baserow, 4), check.names = FALSE)
      
      ## Draw plot
      ## Get colours and strip transparency
      pal = substr(tourism.cols(c(3, 8)), 0, 7)
      names(pal) = varNames
      #curtitle = paste(input$oivsvariable, curtitlestr, "(Quarterly)")
      curylab = obsrollylab(input$oivasobsroll, "Index")
      curtitlestr = switch(input$oivastype,
         country = paste("for visitors from", oivasdimension),
         pov = paste("for", tolower(oivasdimension))
      )
      curtitle = paste("Arrivals vs total spend", curtitlestr)
      output$oivasdy = dyquick(curdat, input$oivasobsroll, 4, "number",
                               pal, curylab, curtitle, "oivasdyLegend",
                               dateWindow = isolate(input$oivasdy_date_window))
   })
})

rs_all = function(env_serv) with(env_serv, {
   allPeriods = rs_data$allPeriods
   CurrentYear = rs_data$CurrentYear
   
   #############
   ## Summary ##
   #############
   ## Establish a reactive list
   rsvals = reactiveValues()
   
   observe({
      rsdata = list(
         rti_dom = rs_rti_subset(rs_data$RTI, input$rsAreaClass, "Domestic"),
         rti_int = rs_rti_subset(rs_data$RTI, input$rsAreaClass, "International"),
         rte_dom = rs_rte_subset(rs_data$RTE, input$rsAreaClass, "Domestic", CurrentYear),
         rte_int = rs_rte_subset(rs_data$RTE, input$rsAreaClass, "International", CurrentYear)
      )
      rsdata$rti_domNZ = (rsdata$rti_dom %>% group_by(Period) %>% summarise(NZ = sum(Spend)))[,2]
      rsdata$rti_intNZ = (rsdata$rti_int %>% group_by(Period) %>% summarise(NZ = sum(Spend)))[,2]
      
      ## By Origin
      rsdata$rte_dom_origin = rs_rte_by(rsdata$rte_dom, "Origin")
      rsdata$rte_int_origin = rs_rte_by(rsdata$rte_int, "Origin")
      
      ## By Product
      rsdata$rte_dom_product = rs_rte_by(rsdata$rte_dom, "Product")
      rsdata$rte_int_product = rs_rte_by(rsdata$rte_int, "Product")
      
      rsvals$data = rsdata
   })
   
   observe({
      rsvals$rsAreaSub = condInputServer(input, "rsAreaSub", rs_data$areaclass, cond_simple = "rsAreaClass")
      rsvals$baserow = which(allPeriods == input$rstsBase)
   })
   
   ## ROW 1
   output$rstsint = renderDygraph({
      ts_prep = rs_ts_prep(rsvals$data, rsvals$rsAreaSub, "int", CurrentYear)
      rs_ts(ts_prep$data, rsvals$rsAreaSub, rsvals$baserow, ts_prep$title, labelsDiv = "rstsintlabels")
   })
   output$rstsdom = renderDygraph({
      ts_prep = rs_ts_prep(rsvals$data, rsvals$rsAreaSub, "dom", CurrentYear)
      rs_ts(ts_prep$data, rsvals$rsAreaSub, rsvals$baserow, ts_prep$title, labelsDiv = "rstsdomlabels")
   })
   
   ## ROW 2
   observe({
      ## Title
      output$rsoriginintTitle = ggvisTitle("Proportion of international tourist spend in ", rsvals$rsAreaSub, 
            " compared to all New Zealand (for the year ending March ", CurrentYear, ")")
      ## Plot
      rs_rte_subarea(rsvals$data$rte_int_origin, rsvals$rsAreaSub) %>%
         rs_origin(rsvals$rsAreaSub, "International", CurrentYear) %>%
         bind_shiny("rsoriginint")
   })
   observe({
      ## Title
      output$rsorigindomTitle = ggvisTitle("Proportion of domestic tourist spend in ", rsvals$rsAreaSub, 
            " compared to all New Zealand (for the year ending March ", CurrentYear, ")")
      ## Plot
      rs_rte_subarea(rsvals$data$rte_dom_origin, rsvals$rsAreaSub) %>%
         rs_origin(rsvals$rsAreaSub, "Domestic", CurrentYear) %>%
         bind_shiny("rsorigindom")
   })
   
   ## ROW 2
   observe({
      ## Title
      output$rsproductintTitle = ggvisTitle("Proportion of international tourist spend in ", rsvals$rsAreaSub, 
            " compared to all New Zealand (for the year ending March ", CurrentYear, ")")
      ## Plot
      rs_rte_subarea(rsvals$data$rte_int_product, rsvals$rsAreaSub) %>%
         rs_product("International") %>%
         bind_shiny("rsproductint")
   })
   observe({
      ## Title
      output$rsproductdomTitle = ggvisTitle("Proportion of domestic tourist spend in ", rsvals$rsAreaSub, 
            " compared to all New Zealand (for the year ending March ", CurrentYear, ")")
      ## Plot
      rs_rte_subarea(rsvals$data$rte_dom_product, rsvals$rsAreaSub) %>%
         rs_product("Domestic") %>%
         bind_shiny("rsproductdom")
   })
   
   #####################
   ## Download as PDF ##
   #####################
   observe({
      pdf_make = function(file){
         pdf_dat = list(
            area = rsvals$rsAreaSub,
            CurrentYear = CurrentYear,
            mbie_footer = mbie_footer,
            ts_int = {
               ts_prep = rs_ts_prep(rsvals$data, rsvals$rsAreaSub, "int", CurrentYear)
               rs_ts_ggplot(ts_prep$data, rsvals$rsAreaSub, rsvals$baserow, ts_prep$title, TRUE)
            },
            ts_dom = {
               ts_prep = rs_ts_prep(rsvals$data, rsvals$rsAreaSub, "dom", CurrentYear)
               rs_ts_ggplot(ts_prep$data, rsvals$rsAreaSub, rsvals$baserow, ts_prep$title, FALSE)
            },
            dc_origin_int = {
               rs_rte_subarea(rsvals$data$rte_int_origin, rsvals$rsAreaSub) %>%
                  rs_dc_ggplot("Origin", rsvals$rsAreaSub, "International")
            },
            dc_origin_dom = {
               rs_rte_subarea(rsvals$data$rte_dom_origin, rsvals$rsAreaSub) %>%
                  rs_dc_ggplot("Origin", rsvals$rsAreaSub, "Domestic")
            },
            dc_product_int = {
               rs_rte_subarea(rsvals$data$rte_int_product, rsvals$rsAreaSub) %>%
                  rs_dc_ggplot("Product", rsvals$rsAreaSub, "International")
            },
            dc_product_dom = {
               rs_rte_subarea(rsvals$data$rte_dom_product, rsvals$rsAreaSub) %>%
                  rs_dc_ggplot("Product", rsvals$rsAreaSub, "Domestic")
            }
         )
         rs_pdf(file, pdf_dat)
      }
      pdf_name = function(){
         paste0(input$rsAreaClass, "-", rsvals$rsAreaSub, ".pdf")
      }
      output$rspdf = downloadHandler(
         filename = pdf_name,
         content = pdf_make,
         contentType = "application/pdf"
      )
   })
})

racc = function(env_serv) with(env_serv, {
   ## Regions - Acccommodation
   observe({
      curdomint = input$raccdomint
      curorigin = condInputServer(input, "raccorigin", NightsOriginscats, cond_simple = "raccdomint")
      # raccareaType = input$raccareaType
      raccareaType = "RC"
      curareatype = names(NightsAreaType)[NightsAreaType == raccareaType]
      # curarea = condInputServer(input, "raccarea", NightsAreaType, cond_simple = "raccareaType")
      curarea = input$raccareaRC
      if(length(curorigin) > 0){
         flist = list(Origin = curorigin, AreaType = curareatype, Area = curarea)
         dat_spread = getdat(nights_origin[[curdomint]], "Date", "Origin", "Value", flist) %>%
            mutate(Value = Value/1000) %>%
            spread(Origin, Value)
         
         ## Some origins may not exist, so exclude those
         originreal = curorigin[curorigin %in% names(dat_spread)]
         if(length(originreal) > 0){
            ## Draw plot
            # pal = paldy(curarea, NightsAreaList[[raccareaType]])
            pal = paldy(originreal, NightsOrigins[[curdomint]])
            curtitle = paste("Visitor nights from", liststr(originreal), "for", curarea,
               paste0("(year ended ", format(as.POSIXlt(dat_spread$Date[1]), "%B"), ")"))
            curdat = list(date = dat_spread$Date,
                          num = dat_spread[originreal])
            
            if(input$raccprop == "Proportions"){
               if(length(curdat$num) > 1)
                  curdat$num = as.data.frame(t(apply(as.matrix(curdat$num), 1,
                     function(x) x/sum(x, na.rm = TRUE)*100)))
               else
                  curdat$num = curdat$num * 0 + 100
               curylab = "Percentage share of visitor nights"
               curtype = "percent"
            } else{
               curylab = "Visitor nights (000s)"
               curtype = "number"
            }
            
            output$raccdy = dyquick(curdat, "Obs", 1, curtype,
                                    pal, curylab, curtitle, "raccdyLegend",
                                    stacked = TRUE,
                                    dateWindow = isolate(input$raccdy_date_window))
            
            ## Handle drawing and download of Data Table
            tabdat = cbind(Date = curdat$date, curdat$num)
            output$raccTable = renderDataTable(tabdat,
               options = list(order = list(c("0", "desc"))))
            output$raccTableDown = tableComboDown(tabdat, curtitle)
         }
      }
   })
   
   checkboxComboServer(input, session, "raccorigindom", NightsOrigins$dom)
   checkboxComboServer(input, session, "raccoriginint", NightsOrigins$int)
   # checkboxComboServer(input, session, "raccareaRC", NightsAreaList$RC)
   # checkboxComboServer(input, session, "raccareaTA", NightsAreaList$TA)
   
   ## CAM data
   observe({
      curdomint = input$raccdomint
      curorigin = switch(curdomint,
         dom = "domestic",
         int = "international"
      )
      curarea = input$racccamarea
      if(length(curarea) > 0){
         dat_spread = nights_cam[[curdomint]] %>%
            filter(Region %in% curarea) %>%
            spread(Region, Value)
         
         ## Draw plot
         pal = paldy(curarea, NightsCAMArea)
         curylab = "Commercial visitor nights (000s)"
         curtitle = paste("Commercial visitor nights from all", curorigin, "visitors",
            "for", liststr(curarea))
         curdat = list(date = dat_spread$TimePeriod,
                       num = dat_spread[curarea])
         if(input$racccamsum == "12-month rolling sum"){
            curdat$num = data.frame(apply(curdat$num, 2, function(x) rollsum(x, 12, fill = NA, align = "right")), check.names = FALSE)
            curtitle = paste(curtitle, "(12-month rolling sum)")
         } else{
            curtitle = paste(curtitle, "(Monthly)")
         }
         output$racccamdy = dyquick(curdat, "Obs", 12, "number",
                                    pal, curylab, curtitle, "racccamdyLegend",
                                    dateWindow = isolate(input$racccamdy_date_window))
            
         ## Handle drawing and download of Data Table
         tabdat = cbind(Date = curdat$date, curdat$num)
         output$racccamTable = renderDataTable(tabdat,
            options = list(order = list(c("0", "desc"))))
         output$racccamTableDown = tableComboDown(tabdat, curtitle)
      }
   })
   
   checkboxComboServer(input, session, "racccamarea", NightsCAMArea)
})

rins = function(env_serv) with(env_serv, {
   all_levels = env_rte$all_rte_levels
   
   observe({
      rinsOrigin = switch(input$rinsdomint,
         All = all_levels$Origin[1],
         Domestic = input$rinsOriginDomestic,
         International = input$rinsOriginInternational
      )
      flist = list(YEMar = input$rinsYear, Origin = rinsOrigin, Product = all_levels$Product[-1])
      rinsRTR = input$rinsRTR
      flist[[rinsRTR]] = all_levels[[rinsRTR]][-1]
      
      raw_rte = getdat(env_rte$all_rte, "Product", rinsRTR, "Spend", flist)
      out_rte = rename_(raw_rte, Area = rinsRTR)
      
      scaled = input$rinsScaled == "sv"
      plist = list(x = "Area", y = "Spend", fill = "Product")
      titles = list(x = "", fill = "Product")
      titles$y = if(scaled) "Percentage of spend" else "Visitor spend ($ millions)"
      ttips = c(prefix = "$", suffix = "m")
      ggshiny = if(scaled) "rinsggsv" else "rinsggdv"
      ggstackbar(out_rte, plist, titles, ttips, scaled) %>% bind_shiny(ggshiny)
      
      ## Title
      output$rinsggTitle = ggvisTitle("Spend by product for ",
         gsub("_", "", rinsRTR, fixed = TRUE), "s from ", rinsOrigin,
         " (year ended March ", input$rinsYear, ")")
      
      ## Handle drawing and download of Data Table
      tabdat = spread_(out_rte, "Product", "Spend")
      output$rinsTable = renderDataTable(tabdat)
      output$rinsTableDown = tableComboDown(tabdat, "IndustrySectors")
   })
})

rgdp = function(env_serv) with(env_serv, {
   ## Regions - Contribution to Regional GDP
   observe({
      rgdpArea = condInputServer(input, "rgdpArea", GDP_AreaType, cond_simple = "rgdpAreaType")
      if(length(rgdpArea) > 0){
         dat_spread = GDP_tourism %>%
            filter(AreaType %in% c(input$rgdpAreaType, "Total"),
                   Area %in% rgdpArea) %>%
            select(-AreaType) %>%
            spread(Area, Value)
         ## Draw plot
         pal = paldy(rgdpArea, GDP_AreaList[[input$rgdpAreaType]])
         curylab = "% of GDP"
         curliststr = liststr(rgdpArea)
         curtitle = paste("International tourism expenditure as a share of regional GDP for",
            curliststr, "(year ended March)")
         curdat = list(date = dat_spread$Date, num = dat_spread[rgdpArea])
         output$rgdpdy = dyquick(curdat, "Obs", 1, "percent-dp2",
                                 pal, curylab, curtitle, "rgdpdyLegend",
                                 dateWindow = isolate(input$rgdpdy_date_window))
   
         ## Handle drawing and download of Data Table
         tabdat = cbind(Date = curdat$date, curdat$num)
         output$rgdpTable = renderDataTable(tabdat,
            options = list(order = list(c("0", "desc"))))
         output$rgdpTableDown = tableComboDown(tabdat, curtitle)
      }
   })
   
   ## checkboxCombo
   condComboServer(input, session, "rgdpArea",
                   cats = GDP_AreaType,
                   choices = GDP_AreaList)
})

iacc = function(env_serv) with(env_serv, {
   ## Industry - Acccommodation
   observe({
      curRTO = input$iaccRTO
      if(length(curRTO) > 0){
         typesstr = input$iacctype
         if(typesstr == "Total") typesstr = "All Types of Commercial Accommodation"
         classstr = names(AccommClass)[which(AccommClass == input$iaccclass)]
         dat_filter = Accomm_RTO %>%
            filter(RTO %in% curRTO,
                   Accommodation_Type == input$iacctype,
                   Class == input$iaccclass)
         ## Spread
         dat_spread = spread(dat_filter, RTO, Value)
         ## Draw plot
         pal = paldy(curRTO, AccommRTOs)
         curtype = AccommClassType[classstr]
         ## Guest Nights and Capacity shown in 000s
         # if(classstr == "Number of guest nights" || classstr == "Capacity (stay unit nights)"){
            # dat_filter = mutate(dat_filter, Value = Value/10^3)
            # classstr = paste(classstr, "(000s)")
         # }
         curylab = obsrollylab(input$iaccobsroll, tolower(classstr))
         curtitle = paste(classstr, "for", tolower(typesstr), "in", liststr(curRTO), "(Monthly)")
         curdat = list(date = dat_spread$TimePeriod, num = dat_spread[curRTO])
         output$iaccdy = dyquick(curdat, input$iaccobsroll, 12, curtype,
                                 pal, curylab, curtitle, "iaccdyLegend",
                                 dateWindow = isolate(input$iaccdy_date_window))
      
         ## Handle drawing and download of Data Table
         tabdat = cbind(Date = curdat$date, curdat$num)
         output$iaccTable = renderDataTable(tabdat,
            options = list(order = list(c("0", "desc"))))
         output$iaccTableDown = tableComboDown(tabdat, curtitle)
      }
   })
   
   ## checkboxCombo
   checkboxComboServer(input, session, "iaccRTO", AccommRTOs)
})

iact = function(env_serv) with(env_serv, {
   ## Industry - Attractions and Activities
   ## Establish a reactive list
   iactvals = reactiveValues()
   observe({
      curCountry = input$iactcountry
      ## Prepare data
      if(length(curCountry) > 0){
         iactvals$curCountry = curCountry
         ## Time-Series Data
         iactvals$dat_filter = ivs_act %>%
            filter(Country %in% curCountry,
                   grepl(paste(paste0("(", input$iacttype, ")"), collapse = "|"), ActivityConcat)) %>%
            group_by(Date, Country) %>%
            summarise(
               TotalSpend = sum(TotalSpend),
               Pop = sum(Pop),
               PopNights = sum(PopNights)
            ) %>%
            mutate(
               AverageNights = PopNights/Pop,
               SpendPerNight = TotalSpend/PopNights,
               SpendPerTrip = TotalSpend/Pop,
               TotalSpend = TotalSpend/10^6
            )
      }
   })
   
   # curnumtype = switch(curVar,
      # "AverageNights" = "number",
      # "SpendPerNight" = "dollar",
      # "SpendPerTrip" = "dollar",
      # "TotalSpend" = "dollar-m"
   # )
   
   ## Event line to note IVS revision 2013
   ivs_rev = list(list(date = "2013-08-01", label = "IVS revision 2013", labelLoc = "top"))
   
   ## ROW 1
   ## Nights Stayed
   observe({
      curCountry = isolate(iactvals$curCountry)
      dat_spread = iactvals$dat_filter %>%
         select(Date, Country, Value = AverageNights) %>%
         spread(Country, Value)
      ## Draw plot
      pal = paldy(curCountry, all_countries)
      curylab = obsrollylab(input$iactobsroll, "nights stayed")
      curtitle = paste("Average nights stayed by visitors from",
                       liststr(curCountry),
                       "participating in", tolower(input$iacttype),
                       "(Quarterly)")
      curdat = list(date = dat_spread$Date, num = dat_spread[curCountry])
      output$iactnights = dyquick(curdat, input$iactobsroll, 4, "number-dp1",
                                  pal, curylab, curtitle,
                                  "iactlabelsR1", FALSE,
                                  events = ivs_rev,
                                  dateWindow = isolate(input$iactnights_date_window))
   })
   ## Spend per Night
   observe({
      curCountry = isolate(iactvals$curCountry)
      dat_spread = iactvals$dat_filter %>%
         select(Date, Country, Value = SpendPerNight) %>%
         spread(Country, Value)
      ## Draw plot
      pal = paldy(curCountry, all_countries)
      curylab = obsrollylab(input$iactobsroll, "spend per night")
      curtitle = paste("Average spend per night by visitors from",
                       liststr(curCountry),
                       "participating in", tolower(input$iacttype),
                       "(Quarterly)")
      curdat = list(date = dat_spread$Date, num = dat_spread[curCountry])
      output$iactspendnights = dyquick(curdat, input$iactobsroll, 4, "dollar",
                                       pal, curylab, curtitle,
                                       "iactlabelsR1", FALSE,
                                       events = ivs_rev,
                                       dateWindow = isolate(input$iactspendnights_date_window))
   })
   ## ROW 2
   ## Spend per Trip
   observe({
      curCountry = isolate(iactvals$curCountry)
      dat_spread = iactvals$dat_filter %>%
         select(Date, Country, Value = SpendPerTrip) %>%
         spread(Country, Value)
      ## Draw plot
      pal = paldy(curCountry, all_countries)
      curylab = obsrollylab(input$iactobsroll, "spend per trip")
      curtitle = paste("Average spend per trip by visitors from",
                       liststr(curCountry),
                       "participating in", tolower(input$iacttype),
                       "(Quarterly)")
      curdat = list(date = dat_spread$Date, num = dat_spread[curCountry])
      output$iactspendtrip = dyquick(curdat, input$iactobsroll, 4, "dollar",
                                     pal, curylab, curtitle,
                                     "iactlabelsR2", FALSE,
                                     events = ivs_rev,
                                     dateWindow = isolate(input$iactspendtrip_date_window))
   })
   ## Total Spend
   observe({
      curCountry = isolate(iactvals$curCountry)
      dat_spread = iactvals$dat_filter %>%
         select(Date, Country, Value = TotalSpend) %>%
         spread(Country, Value)
      ## Draw plot
      pal = paldy(curCountry, all_countries)
      curylab = obsrollylab(input$iactobsroll, "total spend")
      curtitle = paste("Total spend by visitors from",
                       liststr(curCountry),
                       "participating in", tolower(input$iacttype),
                       "(Quarterly)")
      curdat = list(date = dat_spread$Date, num = dat_spread[curCountry])
      output$iacttotalspend = dyquick(curdat, input$iactobsroll, 4, "dollar-m",
                                     pal, curylab, curtitle,
                                     "iactlabelsR2", FALSE,
                                     events = ivs_rev,
                                     dateWindow = isolate(input$iacttotalspend_date_window))
   })
   ## ROW 3
   
   ## checkboxCombo
   checkboxComboServer(input, session, "iactcountry", all_countries)
})

ibdem = function(env_serv) with(env_serv, {
   ## Industry - Business Demography
   observe({
      ## Use selected dataset
      ibdemData = input$ibdemData
      curdatsource = switch(ibdemData,
         "countsize" = bdem_countsize,
         "birthdeath" = bdem_birthdeath,
         "btype" = bdem_btype,
         "overseas" = bdem_overseas
      )
      curcatname = switch(ibdemData,
         "countsize" = "Size",
         "birthdeath" = "Type",
         "btype" = "BusinessType",
         "overseas" = "OverseasEquity"
      )
      ibdemUnit = input$ibdemUnit
      ibdemCat = condInputServer(input, "ibdemCat", bdem_Data, cond_simple = "ibdemData")
      ibdemIndustry = input$ibdemIndustry
      if(length(ibdemIndustry) > 0){
         flist = list(Unit = ibdemUnit, Industry = ibdemIndustry)
         flist[[curcatname]] = ibdemCat
         dat_get = getdat(curdatsource, "TimePeriod", "Industry", "Value", flist)
         dat_spread = spread(dat_get, Industry, Value)
         
         ## Do some work to create a nice title
         strTitleUnit = if(ibdemData == "birthdeath"){
            if(ibdemCat == "Births Per Death"){
               switch(ibdemUnit,
                  "Enterprises" = "Enterprise births per death",
                  "Employee Count" = "Employees created from enterprise births per job lost to deaths"
               )
            } else if(ibdemCat == "Birth"){
               paste0(
                  switch(ibdemUnit,
                     "Enterprises" = "E",
                     "Employee Count" = "Employees created from e"
                  ), "nterprise births"
               )
            } else if(ibdemCat == "Death"){
               paste0(
                  switch(ibdemUnit,
                     "Enterprises" = "E",
                     "Employee Count" = "Employees lost to e"
                  ), "nterprise deaths"
               )
            } else "?"
         } else{
            strUnit = paste0(
               switch(ibdemUnit,
                  "Enterprises" = "E",
                  "Employee Count" = "Employees in e"
               ), "nterprises"
            )
            strCat = if(grepl("^Total", ibdemCat)){
               NULL
            } else if(ibdemData == "countsize"){
               paste("with", tolower(ibdemCat), "employees")
            } else if(ibdemData == "btype"){
               paste("of type", tolower(ibdemCat))
            } else if(ibdemData == "overseas"){
               paste("with", tolower(ibdemCat), "overseas equity")
            } else "?"
            paste(c(strUnit, strCat), collapse = " ")
         }
         
         ## Draw plot
         pal = paldy(ibdemIndustry, bdem_IndustryLevels)
         curylab = if(ibdemData == "birthdeath" && ibdemCat == "Births Per Death") switch(ibdemUnit,
            "Enterprises" = "Ratio of enterprise births to deaths",
            "Employee Count" = "Ratio of employee births to deaths"
         )
         else switch(ibdemUnit,
            "Enterprises" = "Number of enterprises",
            "Employee Count" = "Number of employees"
         )
         curliststr = tolower(liststr(ibdemIndustry))
         curtitle = paste(strTitleUnit, "for", curliststr, "(year ended December)")
         curdat = list(date = dat_spread$TimePeriod, num = dat_spread[ibdemIndustry])
         curtype = if(ibdemData == "birthdeath" && ibdemCat == "Births Per Death") "number-dp2" else "number"
         output$ibdemdy = dyquick(curdat, "Obs", 1, curtype,
                                  pal, curylab, curtitle, "ibdemdyLegend",
                                  dateWindow = isolate(input$ibdemdy_date_window))
   
         ## Handle drawing and download of Data Table
         output$ibdemTable = renderDataTable(dat_spread,
            options = list(order = list(c("0", "desc"))))
         output$ibdemTableDown = tableComboDown(dat_spread, curtitle)
      }
   })
   
   ## checkboxCombo
   checkboxComboServer(input, session, "ibdemIndustry", bdem_IndustryLevels)
})

icas = function(env_serv) with(env_serv, {
   ## Industry - Business Events
   ## ROW 1
   output$icasCountEvents = renderDygraph({
      casdat("Count of events") %>%
         cas_dyquick(input$icasobsroll, "Count of all events", "icasRowLabels1")
   })
   output$icasCountEventsByType = renderDygraph({
      casdat("Counts of event types", percentage = TRUE) %>%
         cas_dyquick(input$icasobsroll, "Event type", "icasRowLabels1", stackedperc = TRUE)
   })
   ## ROW 2
   output$icasCountSingleMulti = renderDygraph({
      casdat("Count of single v multi day events", percentage = TRUE) %>%
         cas_dyquick(input$icasobsroll, "Event length", "icasRowLabels2", stackedperc = TRUE)
   })
   output$icasCountEventSize = renderDygraph({
      subsetpars = list(grep = "All events", gsubpattern = "All events with ", gsubreplace = "")
      casdat("Event size for different event types",
             subsetpars = subsetpars, percentage = TRUE) %>%
         cas_dyquick(input$icasobsroll, "Event size", "icasRowLabels2", stackedperc = TRUE)
   })
   ## ROW 3
   output$icasDelegateDays = renderDygraph({
      casdat("Delegate days", scaling = 1/1000) %>%
         cas_dyquick(input$icasobsroll, "Total delegate days (000s of days)", "icasRowLabels3")
   })
   output$icasDelegateDaysByType = renderDygraph({
      casdat("Delegate days by event type", percentage = TRUE) %>%
         cas_dyquick(input$icasobsroll, "Delegate days by type", "icasRowLabels3", stackedperc = TRUE)
   })
   ## ROW 4
   output$icasDelegateCapacity = renderDygraph({
      casdat("Delegate capacity") %>%
         cas_dyquick(input$icasobsroll, "Delegate capacity", "icasRowLabels4")
   })
   output$icasTotalDelegates = renderDygraph({
      casdat("Total delegates by origin",
             subsetpars = "Total delegates", scaling = 1/1000) %>%
         cas_dyquick(input$icasobsroll, "Total delegates (000s)", "icasRowLabels4")
   })
   ## ROW 5
   output$icasCustomerType = renderDygraph({
      casdat("Customer type", percentage = TRUE) %>%
         cas_dyquick(input$icasobsroll, "Customer type", "icasRowLabels5", stackedperc = TRUE)
   })
   output$icasDelegatesByOrigin = renderDygraph({
      casdat("Total delegates by origin",
             subsetpars = "(Domestic)|(International)", percentage = TRUE) %>%
         cas_dyquick(input$icasobsroll, "Delegate by origin", "icasRowLabels5", stackedperc = TRUE)
   })
})

isvm = function(env_serv) with(env_serv, {
   all_levels = env_rte$all_rte_levels
   
   observe({
      isvmDest = condInputServer(input, "isvmDest", env_rte$compcatRTR, cond_simple = "isvmRTR")
      isvmOrigin = switch(input$isvmdomint,
         Domestic = levelsOrigin$Domestic[-1],
         International = levelsOrigin$International[-1]
      )
      flist = list(YEMar = input$isvmYear, Origin = isvmOrigin, Product = all_levels$Product[-1])
      isvmRTR = input$isvmRTR
      flist[[isvmRTR]] = isvmDest
      
      out_rte = getdat(env_rte$all_rte, "Product", "Origin", "Spend", flist)
      
      scaled = input$isvmScaled == "sv"
      plist = list(x = "Origin", y = "Spend", fill = "Product")
      titles = list(x = "", fill = "Product")
      titles$y = if(scaled) "Percentage of spend" else "Visitor spend ($ millions)"
      ttips = c(prefix = "$", suffix = "m")
      ggshiny = if(scaled) "isvmggsv" else "isvmggdv"
      ggstackbar(out_rte, plist, titles, ttips, scaled) %>% bind_shiny(ggshiny)
      
      ## Title
      output$isvmggTitle = ggvisTitle("Spend by product from visitors of ",
         tolower(input$isvmdomint), " origin for ", isvmDest,
         " (year ended March ", input$isvmYear, ")")
   })
})

vmorigin = function(env_serv) with(env_serv, {
   all_levels = env_rte$all_rte_levels
   observe({
      vmOriginOne = condInputServer(input, "vmOriginOne", env_rte$compcatRTR, cond_simple = "vmOriginclass")
      vmOriginList = switch(input$vmOrigindomint,
         All = all_levels$Origin[1],
         Domestic = input$vmOriginListDomestic,
         International = input$vmOriginListInternational
      )
      vmOriginScale = input$vmOriginScale
      ## If scaled values, remove Totals from vmOriginList
      if(vmOriginScale == "sv" && length(grep("^Total", vmOriginList)) > 0){
         if(length(vmOriginList) == 1)
            vmOriginList = switch(input$vmOrigindomint,
               All = c(levelsOrigin$Domestic[1], levelsOrigin$International[1]),
               Domestic = levelsOrigin$Domestic[-1],
               International = levelsOrigin$International[-1]
            )
         else
            vmOriginList = vmOriginList[-grep("^Total", vmOriginList)]
      }
      
      if(length(vmOriginList) > 0){
         flist = list(Product = input$vmOriginProduct,
                      Origin = vmOriginList)
         flist[[input$vmOriginclass]] = vmOriginOne
         raw_rte = getdat(env_rte$all_rte, "YEMar", "Origin", "Spend", flist)
         pal = as.vector(paldy(levels(raw_rte$Origin), all_levels$Origin))
         spread_rte = spread_(raw_rte, colnames(raw_rte)[2], colnames(raw_rte)[3])
         
         if(vmOriginScale == "sv"){
            ## Scale data
            flistTot = flist
            flistTot[[input$vmOriginclass]] = all_levels[[input$vmOriginclass]][1]
            tot_rte = getdat(env_rte$all_rte, "YEMar", input$vmOriginclass, "Spend", flistTot)
            spread_rte = cbind(spread_rte[1],
               apply(spread_rte[-1], 2, function(x) x/tot_rte$Spend * 100))
         }
         
         curtitle = paste("Visitor spend in", input$vmOriginProduct,
                          "in", vmOriginOne,
                          "from", liststr(vmOriginList),
                          "(year ended March)")
         output$vmOriginPlot = rte_dyquick(spread_rte, pal, curtitle, "vmOriginLegend",
                                           stacked = vmOriginScale == "sv",
                                           dateWindow = isolate(input$vmOriginPlot_date_window))
      
         ## Handle drawing and download of Data Table
         if(vmOriginScale == "sv"){
            ## Round if percentages
            spread_rte_table = cbind(spread_rte[1],
               apply(spread_rte[-1], 2, function(x) round(x, 2)))
         } else{
            spread_rte_table = spread_rte
         }
         output$vmOriginTable = renderDataTable(spread_rte,
            options = list(order = list(c("0", "desc"))))
         output$vmOriginTableDown = tableComboDown(spread_rte, curtitle)
      }
   })
   
   ## checkboxCombo
   levelsOrigin = env_rte$levelsOrigin
   catOrigin = env_rte$catOrigin
   condComboServer(input, session, "vmOriginList",
                   cats = catOrigin[-1],
                   choices = levelsOrigin)
})
vmdest = function(env_serv) with(env_serv, {
   compcatRTR = env_rte$compcatRTR
   all_levels = env_rte$all_rte_levels
   
   observe({
      vmDestList = condInputServer(input, "vmDestList", compcatRTR, cond_simple = "vmDestclass")
      vmDestOrigin = switch(input$vmDestdomint,
         All = all_levels$Origin[1],
         Domestic = input$vmDestOriginDomestic,
         International = input$vmDestOriginInternational
      )
      vmDestScale = input$vmDestScale
      ## If scaled values, remove Totals from vmDestList
      if(vmDestScale == "sv" && length(grep("^Total", vmDestList)) > 0){
         if(length(vmDestList) == 1)
            vmDestList = all_levels[[input$vmDestclass]][-1]
         else
            vmDestList = vmDestList[-grep("^Total", vmDestList)]
      }
      
      if(length(vmDestList) > 0){
         flist = list(Origin = vmDestOrigin,
                      Product = input$vmDestProduct)
         flist[[input$vmDestclass]] = vmDestList
         raw_rte = getdat(env_rte$all_rte, "YEMar", input$vmDestclass, "Spend", flist)
         pal = as.vector(paldy(levels(raw_rte[[input$vmDestclass]]), all_levels[[input$vmDestclass]]))
         spread_rte = spread_(raw_rte, colnames(raw_rte)[2], colnames(raw_rte)[3])
         
         if(vmDestScale == "sv"){
            ## Scale data
            flistTot = flist
            flistTot[[input$vmDestclass]] = all_levels[[input$vmDestclass]][1]
            tot_rte = getdat(env_rte$all_rte, "YEMar", input$vmDestclass, "Spend", flistTot)
            spread_rte = cbind(spread_rte[1],
               apply(spread_rte[-1], 2, function(x) x/tot_rte$Spend * 100))
         }
         
         curtitle = paste(vmDestOrigin, "visitor spend in",
                          input$vmDestProduct,
                          "in", liststr(vmDestList),
                          "(year ended March)")
         output$vmDestPlot = rte_dyquick(spread_rte, pal, curtitle, "vmDestLegend",
                                         stacked = vmDestScale == "sv",
                                         dateWindow = isolate(input$vmDestPlot_date_window))
      
         ## Handle drawing and download of Data Table
         if(vmDestScale == "sv"){
            ## Round if percentages
            spread_rte_table = cbind(spread_rte[1],
               apply(spread_rte[-1], 2, function(x) round(x, 2)))
         } else{
            spread_rte_table = spread_rte
         }
         output$vmDestTable = renderDataTable(spread_rte_table,
            options = list(order = list(c("0", "desc"))))
         output$vmDestTableDown = tableComboDown(spread_rte, curtitle)
      }
   })
   
   ## checkboxCombo
   condComboServer(input, session, "vmDestList",
                   cats = compcatRTR,
                   choices = all_levels)
})

vmacc = function(env_serv) with(env_serv, {
   ## Set colour
   pal = hcl(seq(0, 270, length = length(ivs_accomm_types)))
   
   ## Establish a reactive list
   vmaccvals = reactiveValues()
   observe({
      EndDate = as.Date(input$vmaccYEnd)
      StartDate = as.Date(paste0(as.numeric(format(EndDate, "%Y")) - ivs_accomm_years, "-", as.numeric(format(EndDate,"%m")), "-", format(EndDate,"%d")))
      ## Prepare data
      vmaccvals$dat_filter = ivs_accomm_used %>%
         filter(Date > StartDate, Date <= EndDate) %>%
         select(Date, Country, AccommType, Pop) %>%
         mutate(Country = factor(Country, levels = all_countries_no_total)) %>%
         group_by(Country, AccommType) %>%
         summarise(Pop = sum(Pop, na.rm = TRUE))
   })
   
   observe({
      out_rte = vmaccvals$dat_filter %>%
         mutate(Pop = Pop/10^6)
      
      scaled = input$vmaccScaled == "sv"
      plist = list(x = "Country", y = "Pop", fill = "AccommType")
      titles = list(x = "", fill = "Accommodation type")
      titles$y = if(scaled) "Percentage of total visitors" else "Total visitors (millions)"
      ttips = c(prefix = "", suffix = "m")
      ggshiny = if(scaled) "vmaccggsv" else "vmaccggrv"
      ggstackbar(out_rte, plist, titles, ttips, scaled) %>% bind_shiny(ggshiny)
      
      ## Title
      output$vmaccggTitle = ggvisTitle("Visitors by",
         " accommodation type for international visitors",
         " (36 months ended ", input$vmaccYEnd, ")")
      
      ## Handle drawing and download of Data Table
      assign("vmacc_dat", vmaccvals$dat_filter, envir = .GlobalEnv)
      tabdat = vmaccvals$dat_filter %>%
         mutate(Pop = round(signif(Pop, 4))) %>%
         spread_("Country", "Pop")
      tabdat_raw = vmaccvals$dat_filter %>%
         spread_("Country", "Pop")
      output$vmaccTable = renderDataTable(tabdat,
            options = list(order = list(c("0", "asc"))))
      output$vmaccTableDown = tableComboDown(tabdat_raw, "AccommodationUsed_by_Country")
   })
})

gcforex = function(env_serv) with(env_serv, {
   ## Global Context - Exchange Rates
   env_forex = new.env()
   
   observe({
      # input = list(gcforexCurrency = "AUD", gcforexIndex = "2011")
      curfx = input$gcforexCurrency
      IndexDate = paste0(as.numeric(input$gcforexIndex), "-12-31")
      
      ################
      ## Forex data ##
      ################
      local(if(!exists(paste0("NZD", curfx), envir = env_forex)){
         fx_str = paste("NZD", curfx, sep = "/")
         # cat("getFX", fx_str, "\n")
         tryCatch(getFX(fx_str, from = fx_date_start, env = env_forex), error = function(e){
            ## If `getFX` fails for some reason, use fallback
            fx_sub = paste0("NZD", curfx)
            assign(fx_sub, as.list(env_forex_fallback)[[fx_sub]], envir = env_forex)
         })
      })
      curxts = as.list(env_forex)[[paste0("NZD", curfx)]]
      xts_num = 1/as.vector(curxts)
      xts_num = xts_num/xts_num[index(curxts) == IndexDate] * 100
      dat_forex = data.frame(index(curxts), xts_num)
      names(dat_forex) = c("TimePeriod", curfx)
      
      dat_merge = dat_forex
      
      ivs_country = fx_currency_to_ivs[[curfx]]
      for(cur_country in ivs_country){
         flist = list(Type = "Country of residence", Dimension = cur_country)
         ################
         ## Spend data ##
         ################
         flist$Variable = "Total spend"
         dat_spend = getdat(ivs, "TimePeriod", "Dimension", "Value", flist) %>%
            filter(TimePeriod >= fx_date_leadin) %>%
            mutate(Value = rollmean(Value, 4, fill = NA, align = "right")) %>%
            filter(TimePeriod >= fx_date_start, !is.na(Value)) %>%
            mutate(Value = Value/mean(Value[TimePeriod == IndexDate]) * 100) %>%
            spread(Dimension, Value)
         names(dat_spend) = c("TimePeriod", paste0("Total spend (", cur_country, ")"))
         
         ###################
         ## Arrivals data ##
         ###################
         flist$Variable = "Arrivals"
         dat_arriv = getdat(arriv, "TimePeriod", "Dimension", "Value", flist) %>%
            filter(TimePeriod >= fx_date_leadin) %>%
            mutate(Value = rollmean(Value, 12, fill = NA, align = "right")) %>%
            filter(TimePeriod >= fx_date_start, !is.na(Value)) %>%
            mutate(Value = Value/mean(Value[TimePeriod == IndexDate]) * 100) %>%
            spread(Dimension, Value)
         names(dat_arriv) = c("TimePeriod", paste0("Arrivals (", cur_country, ")"))
         
         ################
         ## Merge Data ##
         ################
         dat_merge = merge(dat_merge, merge(dat_spend, dat_arriv, all = TRUE), all = TRUE)
      }
      xts_merge = xts(dat_merge[-1], dat_merge$TimePeriod)
      pal = paldy(names(dat_merge), names(dat_merge))
      
      curtitle = paste("Index of NZD bought with", curfx,
         "vs total spend and arrivals from", liststr(ivs_country))
      output$gcforexdy = dyforex(xts_merge, curtitle, pal, "gcforexdyLegend",
                                 isolate(input$gcforexdy_date_window))
   
      ## Handle drawing and download of Data Table
      tabdat = dat_merge
      output$gcforexTable = renderDataTable(tabdat,
         options = list(order = list(c("0", "desc"))))
      output$gcforexTableDown = tableComboDown(tabdat, "NZD_forex")
   })
})
