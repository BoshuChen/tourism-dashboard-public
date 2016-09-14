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

oeco = function(env_serv) with(env_serv, local({
   ## Overview - Economic Contribution
   pal2 = paldy(unique(tourism_economic$Type), unique(tourism_economic$Type))
   pal1 = c(Domestic = mbie.cols2(4), International = mbie.cols2(3))
   
   ## GDP contribution as %
   local({
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
   })
   
   ## GDP contribution
   local({
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
   })
   
   ## Employment contribution as %
   local({
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
   })
   
   ## Employment contribution
   local({
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
   })
   
   ## Tourism as % of Exports
   local({
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
   })
   
   ## Total Intl Tourism Spend
   local({
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
}))

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

rs_all = function(env_serv) with(env_serv, local({
   ## Establish a reactive list
   rsvals = reactiveValues()
   
   ## Get colours and strip transparency
   col_all = substr(tourism.cols(c(3, 14, 8)), 0, 7)
   col_cur = col_all[1]
   col_curlight = col_all[2]
   col_NZ = col_all[3]
   
   ## Sub-functions
   rs_ts_data = function(Area, domint){
      out = MVmod %>%
         filter(Type == domint) %>%
         group_by_("Date", Area) %>%
         summarise(Spend = sum(Spend)) %>%
         arrange(Date) %>%
         rename_(Area = Area)
      class(out) = "data.frame"
      out
   }
   rs_ts_title = function(subarea, domint){
      domintlong = switch(domint,
         dom = "Domestic",
         int = "International"
      )
      paste0(domintlong, " tourist expenditure in ", subarea)
   }
   rs_ts_prep = function(rsdata, subarea, domint){
      basedat = rsdata[[paste0("ts_", domint)]] %>%
         filter(Area == subarea)
      list(
         data = list(date = basedat$Date, num = basedat["Spend"]),
         title = rs_ts_title(subarea, domint)
      )
   }
   rs_ts_roll = function(numdat, subarea){
      ## Original values for current area
      ## + 12-month rolling average for area
      curnum = cbind(
         numdat[,"Spend"],
         rollmean(numdat[,"Spend"], 12, fill = NA, align = "right")
      )
      colnames(curnum) = c(subarea, paste(subarea, "(12-mth)"))
      curnum
   }
   rs_ts = function(curdat, subarea, main = NULL, ylab = "Spend",
                    labelsDiv = "rstslabels", labelsSeparateLines = FALSE){
      curnum = rs_ts_roll(curdat$num, subarea)
      yformat = paste0("function(number){",
         "return accounting.formatMoney(number, ",
         "{precision: ", max(0, getdp(curnum[,1]) - 2), "}) + 'm';}")
      xts(curnum, curdat$date) %>%
         padxts() %>%
         dygraph(main = main) %>%
         dySeries(subarea, strokePattern = "dotted", color = col_curlight) %>%
         dySeries(paste(subarea, "(12-mth)"), strokeWidth = 3, color = col_cur) %>%
         dyRangeSelector() %>%
         dyLegend(labelsDiv = labelsDiv, labelsSeparateLines = labelsSeparateLines) %>%
         dyCallbacks(drawCallback = dyRegister()) %>%
         dyHighlight(highlightSeriesOpts = list(strokeWidth = 5)) %>%
         dyAxis("y", ylab, valueFormatter = yformat, axisLabelFormatter = yformat) %>%
         dyOptions(rightGap = 25) %>%
         return
   }
   
   rs_prop_by =
   ## Group the RTE subset by Origin or Product and compute proportions
   ## Also compute the proportions for All New Zealand, for comparison
   function(Area, domint, byvar){
      rs_spread = MVYE %>%
         filter(Type == domint, YearEnded == max(MVYEyears)) %>%
         group_by_(byvar, Area) %>%
         summarise(Spend = sum(Spend)) %>%
         spread_(Area, "Spend")
      rs_prop = cbind(
         rs_spread[,1],
         "All New Zealand" = apply(rs_spread[,-1], 1, function(x) sum(x, na.rm = TRUE))/
                                   sum(rs_spread[,-1], na.rm = TRUE),
         apply(rs_spread[,-1], 2, function(x) x/sum(x, na.rm = TRUE))
      )
      rs_by = switch(byvar,
         Origin = gather(rs_prop, Area, Spend, -Origin),
         Product = gather(rs_prop, Area, Spend, -Product)
      )
      
      rs_by
   }
   rs_prop_subarea =
      ## Get only the chosen subarea
      ## rs_prop_by %>% rs_prop_subarea
      function(rs_by, subarea){
         rs_by_filter = filter(rs_by, Area %in% c("All New Zealand", subarea))
         rs_by_filter$Area = factor(rs_by_filter$Area)
         rs_by_filter
      }
   rs_prop_title = function(subarea, domint){
      ggvisTitle(
         "Proportion of ", domint, " tourist spend in ", subarea, 
         " compared to all New Zealand (for the year ended ", MVYE_str_last, ")"
      )
   }
   rs_prop_title_hc = function(subarea, domint){
      paste0(
         "Proportion of ", domint, " tourist spend in ", subarea, 
         " compared to all New Zealand (for the year ended ", MVYE_str_last, ")"
      )
   }
   rs_dc_diff = function(curdat){
      dat_diff = spread(curdat, Area, Spend)
      names(dat_diff) = c("y", "x1", "x2")
      dat_diff$col = col_cur
      dat_diff$col[dat_diff[,2] > dat_diff[,3]] = col_NZ
      dat_diff
   }
   rs_dc_shared = function(vis, dat_diff){
      vis %>%
         layer_rects(x = ~x1, x2 = ~x2, y = ~y, height := 1,
                     stroke := ~col, fill := ~col, opacity := 0.3, data = dat_diff) %>%
         layer_points() %>%
         add_axis("x", title = "Proportion of total spend", format = "%", tick_size_end = 0) %>%
         add_axis("y", title = "", tick_size_end = 0) %>%
         scale_ordinal("y", reverse = TRUE) %>%
         scale_ordinal("stroke", range = c(col_NZ, col_cur)) %>%
         scale_ordinal("fill", range = c(col_NZ, col_cur)) %>%
         hide_legend("stroke") %>%
         #hide_legend("fill") %>%
         add_legend("fill", title = "") %>%
         return
   }
   rs_origin = function(curdat, subarea, domint){
      curdat %>%
         ggvis(x = ~Spend, y = ~Origin, stroke = ~Area, fill = ~Area) %>%
         rs_dc_shared(rs_dc_diff(curdat)) %>%
         add_tooltip(function(input){
            if(is.null(input)) NULL else
            paste0(round(input$Spend * 100, 1), "% of ", domint, " spending come from ",
                   input$Origin, " for ", input$Area)
         }) %>%
         set_options(width = "auto", height = 500, duration = 0, renderer = "canvas") %>%
         return
   }
   rs_product = function(curdat, domint){
      curdat %>%
         ggvis(x = ~Spend, y = ~Product, stroke = ~Area, fill = ~Area) %>%
         rs_dc_shared(rs_dc_diff(curdat)) %>%
         add_tooltip(function(input){
            if(is.null(input)) NULL else
            paste0(round(input$Spend * 100, 1), "% of ", domint, " spending come from ",
                   input$Product, " for ", input$Area)
         }) %>%
         set_options(width = "auto", height = 250, duration = 0, renderer = "canvas") %>%
         return
   }
   rs_product_hc = function(curdat, domint, main){
      ttipformat = paste0('<b>{point.y:.1f}%</b> of ', domint, ' spending come from<br/>',
                          '<em>{point.category}</em><br/>',
                          'for {series.name}<br/>')
      curdat %>%
         mutate(Spend = Spend * 100) %>%
         hcdotchart(list(x = "Spend", y = "Product", comp = "Area"),
                    main = main, xlab = "Proportion of total spend (%)",
                    ttipformat = ttipformat, cols = c(col_cur, col_NZ))
   }
   
   observe({
      rsdata = list(
         ts_dom = rs_ts_data(input$rsAreaClass, "Domestic"),
         ts_int = rs_ts_data(input$rsAreaClass, "International")
      )
      rsdata$ts_domNZ = rsdata$ts_dom %>% group_by(Date) %>% summarise(NZ = sum(Spend))
      rsdata$ts_intNZ = rsdata$ts_int %>% group_by(Date) %>% summarise(NZ = sum(Spend))
      
      ## By Origin
      # rsdata$rte_dom_origin = rs_prop_by(input$rsAreaClass, "Domestic", "Origin")
      rsdata$rte_int_origin = rs_prop_by(input$rsAreaClass, "International", "Origin")
      
      ## By Product
      rsdata$rte_dom_product = rs_prop_by(input$rsAreaClass, "Domestic", "Product")
      rsdata$rte_int_product = rs_prop_by(input$rsAreaClass, "International", "Product")
      
      rsvals$data = rsdata
   })
   
   observe({
      rsvals$rsAreaSub = condInputServer(input, "rsAreaSub", MVareaclass, cond_simple = "rsAreaClass")
   })
   
   ## ROW 1
   output$rstsint = renderDygraph({
      ts_prep = rs_ts_prep(rsvals$data, rsvals$rsAreaSub, "int")
      rs_ts(ts_prep$data, rsvals$rsAreaSub, ts_prep$title, labelsDiv = "rstsintlabels")
   })
   output$rstsdom = renderDygraph({
      ts_prep = rs_ts_prep(rsvals$data, rsvals$rsAreaSub, "dom")
      rs_ts(ts_prep$data, rsvals$rsAreaSub, ts_prep$title, labelsDiv = "rstsdomlabels")
   })
   
   ## ROW 2
   observe({
      domint = "International"
      ## Title
      output$rsoriginintTitle = rs_prop_title(rsvals$rsAreaSub, domint)
      ## Plot
      rs_prop_subarea(rsvals$data$rte_int_origin, rsvals$rsAreaSub) %>%
         rs_origin(rsvals$rsAreaSub, domint) %>%
         bind_shiny("rsoriginint")
   })
   ## Disable domestic origin, since there's currently no data on domestic origins
   if(FALSE){
   observe({
      domint = "Domestic"
      ## Title
      output$rsorigindomTitle = rs_prop_title(rsvals$rsAreaSub, domint)
      ## Plot
      rs_prop_subarea(rsvals$data$rte_dom_origin, rsvals$rsAreaSub) %>%
         rs_origin(rsvals$rsAreaSub, domint) %>%
         bind_shiny("rsorigindom")
   })
   }
   
   ## ROW 2
   observe({
      domint = "International"
      ## Title
      output$rsproductintTitle = rs_prop_title(rsvals$rsAreaSub, domint)
      ## Plot
      rs_prop_subarea(rsvals$data$rte_int_product, rsvals$rsAreaSub) %>%
         rs_product(domint) %>%
         bind_shiny("rsproductint")
   })
   observe({
      domint = "Domestic"
      ## Title
      output$rsproductdomTitle = rs_prop_title(rsvals$rsAreaSub, domint)
      ## Plot
      rs_prop_subarea(rsvals$data$rte_dom_product, rsvals$rsAreaSub) %>%
         rs_product(domint) %>%
         bind_shiny("rsproductdom")
      ## hc test
      # output$rsproductdom = renderHighchart({
         # rs_prop_subarea(rsvals$data$rte_dom_product, rsvals$rsAreaSub) %>%
            # rs_product_hc(domint, rs_prop_title_hc(rsvals$rsAreaSub, domint))
      # }) 
   })
}))

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
   observe({
      domint = input$rinsdomint
      rinsOrigin = switch(domint,
         All = MVoriginsAll[1],
         # Domestic = input$rinsOriginDomestic,
         Domestic = MVoriginslist$Domestic[1],
         International = input$rinsOriginInternational
      )
      flist = list(YearEnded = input$rinsYear, Origin = rinsOrigin, Product = MVproducts[-1])
      rinsRTR = input$rinsRTR
      flist[[rinsRTR]] = MVarealist[[rinsRTR]]
      
      curdat = (if(domint == "All") MVYE else filter(MVYE, Type == domint)) %>%
         getdat("Product", rinsRTR, "Spend", flist) %>%
         rename_(Area = rinsRTR)
      
      ## Order by size
      lsum = curdat %>%
         group_by(Area) %>%
         summarise(Spend = sum(Spend)) %>%
         arrange(desc(Spend))
      lorder = as.character(lsum$Area)
      curdat$Area = factor(curdat$Area, levels = lorder)
      
      ############################
      scaled = input$rinsScaled == "sv"
      plist = list(x = "Area", y = "Spend", fill = "Product")
      titles = list(x = "", fill = "Product")
      titles$y = if(scaled) "Percentage of spend" else "Visitor spend ($ millions)"
      ttips = c(prefix = "$", suffix = "m")
      ggshiny = if(scaled) "rinsggsv" else "rinsggdv"
      ggstackbar(curdat, plist, titles, ttips, scaled) %>% bind_shiny(ggshiny)
      
      ## Title
      output$rinsggTitle = ggvisTitle("Spend by product for ",
         gsub("_", "", rinsRTR, fixed = TRUE), "s from ", rinsOrigin,
         " (year ended ", MVYE_str, " ", input$rinsYear, ")")
      ############################
      
      # scaled = input$rinsScaled == "sv"
      # plist = list(x = "Area", y = "Spend", fill = "Product")
      # titles = list(
         # main = paste0("Spend by product for ",
            # gsub("_", "", rinsRTR, fixed = TRUE), "s from ", rinsOrigin,
            # " (year ended ", MVYE_str, " ", input$rinsYear, ")"),
         # y = if(scaled) "Percentage of spend" else "Visitor spend ($ millions)"
      # )
      # yformat = if(scaled) list(prefix = "", suffix = "%") else list(prefix = "$", suffix = "m")
      # output$rinshc = renderHighchart({
         # hc_stackbar(curdat, plist, titles, yformat, scaled)
      # })
      
      ## Handle drawing and download of Data Table
      tabdat = spread_(curdat, "Product", "Spend")
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
   ivs_rev = list(list("2013-08-01", label = "IVS revision 2013", labelLoc = "top"))
   
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
   observe({
      # domint = input$isvmdomint
      domint = "International"
      isvmDest = condInputServer(input, "isvmDest", MVareaclass, cond_simple = "isvmRTR")
      isvmOrigin = switch(domint,
         Domestic = MVoriginslist$Domestic[-1],
         International = MVoriginslist$International[-1]
      )
      flist = list(YearEnded = input$isvmYear, Origin = isvmOrigin, Product = MVproducts[-1])
      isvmRTR = input$isvmRTR
      flist[[isvmRTR]] = isvmDest
      
      curdat = (if(domint == "All") MVYE else filter(MVYE, Type == domint)) %>%
         getdat("Product", "Origin", "Spend", flist)
      
      ## Order by size
      lsum = curdat %>%
         group_by(Origin) %>%
         summarise(Spend = sum(Spend)) %>%
         arrange(desc(Spend))
      lorder = as.character(lsum$Origin)
      curdat$Origin = factor(curdat$Origin, levels = lorder)
      
      scaled = input$isvmScaled == "sv"
      plist = list(x = "Origin", y = "Spend", fill = "Product")
      titles = list(x = "", fill = "Product")
      titles$y = if(scaled) "Percentage of spend" else "Visitor spend ($ millions)"
      ttips = c(prefix = "$", suffix = "m")
      ggshiny = if(scaled) "isvmggsv" else "isvmggdv"
      ggstackbar(curdat, plist, titles, ttips, scaled) %>% bind_shiny(ggshiny)
      
      ## Title
      output$isvmggTitle = ggvisTitle("Spend by product from visitors of ",
         tolower(domint), " origin for ", isvmDest,
         " (year ended ", MVYE_str, " ", input$isvmYear, ")")
   })
})

vmorigin = function(env_serv) with(env_serv, {
   observe({
      domint = input$vmOrigindomint
      vmOriginOne = condInputServer(input, "vmOriginOne", MVareaclass, cond_simple = "vmOriginclass")
      vmOriginList = switch(domint,
         All = MVoriginsAll[1],
         # Domestic = input$vmOriginListDomestic,
         Domestic = MVoriginslist$Domestic[1],
         International = input$vmOriginListInternational
      )
      vmOriginScale = input$vmOriginScale
      ## If scaled values, remove Totals from vmOriginList
      if(vmOriginScale == "sv" && length(grep("^Total", vmOriginList)) > 0){
         if(length(vmOriginList) == 1)
            vmOriginList = switch(domint,
               All = vmOriginList,
               Domestic = MVoriginslist$Domestic[-1],
               International = MVoriginslist$International[-1]
            )
         else
            vmOriginList = vmOriginList[-grep("^Total", vmOriginList)]
      }
      
      vmdat = function(flist){
         if(domint == "All"){
            if(vmOriginScale == "sv"){
               ## Must compute totals of Domestic and International separately
               flist$Origin = MVoriginslist$Domestic[1]
               dat_dom = filter(MVYE, Type == "Domestic") %>%
                  getdat("YearEnded", "Origin", "Spend", flist)
               flist$Origin = MVoriginslist$International[1]
               dat_int = filter(MVYE, Type == "International") %>%
                  getdat("YearEnded", "Origin", "Spend", flist)
               curdat = rbind(dat_dom, dat_int)
            } else{
               curdat = MVYE %>%
                  getdat("YearEnded", "Origin", "Spend", flist)
            }
         } else{
            curdat = filter(MVYE, Type == domint) %>%
               getdat("YearEnded", "Origin", "Spend", flist)
         }
         curdat
      }
      
      if(length(vmOriginList) > 0){
         flist = list(Product = input$vmOriginProduct,
                      Origin = vmOriginList)
         flist[[input$vmOriginclass]] = vmOriginOne
         curdat = vmdat(flist)
         pal = as.vector(paldy(levels(curdat$Origin), MVoriginsAll))
         spread_dat = spread_(curdat, colnames(curdat)[2], colnames(curdat)[3])
         ## reorder columns
         # spread_dat = cbind(spread_dat[1], spread_dat[vmOriginList])
         
         if(vmOriginScale == "sv"){
            ## Scale data
            flistTot = flist
            flistTot[[input$vmOriginclass]] = MVarealisttots[[input$vmOriginclass]][1]
            tot_dat = getdat(MVYE, "YearEnded", input$vmOriginclass, "Spend", flistTot)
            spread_dat = cbind(spread_dat[1],
               apply(spread_dat[-1], 2, function(x) x/tot_dat$Spend * 100))
         }
         
         curtitle = paste0("Visitor spend in ", input$vmOriginProduct,
                          " in ", vmOriginOne,
                          " from ", liststr(vmOriginList),
                          " (year ended ", MVYE_str, ")")
         output$vmOriginPlot = rte_dyquick(spread_dat, pal, curtitle, "vmOriginLegend",
                                           stacked = vmOriginScale == "sv",
                                           dateWindow = isolate(input$vmOriginPlot_date_window))
      
         ## Handle drawing and download of Data Table
         if(vmOriginScale == "sv"){
            ## Round if percentages
            spread_dat_table = cbind(spread_dat[1],
               apply(spread_dat[-1], 2, function(x) round(x, 2)))
         } else{
            spread_dat_table = spread_dat
         }
         output$vmOriginTable = renderDataTable(spread_dat,
            options = list(order = list(c("0", "desc"))))
         output$vmOriginTableDown = tableComboDown(spread_dat, curtitle)
      }
   })
   
   ## checkboxCombo
   # condComboServer(input, session, "vmOriginList",
                   # cats = MVorigins[-1],
                   # choices = MVoriginslist)
   condComboServer(input, session, "vmOriginList",
                   cats = MVorigins[-c(1, 2)],
                   choices = MVoriginslist)
})
vmdest = function(env_serv) with(env_serv, {
   observe({
      domint = input$vmDestdomint
      vmDestList = condInputServer(input, "vmDestList", MVareaclass, cond_simple = "vmDestclass")
      vmDestOrigin = switch(domint,
         All = MVoriginsAll[1],
         # Domestic = input$vmDestOriginDomestic,
         Domestic = MVoriginslist$Domestic[1],
         International = input$vmDestOriginInternational
      )
      vmDestScale = input$vmDestScale
      ## If scaled values, remove Totals from vmDestList
      if(vmDestScale == "sv" && length(grep("^Total", vmDestList)) > 0){
         if(length(vmDestList) == 1)
            vmDestList = MVarealisttots[[input$vmDestclass]][-1]
         else
            vmDestList = vmDestList[-grep("^Total", vmDestList)]
      }
      
      if(length(vmDestList) > 0){
         flist = list(Origin = vmDestOrigin,
                      Product = input$vmDestProduct)
         flist[[input$vmDestclass]] = vmDestList
         curdat = (if(domint == "All") MVYE else filter(MVYE, Type == domint)) %>%
            getdat("YearEnded", input$vmDestclass, "Spend", flist)
         pal = as.vector(paldy(levels(curdat[[input$vmDestclass]]), MVarealisttots[[input$vmDestclass]]))
         spread_dat = spread_(curdat, colnames(curdat)[2], colnames(curdat)[3])
         
         if(vmDestScale == "sv"){
            ## Scale data
            flistTot = flist
            flistTot[[input$vmDestclass]] = MVarealisttots[[input$vmDestclass]][1]
            tot_dat = (if(domint == "All") MVYE else filter(MVYE, Type == domint)) %>%
               getdat("YearEnded", input$vmDestclass, "Spend", flistTot)
            spread_dat = cbind(spread_dat[1],
               apply(spread_dat[-1], 2, function(x) x/tot_dat$Spend * 100))
         }
         
         curtitle = paste0(vmDestOrigin, " visitor spend in ",
                          input$vmDestProduct,
                          " in ", liststr(vmDestList),
                          " (year ended ", MVYE_str, ")")
         output$vmDestPlot = rte_dyquick(spread_dat, pal, curtitle, "vmDestLegend",
                                         stacked = vmDestScale == "sv",
                                         dateWindow = isolate(input$vmDestPlot_date_window))
      
         ## Handle drawing and download of Data Table
         if(vmDestScale == "sv"){
            ## Round if percentages
            spread_dat_table = cbind(spread_dat[1],
               apply(spread_dat[-1], 2, function(x) round(x, 2)))
         } else{
            spread_dat_table = spread_dat
         }
         output$vmDestTable = renderDataTable(spread_dat_table,
            options = list(order = list(c("0", "desc"))))
         output$vmDestTableDown = tableComboDown(spread_dat, curtitle)
      }
   })
   
   ## checkboxCombo
   condComboServer(input, session, "vmDestList",
                   cats = MVareaclass,
                   choices = MVarealisttots)
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
   ## Always use fallback for efficiency
   # env_forex = new.env()
   env_forex = env_forex_fallback
   
   observe({
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
      
      ######################
      ## Spend & Arrivals ##
      ######################
      ivs_country = fx_currency_to_ivs[[curfx]]
      dat_part = get_spend_arriv_dat(ivs_country, fx_all_years[1], IndexDate)
      
      ################
      ## Merge Data ##
      ################
      dat_merge = merge(dat_forex, dat_part, all = TRUE)
      xts_merge = xts(dat_merge[-1], dat_merge$TimePeriod)
      pal = paldy(names(xts_merge), names(xts_merge))
      
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

ecshare = function(env_serv) with(env_serv, {
   ## Economic Context - Tourism-related Share Prices
   
   ## Prepare Total Spend and Arrivals data
   ecasdat = local({
      flist = list(Type = "Country of residence", Dimension = all_countries[1])
      date_leadin = paste0(as.numeric(shares_all_years[1]) - 1, "-01-01")
      
      ################
      ## Spend data ##
      ################
      flist$Variable = "Total spend"
      dat_spend = getdat(ivs, "TimePeriod", "Dimension", "Value", flist) %>%
         filter(TimePeriod >= date_leadin) %>%
         mutate(Value = rollmean(Value, 4, fill = NA, align = "right")) %>%
         filter(!is.na(Value)) %>%
         select(TimePeriod, Spend = Value)
      
      ###################
      ## Arrivals data ##
      ###################
      flist$Variable = "Arrivals"
      dat_arriv = getdat(arriv, "TimePeriod", "Dimension", "Value", flist) %>%
         filter(TimePeriod >= date_leadin) %>%
         mutate(Value = rollmean(Value, 12, fill = NA, align = "right")) %>%
         filter(!is.na(Value)) %>%
         select(TimePeriod, Arrivals = Value)
      
      ################
      ## Merge Data ##
      ################
      merge(dat_spend, dat_arriv, all = TRUE)
   })
   
   observe({
      if(length(input$ecsharechk) > 0){
         shares_index = apply(shares_adj[,input$ecsharechk], 2, function(x) x/x[index(shares_adj) == input$ecshareIndex] * 100)
         shares_xts = xts(shares_index, index(shares_adj))
         
         eoy_indexdate = paste0(as.numeric(format(as.Date(input$ecshareIndex), "%Y")) - 1, "-12-31")
         ecas_index = ecasdat %>%
            mutate(Spend = Spend/Spend[TimePeriod == eoy_indexdate] * 100,
                   Arrivals = Arrivals/Arrivals[TimePeriod == eoy_indexdate] * 100)
         ecas_xts = xts(ecas_index[c("Spend", "Arrivals")], ecas_index$TimePeriod)
         
         dat_merge = merge(shares_xts, ecas_xts, all = TRUE)
         pal = paldy(names(dat_merge), c(shares_names, "Spend", "Arrivals"))
         
         curtitle = paste("Index of tourism-related share prices")
         output$ecsharedy = dyforex(dat_merge, curtitle, pal, "ecsharedyLegend",
                                    isolate(input$ecsharedy_date_window))
      
         ## Handle drawing and download of Data Table
         tabdat = cbind(Date = as.character(index(dat_merge)), as.data.frame(dat_merge))
         output$ecshareTable = renderDataTable(tabdat,
            options = list(order = list(c("0", "desc"))))
         output$ecshareTableDown = tableComboDown(tabdat, "Tourism-related_share_prices")
      }
   })
   
   checkboxComboServer(input, session, "ecsharechk", shares_names)
})

ecgrof = function(env_serv) with(env_serv, {
   ## Economic Context - Economic Growth Forecasts
   observe({
      curCountry = input$ecgrofCountry
      IndexDate = paste0(as.numeric(input$ecgrofIndex), "-12-31")
      
      ###################
      ## Economic data ##
      ###################
      dat_eco = imf_weo %>%
         mutate(TimePeriod = as.Date(paste0(Year, "-12-31"))) %>%
         filter(Country %in% c(curCountry, "New Zealand")) %>%
         group_by(Country) %>%
         mutate(Value = Value/Value[TimePeriod == IndexDate] * 100) %>%
         ungroup()
      dat_base = dat_eco %>%
         mutate(Country = ifelse(Year >= EstFrom, paste(Country, "(forecast)"), as.character(Country)))
      dat_repeat = dat_eco %>%
         filter(Year == EstFrom)
      dat_ecoFinal = rbind(dat_base, dat_repeat) %>%
         select(-Year, -EstFrom) %>%
         spread(Country, Value)
      
      ######################
      ## Spend & Arrivals ##
      ######################
      ivs_country = weo_ctoivs[[curCountry]]
      dat_spend_arriv = get_spend_arriv_dat(ivs_country, min(weo_years), IndexDate)
      
      ################
      ## Merge Data ##
      ################
      dat_merge = merge(dat_ecoFinal, dat_spend_arriv, all = TRUE)
      xts_merge = xts(dat_merge[-1], dat_merge$TimePeriod)
      ## Manually set pal name order for consistent colour
      palnames = c(paste0("New Zealand", c("", " (forecast)")),
                   paste0(curCountry, c(" (forecast)", "")),
                   "Total spend", "Arrivals")
      pal = paldy(names(dat_merge), palnames)
      
      curtitle = paste("Index of GDP",
         "vs total spend and arrivals from", curCountry)
      output$ecgrofdy = dyforex(xts_merge, curtitle, pal, "ecgrofdyLegend",
                                 isolate(input$ecgrofdy_date_window))
   
      ## Handle drawing and download of Data Table
      output$ecgrofTable = renderDataTable(dat_merge,
         options = list(order = list(c("0", "desc"))))
      output$ecgrofTableDown = tableComboDown(dat_merge, curtitle)
   })
})
