################
## Data Funcs ##
################
rs_rti_subset =
   ## Subset RTI based on area class (RTO/Region/TA) and Domestic/International
   function(RTIs_concorded, Area, domint){
      out = RTIs_concorded %>%
         filter(RecordType == domint) %>%
         group_by_("Period", Area) %>%
         summarise(Spend = sum(Spend)) %>%
         arrange(Period) %>%
         rename_(Area = Area)
      class(out) = "data.frame"
      out
   }

rs_rte_subset =
   ## Get latest year RTE for Domestic/International
   function(RTEs_concorded, Area, domint, CurrentYear){
      RTEs_concorded %>%
         filter(YEMar == CurrentYear, Type == domint) %>%
         rename_(Area = Area)
   }

rs_rte_by =
   ## Group the RTE subset by Origin or Product and compute proportions
   ## Also compute the proportions for All New Zealand, for comparison
   ## rs_rte_subset %>% rs_rte_by
   function(rte_subset, byvar){
      rte_spread = rte_subset %>%
         group_by_(byvar, "Area") %>%
         summarise(Spend = sum(Spend)) %>%
         spread(Area, Spend)
      rte_prop = cbind(
         rte_spread[,1],
         "All New Zealand" = apply(rte_spread[,-1], 1, function(x) sum(x, na.rm = TRUE))/
                                   sum(rte_spread[,-1], na.rm = TRUE),
         apply(rte_spread[,-1], 2, function(x) x/sum(x, na.rm = TRUE))
      )
      rte_by = switch(byvar,
         Origin = gather(rte_prop, Area, Spend, -Origin),
         Product = gather(rte_prop, Area, Spend, -Product)
      )
      
      ## If Origin, reorder
      if(byvar == "Origin"){
         domint = as.character(unique(rte_subset$Type))
         if(domint == "Domestic"){
            ## order origin from south to north
            Oorder = c("Southland", "Otago", "West Coast", "Canterbury",
               "Marlborough", "Nelson", "Tasman", "Wellington",
               "Manawatu-Whanganui", "Taranaki", "Hawke's Bay" ,"Gisborne" ,
               "Bay of Plenty" , "Waikato" , "Auckland", "Northland")
            rte_by$Origin = factor(rte_by$Origin, levels = Oorder)
         } else{
            ## order origin by order of NZ total spend
            Origin_levels = with(filter(rte_by, Area == "All New Zealand"),
                                 as.character(Origin)[order(Spend)])
            ind_rest = grep("(^Rest of)|(^Africa and Middle East)", Origin_levels)
            Oorder = c(Origin_levels[ind_rest], Origin_levels[-ind_rest])
            rte_by$Origin = factor(rte_by$Origin, levels = Oorder)
         }
      }
      
      rte_by
   }

rs_rte_subarea =
   ## Get only the chosen subarea
   ## rs_rte_subset %>% rs_rte_by %>% rs_rte_subarea
   function(rte_by, subarea){
      rte_by_filter = filter(rte_by, Area %in% c("All New Zealand", subarea))
      rte_by_filter$Area = factor(rte_by_filter$Area)
      rte_by_filter
   }

################
## Plot Funcs ##
################
## Get colours and strip transparency
col_all = substr(tourism.cols(c(3, 14, 8)), 0, 7)
col_cur = col_all[1]
col_curlight = col_all[2]
col_NZ = col_all[3]

rs_ts_title = function(subarea, value, domint, CurrentYear){
   domintlong = switch(domint,
      dom = "Domestic",
      int = "International"
   )
   paste0(domintlong, " tourist expenditure in ", subarea,
          " (total in year ending March ", CurrentYear, 
          " = $", format(round(value, -1), big.mark = ","), "m)")
}
rs_ts_prep = function(rsdata, subarea, domint, CurrentYear){
   totval = sum(filter(rsdata[[paste0("rte_", domint)]], Area == subarea)$Spend)
   basedat = cbind(
      filter(rsdata[[paste0("rti_", domint)]], Area == subarea),
      rsdata[[paste0("rti_", domint, "NZ")]]
   )
   list(
      data = list(date = basedat$Period, num = basedat[c("Spend", "NZ")]),
      title = rs_ts_title(subarea, totval, domint, CurrentYear)
   )
}
rs_ts_roll = function(rawnum, baserow, subarea){
   datIndex = rebaseIndex(rawnum, baserow)
   ## Original values for current area
   ## + 12-month rolling average for area and All NZ
   curnum = cbind(
      datIndex[,"Spend"],
      rollmean(datIndex[,"Spend"], 12, fill = NA, align = "right"),
      rollmean(datIndex[,"NZ"], 12, fill = NA, align = "right")
   )
   colnames(curnum) = c(subarea, paste(subarea, "(12-mth)"), "All NZ (12-mth)")
   curnum
}
rs_ts = function(curdat, subarea, baserow, main = NULL, ylab = "Index",
                 labelsDiv = "rstslabels", labelsSeparateLines = FALSE){
   curnum = rs_ts_roll(curdat$num, baserow, subarea)
   xts(curnum, curdat$date) %>%
      padxts() %>%
      dygraph(main = main) %>%
      dySeries(subarea, strokePattern = "dotted", color = col_curlight) %>%
      dySeries(paste(subarea, "(12-mth)"), strokeWidth = 3, color = col_cur) %>%
      dySeries("All NZ (12-mth)", strokeWidth = 3, strokePattern = "dashed", color = col_NZ) %>%
      dyRangeSelector() %>%
      dyLegend(labelsDiv = labelsDiv, labelsSeparateLines = labelsSeparateLines) %>%
      dyCallbacks(drawCallback = dyRegister()) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 5)) %>%
      dyAxis("y", ylab) %>%
      dyOptions(rightGap = 25) %>%
      return
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
rs_origin = function(curdat, subarea, domint, CurrentYear){
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
## ggplot funcs for exporting
rs_ggtheme =
   theme(text = element_text(size = 7, family = "Calibri"),
         plot.title = element_text(size = 9, family = "Calibri"),
         panel.grid.major = element_line(colour = "#EAEAEA"),
         panel.grid.minor = element_line(colour = "#EFEFEF"),
         panel.background = element_rect(fill = "#FDFDFD", colour = NA),
         legend.title=element_blank(),
         legend.key = element_rect(fill = NA))
rs_haslegend = function(x, haslegend){
   if(haslegend == TRUE) x + theme(legend.position = "bottom")
   else x + theme(legend.position = "none")
}
rs_ts_ggplot = function(curdat, subarea, baserow, main = NULL, haslegend){
   curnum = rs_ts_roll(curdat$num, baserow, subarea)
   curdf = data.frame(Period = curdat$date, curnum, check.names = FALSE) %>%
      gather(Area, Spend, -Period) %>%
      filter(!is.na(Spend))
   
   ggplot(curdf, aes(x = Period, y = Spend, linetype = Area, size = Area, colour = Area)) +
      geom_line() +
      scale_linetype_manual(values = c("solid", "solid", "dashed")) +
      scale_size_manual(values = c(0.4, 1.1, 1.1)) +
      scale_color_manual(values = c(col_curlight, col_cur, col_NZ)) +
      labs(x = "", y = "Index", title = main) +
      rs_ggtheme %>%
      rs_haslegend(haslegend)
}
rs_dc_ggplot = function(curdat, type, subarea, domint){
   main = if(type == "Origin")
      paste0(domint, " tourist spend in ", subarea, 
             "\nCompared to all New Zealand") else ""
   haslegend = type == "Origin" && domint == "International"
   ylab = if(type == "Origin" && domint == "Domestic")
      "Origin Region\n" else ""
   xlab = if(type == "Product")
      "\nPercentage of Spend" else ""
      
   dat_diff = rs_dc_diff(curdat)
   dat_diff$Area = subarea
   dat_diff$Area[dat_diff[,2] > dat_diff[,3]] = "All New Zealand"
   cur_aes = switch(type,
      Origin = aes(x = Spend, y = Origin, size = Area, colour = Area),
      Product = aes(x = Spend, y = Product, size = Area, colour = Area)
   )
   ggplot(curdat, cur_aes) +
      geom_segment(aes(x = x1, xend = x2, y = y, yend = y, size = NULL),
                       data = dat_diff, show_guide = FALSE) +
      geom_point() +
      scale_x_continuous(label = percent) +
      scale_size_manual(values = c(2, 2)) +
      scale_color_manual(values = c(col_NZ, col_cur)) +
      labs(x = xlab, y = ylab, title = main) +
      rs_ggtheme %>%
      rs_haslegend(haslegend)
}
rs_pdf = function(filepath, pdf_dat) with(pdf_dat, {
   one_pager = function(){
      vplayout <- function(x,y) {
         # Function to help in regular layouts in grid environment
         # this function taken from Hadley Wickham's ggplot2 book
         viewport(layout.pos.row=x, layout.pos.col=y)
      }
      grid.newpage()

      ## Split the first page into a a 16 row x 2 column grid
      pushViewport(viewport(layout=grid.layout(17,2)))

      ## place the first 6 plots
      print(ts_int, vp=vplayout(2:5,1:2))
      print(ts_dom, vp=vplayout(6:8,1:2))
      print(dc_origin_int, vp=vplayout(9:13,1))
      print(dc_origin_dom, vp=vplayout(9:13,2))
      print(dc_product_int, vp=vplayout(14:16,1))
      print(dc_product_dom, vp=vplayout(14:16,2))

      grid.text("Regional Tourism summary", .5, .98, gp = gpar(cex = 1.2, fontfamily = "Calibri"))
      grid.text(paste0(area, " (for the year ending March ", CurrentYear, ")"), .5, .96, gp = gpar(cex = 1.2, fontfamily = "Calibri"))
      grid.raster(mbie_footer, 0.5, 0.03, width = 1)
   }
   CairoPDF(filepath, width=8.1, height=11.4, paper="a4")
   one_pager()
   dev.off()
})
