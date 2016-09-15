oeco = function() tagList(
   div(class = "float divfull row-odd",
      ## ROW 1
      div(id = "oecogdplabel", class = "float dylegend divplothalf-label"),
      ## GDP contribution as %
      div(class = "float divplothalf",
         dygraphOutput("oecogdpperc", height = 400)
      ),
      ## GDP contribution
      div(class = "float divplothalf",
         dygraphOutput("oecogdp", height = 400)
      )
   ),
   ## ROW 2
   div(class = "float divfull row-even",
      div(id = "oecoemplabel", class = "float dylegend divplothalf-label"),
      ## Employment contribution as %
      div(class = "float divplothalf",
         dygraphOutput("oecoempperc", height = 400)
      ),
      ## Employment contribution
      div(class = "float divplothalf",
         dygraphOutput("oecoemp", height = 400)
      )
   ),
   ## ROW 3
   div(class = "float divfull row-odd",
      div(id = "oecoexplabel", class = "float dylegend divplothalf-label"),
      ## Intl Tourism as % of Exports
      div(class = "float divplothalf",
         dygraphOutput("oecoexpperc", height = 400)
      ),
      ## Intl Tourist Spend
      div(class = "float divplothalf",
         dygraphOutput("oecospend", height = 400)
      )
   ),
   ## ROW 4
   div(class = "float divfull",
      div(class = "float divplothalf",
         datasource2(tourism_economic)
      ),
      div(class = "float divplothalf", style = "padding-left: 86px;",
         dyDownloadGroup("oecoDyDownloads", "Download Plots:", c(
               "Value added by tourism as a percentage of GDP" = "oecogdpperc",
               "Value added by tourism" = "oecogdp",
               "Tourism employment as a percentage of total employment" = "oecoempperc",
               "Tourism employment" = "oecoemp",
               "International tourism as a percentage of total exports" = "oecoexpperc",
               "Total expenditure" = "oecospend"
            ))
      )
   )
)

oiva = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            radioButtons("oivaobsroll", "Choose what to plot:", choices = obsroll, selected = "Both"),
            radioButtons("oivatype", "Type", arrivType),
            radioButtons("oivavariable", "Variable", arrivVar),
            condInputs("oivadimension", cats = arrivType,
                       choices = country_pov,
                       type = "checkboxComboInput",
                       cond_simple = "oivatype")
         )
      ),
      div(id = "oivadyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("oivady", "Download Plot", asbutton = TRUE),
      dygraphOutput("oivady", height = 600),
      datasource2(arriv)
   )
), fixedRow(
   tableCombo("oivaTable")
))

oivs = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            radioButtons("oivsobsroll", "Choose what to plot:", choices = obsroll, selected = "Both"),
            radioButtons("oivstype", "Type", arrivType),
            radioButtons("oivsvariable", "Variable", ivsVar),
            condInputs("oivsdimension", cats = arrivType,
                       choices = country_pov,
                       type = "checkboxComboInput",
                       cond_simple = "oivstype")
         )
      ),
      div(id = "oivsdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("oivsdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("oivsdy", height = 600),
      datasource2(ivs)
   )
), fixedRow(
   tableCombo("oivsTable")
))

oivas = function() tagList(
   div(class = "float divwell", tags$form(class = "well",
      radioButtons("oivasobsroll", "Choose what to plot:", choices = obsroll, selected = "Both"),
      radioButtons("oivastype", "Type", arrivType),
      selectInputF("oivasBase", "Set base index period (12 months ending...)", ivsIndexBase),
      condInputs("oivasdimension", cats = arrivType,
                 choices = country_pov,
                 type = "selectInputF",
                 cond_simple = "oivastype"),
      div(id = "oivasdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("oivasdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("oivasdy", height = 600),
      datasource2(ivs, arriv)
   )
)

rs_all = function() tagList(
   div(class = "float divwell",
      tags$form(class = "well",
         radioButtons("rsAreaClass", "Regional level:", MVareaclass),
         condInputs("rsAreaSub", cats = MVareaclass,
                    choices = MVarealist,
                    type = "selectInputF",
                    cond_simple = "rsAreaClass")
         # downloadButton("rspdf", label = "Download as PDF report")
      )
   ),
   div(class = "float divplot",
      ## COL 1
      div(class = "float divflex",
         ## ROW 1
         div(id = "rstsintlabels", class = "float dylegend divplothalf-label"),
         ## TimeSeries International
         div(class = "float divfull",
            dygraphOutput("rstsint", height = 500)
         ),
         ## ROW 2
         ## Spend By Origin Dotchart International
         div(class = "float divfull",
            uiOutput("rsoriginintTitle"),
            ggvisOutput("rsoriginint")
         ),
         ## ROW 3
         ## Spend By Product Dotchart International
         div(class = "float divfull",
            uiOutput("rsproductintTitle"),
            ggvisOutput("rsproductint")
         )
      ),
      ## COL 2
      div(class = "float divflex",
         ## ROW 1
         div(id = "rstsdomlabels", class = "float dylegend divplothalf-label"),
         ## TimeSeries Domestic
         div(class = "float divfull",
            dygraphOutput("rstsdom", height = 500)
         ),
         ## ROW 2
         ## Spend By Origin Dotchart Domestic
         div(class = "float divfull",
            style = "height: 548px;", # TEMPORARY HEIGHT TO FILL SPACE
            uiOutput("rsorigindomTitle"),
            ggvisOutput("rsorigindom")
         ),
         ## ROW 3
         ## Spend By Product Dotchart Domestic
         div(class = "float divfull",
            uiOutput("rsproductdomTitle"),
            ggvisOutput("rsproductdom")
            # highchartOutput("rsproductdom", height = 350)
         )
      ),
      div(class = "float divfull",
         datasource2(MVmod)
      )
   )
)

racc = function() tagList(
fixedRow(
   ## IVS
   div(class = "float divwell", tags$form(class = "well",
      radioButtons("raccdomint", "Choose Origin", domint, "int"),
      tags$h3(class = "input-header", "Visitor Nights"),
      div(
         tags$p("These inputs relate to the top graph and represent all visitor nights",
            "including non-commercial accommodation, such as staying with family and friends."),
         tags$p("See also",
            tags$span(class = "defword", "Visitor Markets - Accommodation used"),
            "for breakdowns of the accommodation types."),
         selectInputF("raccareaRC", "Choose Destination Region:", NightsAreaList$RC),
         radioButtons("raccprop", "Choose what to plot:", c("Visitor Nights", "Proportions")),
         condInputs("raccorigin",
                    cats = NightsOriginscats,
                    choices = NightsOrigins,
                    selected = TRUE,
                    type = "checkboxComboInput",
                    cond_simple = "raccdomint")
      ),
      tags$h3(class = "input-header", "Commercial Visitor Nights"),
      div(
         tags$p("These inputs relate to the bottom graph and represent commercial visitor nights only.",
            "The option to show a 12-month rolling SUM is included for comparison with the graph of all visitor nights."),
         tags$p("See also",
            tags$span(class = "defword", "Industry - Commercial Accommodation"),
            "for more."),
         radioButtons("racccamsum", "Choose what to plot:", choices = c("Observation", "12-month rolling sum")),
         checkboxComboInput("racccamarea", "Choose Destination Regions:", NightsCAMArea)
      )
   )),
   div(class = "float divplotduo",
      dyDownload("raccdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("raccdy", height = 500),
      datasource2(nights_origin)
   ),
   div(class = "float divwell divplotduo-label", tags$form(class = "well",
      div(id = "raccdyLegend", class = "dylegend")
   )),
   ## CAM
   div(class = "float divplotduo",
      dyDownload("racccamdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("racccamdy", height = 500),
      datasource2(nights_region)
   ),
   div(class = "float divwell divplotduo-label", tags$form(class = "well",
      div(id = "racccamdyLegend", class = "dylegend")
   ))
), fixedRow(
   tableCombo("raccTable")
), fixedRow(
   tableCombo("racccamTable")
))

rins = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      selectInputF("rinsYear", paste0("Year Ended ", MVYE_str, ":"), rev(MVYEyears)),
      radioButtons("rinsRTR", "Select Regional Level:", MVareaclass),
      radioButtons("rinsdomint", "Choose Origin:", MVorigins),
      # condInputs("rinsOrigin", MVorigins[-1], MVoriginslist, "selectInputF", cond_simple = "rinsdomint"),
      condInputs("rinsOrigin", MVorigins[-c(1, 2)], MVoriginslist, "selectInputF", cond_simple = "rinsdomint"),
      radioButtons("rinsScaled", "Choose what to plot:", catscale)
   )),
   div(class = "float divplot",
      uiOutput("rinsggTitle"),
      conditionalPanel('input.rinsScaled == "sv"',
         ggvisOutput("rinsggsv")
      ),
      conditionalPanel('input.rinsScaled == "dv"',
         ggvisOutput("rinsggdv")
      ),
      # highchartOutput("rinshc", height = 600),
      datasource2(MVmod)
   )
), fixedRow(
   tableCombo("rinsTable")
))

rgdp = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            radioButtons("rgdpAreaType", "Regional Level:", GDP_AreaType),
            condInputs("rgdpArea", cats = GDP_AreaType,
                       choices = GDP_AreaList,
                       type = "checkboxComboInput",
                       cond_simple = "rgdpAreaType")
         )
      ),
      div(id = "rgdpdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("rgdpdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("rgdpdy", height = 600),
      datasource2(GDP_tourism)
   )
), fixedRow(
   tableCombo("rgdpTable")
))

iacc = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            radioButtons("iaccobsroll", "Choose what to plot:", choices = obsroll, selected = "Both"),
            selectInputF("iacctype", "Type of accommodation", AccommType),
            radioButtons("iaccclass", "Variable", AccommClass),
            checkboxComboInput("iaccRTO", "Choose a Regional Tourism Organisation", AccommRTOs)
         )
      ),
      div(id = "iaccdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("iaccdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("iaccdy", height = 600),
      datasource2(Accomm_RTO)
   )
), fixedRow(
   tableCombo("iaccTable")
))

iact = function() tagList(
   div(class = "float divwell", style = paste0("height: ", 550 * 2 + 20, "px;"),
      tags$form(class = "well",
      radioButtons("iactobsroll", "Choose what to plot:", choices = obsroll, selected = "Both"),
      helpText(paste(
         "These figures represent total spending/nights stayed by visitors who have participated",
         "in at least one activity classified into one of the activity categories below.",
         "They DO NOT represent spending/nights stayed directly relating to the specific activities."
      )),
      helpText(ivs_act_text),
      radioButtons("iacttype", "Activity", act_types),
      checkboxComboInput("iactcountry", "Country", all_countries),
      dyDownloadGroup("iactDyDownloads", "Download Plots:", c(
            "Average nights stayed" = "iactnights",
            "Average spend per night" = "iactspendnights",
            "Average spend per trip" = "iactspendtrip",
            "Total spend" = "iacttotalspend"
         ))
   )),
   ## ROW 1
   div(class = "float divplot row-odd",
      div(id = "iactlabelsR1", class = "float dylegend divplothalf-label-large"),
      ## Nights Stayed
      div(class = "float divplothalf",
         dygraphOutput("iactnights", height = 500)
      ),
      ## Spend per Night
      div(class = "float divplothalf",
         dygraphOutput("iactspendnights", height = 500)
      )
   ),
   ## ROW 2
   div(class = "float divplot row-even",
      div(id = "iactlabelsR2", class = "float dylegend divplothalf-label-large"),
      ## Spend per Trip
      div(class = "float divplothalf",
         dygraphOutput("iactspendtrip", height = 500)
      ),
      ## Total Spend
      div(class = "float divplothalf",
         dygraphOutput("iacttotalspend", height = 500)
      )
   ),
   ## ROW 3
   ## Shared Activities
   # div(class = "float divplothalf",
      # ggvisOutput("iactshared")
   # ),
   ## Number of Shared Activities
   # div(class = "float divplothalf",
      # ggvisOutput("iactnumshared")
   # ),
   div(class = "float align-well", datasource_ivs_act),
   div(class = "float divfull well",
      tags$h4("Activity Category Breakdowns"),
      act_breakdowns
   )
)

ibdem = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      helpText(paste(
         '"Accommodation and Food Services" is considered',
         'to be an approximation of the Tourism Industry'
      )),
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            checkboxComboInput("ibdemIndustry", "Choose Industries:", bdem_IndustryLevels),
            radioButtons("ibdemData", "Dataset:", bdem_Data),
            condInputs("ibdemCat", cats = bdem_Data,
                       choices = bdem_Cats,
                       type = "selectInputF",
                       cond_simple = "ibdemData"),
            radioButtons("ibdemUnit", "Units:", bdem_UnitLevels)
         )
      ),
      div(id = "ibdemdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("ibdemdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("ibdemdy", height = 600),
      datasource2(bdem_countsize)
   )
), fixedRow(
   tableCombo("ibdemTable")
))

icas = function() tagList(
   div(class = "float divwell", style = paste0("height: ", 460 * 5, "px;"),
      tags$form(class = "well",
         radioButtons("icasobsroll", "Choose what to plot:", choices = obsroll[-3]),
         dyDownloadGroup("icasDyDownloads", "Download Plots:", c(
            "Count of all events" = "icasCountEvents",
            "Event type" = "icasCountEventsByType",
            "Event length" = "icasCountSingleMulti",
            "Event size" = "icasCountEventSize",
            "Total delegate days" = "icasDelegateDays",
            "Delegate days by type" = "icasDelegateDaysByType",
            "Delegate capacity" = "icasDelegateCapacity",
            "Total delegates" = "icasTotalDelegates",
            "Customer type" = "icasCustomerType",
            "Delegate by origin" = "icasDelegatesByOrigin"
         ))
      )
   ),
   ## ROW 1
   div(class = "float divplot row-odd",
      div(id = "icasRowLabels1", class = "float dylegend divplothalf-label"),
      ## Count of events
      div(class = "float divplothalf",
         dygraphOutput("icasCountEvents", height = 400)
      ),
      ## Counts of event types
      div(class = "float divplothalf",
         dygraphOutput("icasCountEventsByType", height = 400)
      )
   ),
   ## ROW 2
   div(class = "float divplot row-even",
      div(id = "icasRowLabels2", class = "float dylegend divplothalf-label"),
      ## Count of single v multi day events
      div(class = "float divplothalf",
         dygraphOutput("icasCountSingleMulti", height = 400)
      ),
      ## Event size (taken from Event size for different event types, using All events only)
      div(class = "float divplothalf",
         dygraphOutput("icasCountEventSize", height = 400)
      )
   ),
   ## ROW 3
   div(class = "float divplot row-odd",
      div(id = "icasRowLabels3", class = "float dylegend divplothalf-label"),
      ## Delegate days
      div(class = "float divplothalf",
         dygraphOutput("icasDelegateDays", height = 400)
      ),
      ## Delegate days by event type
      div(class = "float divplothalf",
         dygraphOutput("icasDelegateDaysByType", height = 400)
      )
   ),
   ## ROW 4
   div(class = "float divplot row-even",
      div(id = "icasRowLabels4", class = "float dylegend divplothalf-label"),
      ## Delegate capacity
      div(class = "float divplothalf",
         dygraphOutput("icasDelegateCapacity", height = 400)
      ),
      ## Total delegates
      div(class = "float divplothalf",
         dygraphOutput("icasTotalDelegates", height = 400)
      )
   ),
   ## ROW 5
   div(class = "float divplot row-odd",
      div(id = "icasRowLabels5", class = "float dylegend divplothalf-label"),
      ## Customer type
      div(class = "float divplothalf",
         dygraphOutput("icasCustomerType", height = 400)
      ),
      ## Total delegates by origin
      div(class = "float divplothalf",
         dygraphOutput("icasDelegatesByOrigin", height = 400)
      )
   ),
   div(class = "float align-well", datasource2(all_cas))
)

isvm = function() tagList(
   div(class = "float divwell", tags$form(class = "well",
      selectInputF("isvmYear", paste0("Year Ended ", MVYE_str, ":"), rev(MVYEyears)),
      # radioButtons("isvmdomint", "Choose Origin:", MVorigins[-1], MVorigins[3]),
      radioButtons("isvmRTR", "Select Destination:", MVareaclass),
      condInputs("isvmDest", MVareaclass, MVarealisttots, "selectInputF", cond_simple = "isvmRTR"),
      radioButtons("isvmScaled", "Choose what to plot:", catscale)
   )),
   div(class = "float divplot",
      uiOutput("isvmggTitle"),
      conditionalPanel('input.isvmScaled == "sv"',
         ggvisOutput("isvmggsv")
      ),
      conditionalPanel('input.isvmScaled == "dv"',
         ggvisOutput("isvmggdv")
      ),
      datasource2(MVmod)
   )
)

vmorigin = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            selectInputF("vmOriginProduct", "Choose the Product:", MVproducts),
            radioButtons("vmOriginclass", "Select destination:", MVareaclass),
            condInputs("vmOriginOne", MVareaclass, MVarealisttots, "selectInputF", cond_simple = "vmOriginclass"),
            radioButtons("vmOrigindomint", "Choose Origin:", MVorigins),
            # condInputs("vmOriginList", MVorigins[-1], MVoriginslist, "checkboxComboInput", cond_simple = "vmOrigindomint")
            condInputs("vmOriginList", MVorigins[-c(1, 2)], MVoriginslist, "checkboxComboInput", cond_simple = "vmOrigindomint")
         )
      ),
      radioButtons("vmOriginScale", "Choose what to plot:", catscale),
      div(id = "vmOriginLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("vmOriginPlot", "Download Plot", asbutton = TRUE),
      dygraphOutput("vmOriginPlot", height = 600),
      datasource2(MVmod)
   )
), fixedRow(
   tableCombo("vmOriginTable")
))

vmdest = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            radioButtons("vmDestdomint", "Choose Origin:", MVorigins),
            # condInputs("vmDestOrigin", MVorigins[-1], MVoriginslist, "selectInputF", cond_simple = "vmDestdomint"),
            condInputs("vmDestOrigin", MVorigins[-c(1, 2)], MVoriginslist, "selectInputF", cond_simple = "vmDestdomint"),
            selectInputF("vmDestProduct", "Choose the Product:", MVproducts),
            radioButtons("vmDestclass", "Select destination:", MVareaclass),
            condInputs("vmDestList", MVareaclass, MVarealisttots, "checkboxComboInput",
                       labels = MVareaclass_s, cond_simple = "vmDestclass")
         )
      ),
      radioButtons("vmDestScale", "Choose what to plot:", catscale),
      div(id = "vmDestLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("vmDestPlot", "Download Plot", asbutton = TRUE),
      dygraphOutput("vmDestPlot", height = 600),
      datasource2(MVmod)
   )
), fixedRow(
   tableCombo("vmDestTable")
))

vmacc = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      # radioButtons("vmaccvariable", "Variable", ivs_values),
      radioButtons("vmaccScaled", "Choose what to plot:", ivs_accomm_scale),
      selectInputF("vmaccYEnd", "For the 36-months ended:", ivs_accomm_dates)
   )),
   div(class = "float divplot",
      uiOutput("vmaccggTitle"),
      conditionalPanel('input.vmaccScaled == "sv"',
         ggvisOutput("vmaccggsv")
      ),
      conditionalPanel('input.vmaccScaled == "rv"',
         ggvisOutput("vmaccggrv")
      ),
      datasource_ivs_accomm_used
   )
), fixedRow(
   tableCombo("vmaccTable")
))

gcforex = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            selectInputF("gcforexCurrency", "Select a Currency:", fx_currencies),
            div(class = "jui-tip",
               title = "Index = 100 at the end of the Index Year",
               selectInputF("gcforexIndex", "Index Year", fx_all_years)
            )
         )
      ),
      div(id = "gcforexdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("gcforexdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("gcforexdy", height = 600),
      datasource("OANDA, using the R package quantmod")
   )
), fixedRow(
   tableCombo("gcforexTable")
))

ecshare = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            checkboxComboInput("ecsharechk", "Select Companies:", shares_names, TRUE),
            div(class = "jui-tip",
               title = "Index = 100 at the end of the Index Year",
               selectInputF("ecshareIndex", "Index Year", shares_index_dates)
            )
         )
      ),
      div(id = "ecsharedyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("ecsharedy", "Download Plot", asbutton = TRUE),
      dygraphOutput("ecsharedy", height = 600),
      datasource("Adjusted Close Prices, Yahoo Finance, using the R package quantmod")
   )
), fixedRow(
   tableCombo("ecshareTable")
))

ecgrof = function() tagList(
fixedRow(
   div(class = "float divwell", tags$form(class = "well",
      div(class = "divinput",
         tags$h3("Show/Hide"),
         div(
            selectInputF("ecgrofCountry", "Select a Country:", weo_countries),
            div(class = "jui-tip",
               title = "Index = 100 at the end of the Index Year",
               selectInputF("ecgrofIndex", "Index Year", weo_years)
            )
         )
      ),
      div(id = "ecgrofdyLegend", class = "dylegend")
   )),
   div(class = "float divplot",
      dyDownload("ecgrofdy", "Download Plot", asbutton = TRUE),
      dygraphOutput("ecgrofdy", height = 600),
      datasource2(imf_weo)
   )
), fixedRow(
   tableCombo("ecgrofTable")
))
