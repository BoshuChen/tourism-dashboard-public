tabdesc = c(
   "Economic Contribution" = "This tab shows the annual high-level contribution of tourism to the national economy in terms of expenditure, employment and exports.",
   "International Visitor Arrivals" = "This tab shows national monthly arrivals and stock of international visitors, by country and purpose of visit.",
   "International Visitor Spend" = "This tab shows national monthly spend (total, per trip and per night) of international visitors, by country and purpose of visit.",
   "International Visitor Arrivals vs Spend" = "This tab compares national monthly arrivals with the total spend of international visitors, by country and purpose of visit.",
   "Regional Summaries" = "This tab provides an annual high-level tourism overview for regions, RTOs and territorial authorities. It provides information on tourism expenditure by country and tourism product.",
   "Visitor Nights" = "This tab shows annual total and commercial visitor nights by New Zealand region for international and domestic visitors. Domestic visitors' total nights are not available after 2012.",
   "Tourism Product" = "This tab shows the proportion of annual spend by tourism product for international and domestic visitors, for regions, RTOs and territorial authorities.",
   "Tourism as a percentage of Regional GDP" = "This tab shows annual international tourism expenditure as a percentage of GDP, by regions and territorial authorities.",
   "Commercial Accommodation" = "This tab shows a variety of monthly accommodation statistics for RTOs, by different types of commercial accommodation.",
   "Attractions and Activities" = "This tab shows national nights stayed, total spend, spend per night and spend per trip for international visitors, by tourism-related activity. Data is presented quarterly.",
   "Business Demography" = "This tab shows annual national trends by industry for a variety of statistics on businesses and employees.",
   "Business Events" = "This tab shows quarterly key diagnostics on the number, type, length and size of business events nationally.",
   "Spend by Visitor Market" = "This tab shows annual spend by international visitors by country, and annual spend by domestic visitors by region, by tourism product, for regions, RTOs, and territorial authorities.",
   "Visitor Spend by Origin" = "This tab shows annual spend by international and domestic visitors by tourism product, for regions, RTOs and territorial authorities. You can compare between domestic origins by region, or international origins by country.",
   "Visitor Spend by Destination" = "This tab shows annual spend by international and domestic visitors by tourism product, for regions, RTOs and territorial authorities. You can compare between destinations by region, RTO or territorial authority.",
   "Accommodation Used" = "This tab shows visitors over a three-year period by country and detailed type of accommodation used.",
   "Exchange Rates" = "This tab shows daily indexes of exchange rates for our major tourism markets, compared with the number of arrivals and total spend from those markets.",
   "Tourism-related Share Prices" = "This tab shows daily indexes of share prices for New Zealand's main tourism-related publically listed companies, compared with total arrivals and tourism spend.",
   "Main Market GDP" = "This tab shows historical gross domestic product for our main markets, along with latest available forecasts, compared with the number of arrivals and total spend from those markets."
)

frontp = function() div(class = "frontp",
   div(class = "front-banner",
      div(class = "imgcon"),
      div(class = "hcon", h1("The New Zealand"), h1("Tourism Dashboard"))
   ),
   #tags$p(tags$span(class = "warning", "This app is still in active development and has not been officially launched.")),
   tags$p(class = "intro", "The New Zealand Tourism Dashboard is a one-stop shop for all information about tourism. It brings together a range of tourism datasets produced by MBIE and Statistics New Zealand into one easy-to-use tool. Information is presented using dynamic graphs and data tables."),
   div(class = "intro-divider"),
   tags$p("Main subject-area groupings of tourism data are shown on the toolbar above. To navigate around this site, left-click one of the subject areas and then select one of the related categories in the drop down list."),
   tags$p("Graphs can be downloaded as png and tables can be downloaded to a csv file. Information on how to use the graphs and tables, along with an example, is available in the",
      tags$a("Help", title = "Help Tab", href = "#", id = "HelpTabLink"), "tab.",
      tags$script("$('#HelpTabLink').click(function(){$('a[data-value=\"Help\"]')[0].click();});")
   ),
   div(class = "box-con",
      tags$a(target = "_blank",
         href = "http://mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/tourism-data-sources",
         div(class = "float box box-more",
            tags$p(class = "intro", "Find out more"),
            tags$p("Click here for more information on the data sources used and their differences.")
      )),
      tags$a(target = "_blank",
         href = "http://www.mbie.govt.nz/info-services/business/business-growth-agenda/regions",
         div(class = "float box box-rear",
            tags$p(
               tags$img(class = "rear-preview", src = "REAR-cover-2015.jpg"),
               "The", span(class = "bold", "Regional Economic Activity Report"),
               "produced by MBIE presents regional information and trends that supplements the information in this dashboard. Click here to find the online tool and mobile app."
            )
         )
      )
   ),
   div(class = "box box-timeout",
      tags$p(tags$span(class = "bold", "PLEASE NOTE:"),
             "This app may time-out if left idle too long, which will cause the screen to grey-out.",
             "To use the app again, refresh the page. This will reset all previously-selected input options.")
   )
)

helpp = function() div(class = "frontp",
   h3("Graphs"),
   div(
      tags$p("The graphs in this dashboard will provide more information when you mouseover elements of the graph."),
      tags$p("Many of the graphs are interactive. These graphs have toolbars which can be used to select different data series and cut them in different ways."),
      tags$p("Use the toolbar on the left to choose what is graphed (when applicable). Information may be selected in several ways:"),
      tags$ul(
         tags$li("Drop down boxes are used when there is a large amount of options. Left-click the drop down box to display the list, then one option can be selected via left mouse button."),
         tags$li("Option boxes are used when only a few options are available. Left-clicking one option will deselect any others."),
         tags$li("Selection boxes are used when multiple series can be plotted on the graph. Left-click each of the series that you would like to be plotted and they will be marked with a tick. Selecting", tags$span(class = "defword", "Select all"), "will turn on all series.")
      ),
      tags$p("Additionally, the graphs for time series data have a number of other interactive features. Here are some tips to make the most out of these graphs."),
      tags$ul(
         tags$li("Moving your cursor over a specific series will highlight it, both on the graph and on the legend to the left."),
         tags$li("Zoom into a narrower time region by dragging across the area of the graph."),
         tags$li("As you zoom, the x axis labels will update automatically to something that is appropriate for the zoom level."),
         tags$li("Note that the dates on the x axis", tags$span(class = "warning", "do not"), "correspond to the dates of the data, they are merely labels."),
         tags$li("Zoom back out by double-clicking the graph."),
         tags$li("Selecting and dragging a range in the Range Selector allows you to change the time period while keeping the range length the same.")
      ),
      tags$p("The image of the graph may be downloaded in PNG format for some graphs."),
      tags$ul(
         tags$li("For some graphs, there is a", tags$span(class = "defword", "Download Plot"), "button above the top left-hand corner of the graph."),
         tags$li("Otherwise there will be download plots links at the bottom of the left toolbar, or at the bottom of the page."),
         tags$li("Download as PNG does not work with Internet Explorer.")
      ),
      h4("Interactive Graph Example"),
      div(
         tags$p("This time series graph, plotting total visitor spend from China and Japan, can be used to practice the interactive graph functions described above."),
         div(class = "frontexcon frontdycon",
            div(class = "float divwell", tags$form(class = "well",
               radioButtons("frontdyobsroll", "Choose what to plot:", choices = obsroll, selected = "Both"),
               div(id = "frontdyLegend", class = "dylegend")
            )),
            div(class = "float divplot",
               dyDownload("frontdyout", "Download Plot", asbutton = TRUE),
               dygraphOutput("frontdyout", height = 600)
            )
         ),
         tags$p("These time series graphs are powered by dygraphs for R. More information is available", tags$a("here.", href = "https://rstudio.github.io/dygraphs/", target = "_blank"))
      )
   ),
   h3("Data Tables"),
   div(
      tags$p("Here are some tips to make the most of the data tables which present the values from the graphs."),
      tags$ul(
         tags$li("Left-click on a column heading to sort the table by that heading. Left-click again to reverse sorting direction."),
         tags$li("Use the", tags$span(class = "defword", "Search"), "box on the top-right to filter the data. This will search in all columns."),
         tags$li("To search in a specific column only, use the individual search boxes below the table, e.g. search for", tags$span(class = "exans", "2010"), "in the", tags$span(class = "defword", "Date"), "search box to return data for 2010."),
         tags$li("The", tags$span(class = "defword", "Download table of values"), "button above the top left corner of the data table can be used to download the table of data as a csv file. The downloaded table will not reflect any sorting or filtering you have done with the online interactive version of the table."),
         tags$li("The number of values shown can be selected in the drop down box in the top left of the table.")
      ),
      h4("Data Table Example"),
      div(
         tags$p("This table, presenting data of total visitor spend from China and Japan, can be used to practice the data table functions described above."),
         div(class = "frontexcon",
            tableCombo("frontTable")
         ),
         tags$p("The data tables in the dashboard are powered by DataTables for jQuery. More information is available", tags$a("here.", href = "https://www.datatables.net/", target = "_blank"))
      )
   ),
   h3("Notes on Terminology"),
   div(
      tags$ul(
         tags$li(tags$span("Observations", class = "defword"),
                 "refer to the actual, underlying data values.")
      ),
      tags$ul(
         tags$li(tags$span("12-month rolling average (12-mth)", class = "defword"),
                 "is calculated by averaging the values of the previous 12 months",
                 "and is a way to view the general trend without seasonal variation.")
      ),
      tags$ul(
         tags$li(tags$span("YE", class = "defword"),
                 "Year Ended, e.g. YE March 2015 represents the period between 1 April 2014 and 31 March 2015.")
      ),
      tags$ul(
         tags$li(tags$span("RTO", class = "defword"),
                 "Regional Tourism Organisation.")
      ),
      tags$ul(
         tags$li(tags$span("TA", class = "defword"),
                 "Territorial Authority.")
      ),
      h4("Why do the TSA and IVS show different high-level results?"),
      div(
         tags$p("The Tourism Satellite Account (TSA) and International Visitor Survey (IVS), two major sources of tourism information, often do not align with one another. This section outlines the main differences between these two data sources"),
         tags$p("The TSA is an annual publication from Statistics New Zealand. It is a 'top-down' process, integrating data about the supply and use of tourism-related goods and services. It provides a summary measure of the contribution tourism makes to GDP and employment, consistent and integrated with official national accounts produced by Statistics New Zealand. Expenditure on international airfares is included in the TSA. The TSA also includes expenditure of international students studying in New Zealand for less than 12 months."),
         tags$p("The IVS is a quarterly survey, also from Statistics New Zealand. It is derived 'bottom-up', surveying international visitors about their expenditure during their visit in New Zealand. It includes any expenses that have been pre-paid prior to arriving. The IVS does not include expenditure on international airfares or expenditure of international students."),
         tags$p("The TSA is the most comprehensive and rigorous expenditure estimates for tourism. However, data is not timely, with provisional figures available for the years ended March 2013-15, and detailed results for 2012. The IVS provides more regular, timely results on a quarterly basis, but is less comprehensive and rigorous.")
      )
   ),
   h3("Feedback"),
   div(
      tags$p("Have any feedback? Please send any feedback on the NZ Tourism Dashboard to",
             tags$a(href = "mailto:TR_SharedMailbox@mbie.govt.nz", "TR_SharedMailbox@mbie.govt.nz")),
      tags$p("Please mention the following in the subject:"),
      tags$ul(
         tags$li("The name of the app (NZ Tourism Dashboard)"),
         tags$li("Which tab/sub-tab you are referring (if applicable)"),
         tags$li("e.g. NZ Tourism Dashboard: Overview - International Visitor Spend")
      ),
      tags$p("Then details of feedback in the body of the email.")
   )
)
