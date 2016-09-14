source("shared_load.R")
source("ui_doctabs.R")
source("ui_pack.R")
source("mbie-styles.R")
source("ga_Rfuncs.R")

shinyUI(
   dashboardPage("New Zealand Tourism Dashboard", thead = tagList(
      tags$head(
         HTML('<link rel="canonical" href="http://tourismdashboard.mbie.govt.nz/"/>'),
         includeCSS("mbie-styles.css"),
         includeCSS("tdstyles.css"),
         tags$script(src = "jszip.min.js"),
         tags$script(src = "accounting.min.js"),
         tags$script("accounting.settings.currency.precision = 0;"),
         dyExtraHead(),
         tags$script(src = "jquery-ui-1-11-4.min.js"),
         ## Add jQuery UI tooltips
         ## Use: have class "jui-tip" and
         ##      title attribute = tooltip message
         ## e.g. tags$div(class = "jui-tip", title = "Tooltip Message", radioButtons(...))
         tags$script('$(function(){$(".jui-tip").tooltip();});'),
         ## Use jQuery UI accordion for nice looking show/hide inputs feature
         tags$script('$(function(){$("div.divinput").accordion({
            collapsible: true,
            heightStyle: "content"
         });});'),
         ## Disable DataTable error reporting
         tags$script('$.fn.dataTableExt.sErrMode = "throw";'),
         includeScript("ga_tracker.js"),
         # tags$script("galog = []; ga = function(){galog.push(arguments);};"),
         ga_common(),
         ## iframe resizer code to dynamically adjust iframe height
         ## also requires work by mbie web services to work
         tags$script(src = "iframeResizer.contentWindow.min.js")
      ),
      ## Place mbie_header inside a container-fluid for correct positioning
      div(class = "container-fluid", mbie_header())
   ),
   tabPanel("Start",
      frontp()
   ),
   navbarMenu("Overview",
      tabPwT("Economic Contribution",
         oeco()
      ), tabPwT("International Visitor Arrivals",
         oiva()
      ), tabPwT("International Visitor Spend",
         oivs()
      ), tabPwT("International Visitor Arrivals vs Spend",
         oivas()
      )
   ),
   navbarMenu("Regions",
      tabPwT("Regional Summaries",
         rs_all()
      ),
      tabPwT("Visitor Nights",
         racc()
      ),
      tabPwT("Tourism Product",
         rins()
      ),
      tabPwT("Tourism as a percentage of Regional GDP",
         rgdp()
      )
   ),
   navbarMenu("Industry",
      tabPwT("Commercial Accommodation",
         iacc()
      ), tabPwT("Attractions and Activities",
         iact()
      ),
      tabPwT("Business Demography",
         ibdem()
      ),
      tabPwT("Business Events",
        icas()
      ),
      tabPwT("Spend by Visitor Market",
        isvm()
      )
   ),
   navbarMenu("Visitor Markets",
      tabPwT("Visitor Spend by Origin",
         vmorigin()
      ),
      tabPwT("Visitor Spend by Destination",
         vmdest()
      ),
      tabPwT("Accommodation Used",
         vmacc()
      )
   ),
   navbarMenu("Economic Context",
      tabPwT("Exchange Rates",
         gcforex()
      ),
      tabPwT("Tourism-related Share Prices",
         ecshare()
      ),
      tabPwT("Main Market GDP",
         ecgrof()
      )
   ),
   tabPwT("Help",
      helpp()
   )
))
