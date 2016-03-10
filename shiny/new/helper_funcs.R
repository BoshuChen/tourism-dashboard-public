mbie.cols2 =
   ## mbie.cols that supports more than 7 colours
   function(x = 1:7){
      MBIE.cols = structure(
         c("#006272", "#97D700", "#00B5E2", "#753BBD", "#DF1995", "#FF6900", "#FBE122"),
         .Names = c("Teal", "Green", "Blue", "Purple", "Pink", "Orange", "Yellow")
      )
      if (x[1] == "Duo1")
         x <- 1:2
      if (x[1] == "Trio1")
         x <- 1:3
      if (x[1] == "Duo2")
         x <- 2:3
      if (x[1] == "Trio2")
         x <- 3:5
      if (x[1] == "Duo3")
         x <- 4:5
      if (x[1] == "Trio3")
         x <- c(4, 6:7)
      if (x[1] == "Duo4")
         x <- 6:7
      if (x[1] == "Duo5")
         x <- c(4, 7)
      if(max(x) <= 7 || mode(x) == "character")
         as.vector(MBIE.cols[x])
      else
         colorRampPalette(MBIE.cols)(max(x))[x]
   }

# Create a vector of the Ministry of Tourism colours
Tourism.cols <- c(
	Forest = "#3D4721FF",
	Grass = "#A8B50AFF",
	Southerly="#5C788FFF",
	Volcano="#B09E0DFF",
	Koura="#D4470FFF",
	Merino="#C2C4A3FF",
	Moss= "#5E7803FF",
	Pohutukawa="#AD2624FF",
	Sunrise="#D48500FF",
	CityLights ="#EDBD3DFF",
	SouthernCross="#265787FF",
	WineCountry="#61384DFF",
	Flax="#708270FF",
	Ocean="#A8C4C4FF",
	RiverStone="#ADABA6FF",
	Waka="#826E59FF",
	CabbageTree="#A8AD70FF",
	Sky="#94B5E0FF")

# A function tourism.cols() for easy reference to the vector Tourism.cols 
tourism.cols <- function(x=c(1,2,3,5,12,6)){
	# function to return in vector format a subset of the Ministry of Tourism's 2007
	# palette of colors.  By default returns 6 colours that form a nice set
	# for most plots.  Note that normally the 4th colour (Volcano) is too similar
	# to the 2nd (Grass) for use in plots.
	if(x[1]=="Primary") x<-1:6
	if(x[1]=="Supporting") x<-7:18
	if(x[1]=="All") x <-1:18
	if(x[1]=="Pale")x <- 13:18
	if(x[1]=="Alternating") x <- rep(c(1,6,4,8,2,16,3,5,15,18,9,14,7,12,11,17,10,13),6)
	as.vector(Tourism.cols[x])
}

wa2wha =
   ## Recursively checks through the dataset, swapping any
   ##  instances of Wanganui to Whanganui
   ## If check.characters = FALSE, only checks and swaps factors
   ## If check.characters = TRUE, also checks characters, but this takes considerably longer
   function(x, check.characters = FALSE){
      if(is.list(x)){
         for(i in 1:length(x))
            if(!is.null(x[[i]]))
               x[[i]] = wa2wha(x[[i]], check.characters = check.characters)
      } else{
         if(is.factor(x)){
            if(length(grep("Wanganui", levels(x), fixed = TRUE)) > 0)
               levels(x) = gsub("Wanganui", "Whanganui", levels(x), fixed = TRUE)
         } else{
            ## Only check characters if argument is TRUE
            ## Since check is much more costly
            if(check.characters == TRUE && is.character(x))
               if(length(grep("Wanganui", x, fixed = TRUE)) > 0)
                  x = gsub("Wanganui", "Whanganui", x, fixed = TRUE)
         }
      }
      x
   }

padxts =
   ## Adds padding of NA before and after the data of the given xts
   ## This creates some white-space margins on either side of the data
   ##  making is easier to see the edge values.
   ## The amount of padding is determined by the ideal padding (ipad)
   ##  padding is {nrow(xts) * ipad} on either side of the data
   ## Default is 4%, which is what R does to its graphs by default.
   function(curxts, ipad = 0.04){
      xdates = index(curxts)
      padDist = diff(range(xdates)) * 0.04
      padMat = matrix(as.numeric(NA), nrow = 1, ncol = ncol(curxts))
      colnames(padMat) = names(curxts)
      padTop = xts(padMat, order.by = first(xdates) - padDist)
      padBot = xts(padMat, order.by = last(xdates) + padDist)
      rbind(padTop, curxts, padBot)
   }

paldy =
   ## Function for computing consistent mbie colours for use by dygraphs
   ## Arguments:
   ##  curnames - vector containing currently selected variables
   ##  allnames - vector of all variables
   function(curnames, allnames){
      basepal = mbie.cols2(seq(length = length(allnames)))
      names(basepal) = allnames
      basepal[curnames]
   }

datasource =
   ## Old datasource function, accepts characters and sticks them into a list
   ## Where possible, `datasource2` is preferred, since it will update automatically
   function(...){
      dlist = list(...)
      do.call(tags$ul, c(list(class = "datasource"), lapply(dlist, tags$li)))
   }
datasource2 =
   ## Retrieve metadata stored in `sourcedat`
   ##  to generate appropriate data source text.
   ## Can be passed additional datasets via `...`, from which
   ##  only the "dataset" metadata will be taken.
   ## See integrate.R for what kind of metadata
   ##  is stored in data files (via the `addmeta` function),
   ##  and the dataset definitions stored in `defdata`
   function(sourcedat, ...){
      dsets = attr(sourcedat, "dataset")
      dlist = list(...)
      for(i in seq(length = length(dlist)))
         dsets = c(dsets, attr(dlist[[i]], "dataset"))
      ddate = attr(sourcedat, "date")
      dli = lapply(dsets, function(x){
         curdef = defdata[[x]]
         with(curdef, tags$li(
            tags$a(long, href = URL, target = "_blank"), "-", provider
         ))
      })
      do.call(tags$ul, c(list(class = "datasource"), dli, list(tags$li("Data Retrieved:", ddate))))
   }

front_ex =
   ## Wrapper for constructing help examples for the Front page
   ## Arguments
   ##  exq   - The title, usually a question
   ##  extab - The tab to look at to answer question
   ##  exans - (optional) An example answer to the question
   function(exq, extab, exans = NULL) tagList(
      tags$h4(exq),
      tags$div(
         tags$p(tags$span(class = "extab", "Tab to Use:"), extab),
         if(!is.null(exans)) tags$p(tags$span(class = "exans", "Example answer:"), exans) else NULL
      )
   )

tabTitle =
   ## Creates an appropriately styled title
   function(x)
      h3(class = "tabTitle", x)
tabDesc =
   ## Creates an appropriately styled description
   ## If NA, returns NULL
   function(x)
      if(!is.na(x)) tags$p(class = "tabDesc", x) else NULL
tabPwT =
   ## tabPanel with Title (using tabTitle)
   ## Also searches for a match in `tabdesc`
   ##  (a named vector of descriptions found in "ui_doctabs.R")
   ## If one is found, adds the description below the title
   function(title, ...){
      tabPanel(title,
         div(class = "tabTitlePanel",
            tabTitle(title),
            tabDesc(tabdesc[title][[1]]),
            div(class = "tabTitlePanel-end")
         ),
         ...
      )
   }

ggvisTitle =
   ## Create an HTML title for ggvis plots
   function(...) renderUI({
      div(class = "ggvis-title", paste0(...))
   })

rebaseIndex =
   ## Rebases/rescales the index based on the past n periods from baserow
   ## Uses `apply`, so if dat is a data.frame, it is coerced to a matrix
   ## Result will always be a matrix
   function(dat, baserow, n = 12) apply(dat, 2, function(x){
      baseval = mean(x[(baserow - (n - 1)):baserow])
      if(baseval == 0) baseval = NA
      x/baseval * 100
   })

liststr =
   ## Given a character vector, creates a string
   ##  appropriate for use as a title
   ## e.g.  c("A", "B")           --> "A and B"
   ## e.g.2 c("A", "B", "C", "D") --> "A, B and 2 more"
   function(complist){
      if(length(complist) <= 2){
         curliststr = paste(complist, collapse = " and ")
      } else{
         curliststr = paste(
            paste(complist[1:2], collapse = ", "),
            "and", length(complist) - 2, "more"
         )
      }
      curliststr
   }

obsrollylab =
   ## Given obsroll, which is one of "Both", "Obs" or "RA
   ## Construct an appropriate y label
   function(obsroll, metric) switch(obsroll,
      Both = paste("Actual and 12-month rolling average", metric),
      Obs = metric,
      RA = paste("12-month rolling average", metric)
   )

dashboardPage =
   ## Modified navbarPage from shiny
   ## Cuts bloat and enables use of tags$head with `thead`
   function(title, ..., id = "dashboard", thead = NULL, header = NULL, footer = NULL, windowTitle = title){
      pageTitle = title
      navbarClass = "navbar navbar-default"
      tabs = list(...)
      tabset = shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id)
      containerDiv = div(class = "container", div(class = "navbar-header", 
         span(class = "navbar-brand", pageTitle)), tabset$navList)
      contentDiv = div(class = "container-fluid")
      if(!is.null(header))
         contentDiv = tagAppendChild(contentDiv, div(class = "row", header))
      contentDiv = tagAppendChild(contentDiv, tabset$content)
      if(!is.null(footer)) 
         contentDiv = tagAppendChild(contentDiv, div(class = "row", footer))
      bootstrapPage(title = windowTitle, thead,
                    tags$nav(class = navbarClass, role = "navigation", containerDiv),
                    contentDiv)
   }

selectInputF =
   ## A wrapper for selectInput with selectize = FALSE
   function(inputId, label, choices, selected = NULL)
      shiny:::selectInput(inputId, label, choices, selected, selectize = FALSE)

checkboxComboInput =
   ## A combo input that presents a Checkbox Group Input
   ##  with a "Select all" checkbox
   ## This functions goes in ui.R while
   ##  `checkboxComboServer` should go in server.R
   ##
   ## Arguments:
   ##  Simplified versions of the arguments to `checkboxGroupInput`
   ##  Only difference is that `selected` is now a logical
   ##   If TRUE, all choices selected
   ##   If FALSE (default), first choice is selected
   ##
   ## Example:
   ## = ui.R =
   ## checkboxComboInput("compList", "Choose Products:", all_levels$Product)
   ## = server.R =
   ## checkboxComboServer("compList", all_levels$Product, input, session)
   function(inputId, label, choices, selected = FALSE){
      label = shiny:::controlLabel(inputId, label)
      
      chkboxAll = div(class = "checkbox checkboxcombo-all", tags$label(
         tags$input(id = paste0(inputId, "AllChk"), type = "checkbox"),
         tags$span("Select all")
      ))
      if(selected) chkboxAll$children[[1]]$children[[1]]$attribs$checked = "checked"
      
      choices = shiny:::choicesWithNames(choices)
      selected = if(selected == TRUE) choices else choices[1]
      chkboxGroup = shiny:::generateOptions(inputId, choices, selected, FALSE)
      
      tags$div(id = inputId,
         class = "form-group shiny-input-checkboxgroup shiny-input-container checkboxcombo",
         label, chkboxAll, chkboxGroup
      )
   }
checkboxComboServer =
   ## The function to place in server.R to provide the "Select all"
   ##  functionality to `checkboxComboInput`
   ##
   ## Arguments:
   ##  "input" and "session" come straight from the arguments to `shinyServer`
   ##    i.e. shinyServer(function(input, output, session){...})
   ##  "inputId" and "choices" should match those given to `checkboxComboInput`
   ##  "cdef" is the index of the choice to be selected when
   ##    select all is unticked.
   function(input, session, inputId, choices, cdef = 1) observe({
      if(input[[paste0(inputId, "AllChk")]] == TRUE)
         updateCheckboxGroupInput(session, inputId, selected = choices)
      else
         updateCheckboxGroupInput(session, inputId, selected = choices[cdef])
   })
condComboServer =
   ## Add a checkboxComboServer for each checkboxComboInput
   ##  generated by condInputs, in a single call.
   ## "cats" and "choices" come from the arguments to `condInputs`
   ## The for loop requires a `local` due to funny shiny business.
   function(input, session, inputId, cats, choices, cdef = 1) observe({
      for(i in 1:length(cats)) local({
         curcat = cats[i]
         curID = condID(inputId, curcat)
         curchoices = choices[[curcat]]
         checkboxComboServer(input, session, curID, curchoices, cdef)
      })
   })

condInputs =
   ## A wrapper to provide a systematic way to create conditional inputs
   ## This functions goes in ui.R while
   ##  `condInputServer` should go in server.R
   ##
   ## Arguments:
   ## -inputId-
   ## The base ID to be given to the inputs, as a character vector of length 1
   ##
   ## -cats-
   ## The input categories, of which one is shown at a given point
   ##  in time based on whether the conditions are met.
   ## Given as a character vector, possibly named.
   ## If named, these names are passed to the "label" argument
   ##  e.g. c("Domestic" = "dom", "International" = "int")
   ##
   ## -choices-
   ## A named list of choices corresponding to the categories provided in "cats"
   ##  e.g. list(dom = c("Auckland", "Wellington"),
   ##            int = c("UK", "US"))
   ##
   ## -type-
   ## The type of input, either a function or the name
   ##  of a function (as a character vector of length 1)
   ##  e.g.  "selectInputF"
   ##  e.g.2 "checkboxComboInput"
   ##
   ## -label-
   ## The labels passed to the inputs.
   ## By default this is generated automatically from
   ##  "names(cats)" if it exists, or "cats" itself.
   ## But custom labels could be provided here as a
   ##  character vector corresponding to "cats".
   ##
   ## -cond_simple- and -cond_full-
   ## Used to provide the conditions. Only one of these should be provided.
   ## See `condParse` for exact specifications.
   function(inputId, cats, choices, type, labels = NULL,
            cond_simple = NULL, cond_full = NULL, ...){
      if(is.null(labels)){
         if(!is.null(names(cats)))
            labels = names(cats)
         else
            labels = cats
      }
      labelfull = paste0("Choose ", labels, ":")
      
      ## Generate conditional panels
      condition = condParse(cats, cond_simple, cond_full, "js")
      compPanels = list()
      for(i in 1:length(cats)){
         curcat = cats[i]
         curlab = labelfull[i]
         curargs = list(
            inputId = condID(inputId, curcat),
            label = curlab,
            choices = choices[[curcat]]
         )
         curargs = c(curargs, list(...))
         compPanels[[curcat]] = conditionalPanel(condition[i], do.call(type, curargs))
      }
      
      do.call(tagList, compPanels)
   }

condInputServer =
   ## The function to place in server.R to easily grab the input value
   ##  provided by an active conditional input generated using `condInputs`
   ##
   ## Arguments:
   ##  "input" comes straight from the arguments to `shinyServer`
   ##    i.e. shinyServer(function(input, output, session){...})
   ##  "inputId", "cats" and "cond_simple"/"cond_full"
   ##    should match those given to `condInputs`
   function(input, inputId, cats, cond_simple = NULL, cond_full = NULL){
      ## Check conditions
      condition = condParse(cats, cond_simple, cond_full, "R")
      condchk = sapply(condition, function(x) eval(parse(text = x)))
      
      input[[condID(inputId, cats[condchk])]]
   }

condParse =
   ## A parser used by `condInputs` and `condInputServer`
   ##  to handle "cond_simple" and "cond_full" arguments
   ## Only one of "cond_simple" or "cond_full" should be provided.
   ## If both are provided, only "cond_simple" is used.
   ##
   ## -cond_simple-
   ## Used for simple conditions with a direct, one-to-one
   ##  correspondence from an input to the conditions.
   ## This is the inputId of an input (usually a `radioButtons`)
   ##  that will choose one of the conditional inputs.
   ## e.g. cond_simple = "compclass"
   ##  is interpreted to mean:
   ##   if(input$compclass == cats[1])
   ##   then <show input corresponding to cats[1]>
   ##
   ## -cond_full-
   ## Used for more complicated conditions.
   ## A named list corresponding to the categories provided in "cats"
   ##  each containing a named list where:
   ##  - the names correspond to the inputId of an input
   ##  - the contents correspond to the values of that input
   ##    for which the condition is satisfied.
   ## e.g. the following is roughly equivalent to cond_simple = "compclass"
   ##  cond_full = list(cats[1] = list(compclass = cats[1]))
   ## e.g.2 a case where the condition for "RTO" is met for
   ##   both "mi" and "mo" for input$compclass
   ##  and where the condition for "OriginDom" is met when
   ##   both input$compclass == "oo" and input$domint == "Domestic"
   ##  cond_full = list(RTO = list(compclass = c("mi", "mo")),
   ##                   OriginDom = list(compclass = "oo", domint = "Domestic"))
   function(cats, cond_simple = NULL, cond_full = NULL, type = "js"){
      ## Set key values to switch between JavaScript and R condition parsing
      subskey = if(type == "js") "." else "$"
      
      if(!is.null(cond_simple)){
         ## Simple conditions
         condition = paste0("input", subskey, cond_simple, "==", sapply(cats, deparse))
      } else{
         ## Complex conditions
         condition = NULL
         for(i in 1:length(cats)){
            curcat = cats[i]
            catconds = cond_full[[curcat]]
            subconds = NULL
            for(j in 1:length(catconds)){
               subconds[j] = paste0("input", subskey, names(catconds)[j], "==",
                                  sapply(catconds[[j]], deparse), collapse = "||")
            }
            condition[i] = paste0("(", subconds, ")", collapse = "&&")
         }
      }
      condition
   }

condID =
   ## Simple wrapper for removing spaces from cats for ID generation
   function(baseID, curcat)
      paste0(baseID, gsub(" ", "_", curcat))

tableCombo =
   ## Combination input of a table and download button for table
   function(inputId) tagList(
      downloadButton(paste0(inputId, "Down"), "Download table of values"),
      tags$div(tags$form(class = "well",
         dataTableOutput(inputId)
      ))
   )
tableComboDown =
   ## Handle download of Data Table
   function(curtab, filename){
      ## Tidy filename, not strictly necessary on Windows sytems
      filename = gsub(" ", "_", filename, fixed = TRUE)
      filename = gsub(",", "", filename, fixed = TRUE)
      downloadHandler(
         filename = paste0(filename, "_DownloadDate_", Sys.Date(), ".csv"),
         content = function(file) write.csv(curtab, file, row.names = FALSE),
         contentType = "text/csv"
      )
   }

getdat =
   ## A wrapper function that handles the task of getting the
   ##  required data subset, filtered, summed and re-factored as required
   ## Where a filtering condition specifies a Total for a variable which does
   ##  not have a Total level in "dat", the function will automatically sum
   ##  over the correct variables to compute a Total.
   ##
   ## Arguments:
   ##  dat - The full data, as a data.frame
   ##  xvar - The x variable, as a character vector of length 1
   ##          e.g. "Period" or "YEMar"
   ##  yvar - The y variable, as a character vector of length 1
   ##          e.g. "Spend"
   ##  compvar - The variable for comparison, as a character vector of length 1
   ##          e.g. "RTO" or "Industry"
   ##  flist - Arguments to pass to filter, in a more convenient format
   ##          e.g. list(Origin = input$compOrigin, Product = compList, RTO = compOne)
   ##             This is intepreted as "Origin == input$compOrigin"
   ##             Each variable should only have one condition
   ##              (given as a character vector of length 1)
   ##             Except the compvar variable, which can have a list
   ##              of conditions to check for, which is intepreted as
   ##             "Product == compList[1]|Product == compList[2]|..."
   function(dat, xvar, compvar, yvar, flist){
      library(dplyr)
      #################
      ## Handle Totals
      #################
      ## Check if any in flist have a Total
      ## For those that do, check if a Total exists in dat
      ## If it doesn't, need to compute Totals separately
      tname = character()
      for(curvar in names(flist)){
         it = grep("^Total", flist[[curvar]])
         if(length(it) > 0){
            if(!all(flist[[curvar]][it] %in% levels(dat[[curvar]]))){
               ## If it's the compvar, save the first Total
               ## Any other "Totals" are discarded, remaining compvars are kept
               ## If it's not compvar, simply remove it
               ## (leads to summation later, hence getting Totals)
               ## TODO: Handling of situations where some Totals exist in data
               ##       While others don't
               if(curvar == compvar){
                  tname = flist[[curvar]][it][1]
                  flist[[curvar]] = flist[[curvar]][-it]
               } else flist[[curvar]] = NULL
            }
         }
      }
      
      ##################
      ## Compute Totals
      ##################
      ## Only compute if there is a total compvar to do
      if(length(tname) > 0){
         ## Compile flist into a format appropriate for calling filter_
         ## But excluding compvar
         fdots = list()
         for(curvar in names(flist))
            fdots[[curvar]] = paste0(curvar, "==", sapply(flist[[curvar]], deparse), collapse = "|")
         fdots[[compvar]] = NULL
         
         ## Call filter_
         curdat = filter_(dat, .dots = fdots)
         
         ## Sum spend to get totals
         sum_args = list()
         sum_args[[yvar]] = paste0("sum(", yvar, ")")
         
         curtot = curdat %>%
            group_by_(.dots = xvar) %>%
            summarise_(.dots = sum_args)
         class(curtot) = "data.frame"
         
         ## Bind Total name and save final result to outtot
         bind_args = list()
         bind_args[[compvar]] = factor(tname, levels = c(tname, levels(dat[[compvar]])))
         outtot = do.call(cbind, c(list(curtot), bind_args))[,c(xvar, compvar, yvar)]
      } else outtot = NULL
      
      #####################
      ## Compute Remainder
      #####################
      ## Only compute if there are non-total compvars to do
      if(length(flist[[compvar]]) > 0){
         ## Compile flist into a format appropriate for calling filter_
         fdots = list()
         for(curvar in names(flist))
            fdots[[curvar]] = paste0(curvar, "==", sapply(flist[[curvar]], deparse), collapse = "|")
         
         ## Call filter_
         currem = filter_(dat, .dots = fdots)
         
         ## Sum over all columns not wanted in final output
         sum_args = list()
         sum_args[[yvar]] = paste0("sum(", yvar, ")")
         
         outrem = currem %>%
            group_by_(.dots = c(xvar, compvar)) %>%
            summarise_(.dots = sum_args)
         class(outrem) = "data.frame"
         outrem = outrem[,c(xvar, compvar, yvar)]
         
         ## Update factor levels if totals were computed
         if(length(tname) > 0)
            outrem[[compvar]] = factor(outrem[[compvar]], levels = c(tname, levels(dat[[compvar]])))
      } else outrem = NULL
      
      ###########
      ## Combine
      ###########
      
      if(!is.null(outtot) && !is.null(outrem)){
         outfinal = full_join(outtot, outrem, by = colnames(outtot))
      } else{
         if(is.null(outtot)) outfinal = outrem
         else outfinal = outtot
      }
      
      ## Refactor
      for(j in 1:ncol(outfinal))
         if(is.factor(outfinal[[j]]))
            outfinal[[j]] = factor(outfinal[[j]])
      outfinal
   }

casdat =
   ## A function for grabbing CAS data for the given datname
   ## Requires `all_cas.rda` to have been loaded
   ## Totals over all regions, does other processing based on the other arguments
   ## Separate as it is migrated from what used to be a standalone CAS app
   ##
   ## Arguments:
   ## -datname-
   ## Name of the dataset within CAS, see `names(all_cas)` for the full list
   ## e.g.
   ##  casdat("Count of events")
   ##
   ## -subsetpars-
   ## A method to grab only a subset of the columns of the data
   ## Can simply be a character vector of length 1, which is passed to grep
   ## Or it can be a list, with elements: "grep", "gsubpattern" and "gsubreplace"
   ##  each of these should have a character vector of length 1
   ##  grep is passed to grep to get columns
   ##  gsubpattern and gsubreplace is then applied on these columns to rename them
   ## e.g.
   ##  casdat("Total delegates by origin", subsetpars = "Total delegates")
   ## e.g.2
   ##  subsetpars = list(grep = "All events", gsubpattern = "All events with ", gsubreplace = "")
   ##  casdat("Event size for different event types", subsetpars = subsetpars)
   ##
   ## -scaling-
   ## A scaling multiplier to apply to the data, should be a numeric vector of length 1.
   ## e.g. to scale to a 000s
   ##  casdat("Delegate days", scaling = 1/1000)
   ## Note that the scaling is a multiplier, so this multiplies the data by 1/1000
   ##
   ## -percentage-
   ## Scales across the columns into a percentage
   ##  i.e. scaling is applied to each row, such that sum(row) = 100
   ## e.g.
   ##  casdat("Counts of event types", percentage = TRUE)
   ## Will return % share of each event type.
   function(datname, subsetpars = NULL, scaling = NULL, percentage = FALSE){
      cur_dat = all_cas[[datname]]
      dat_group = group_by(cur_dat, Period)
      var_names = colnames(cur_dat)[-(1:2)]
      sum_text = paste0("summarise(dat_group, ",
         paste0("`", var_names, "` = ", "sum(`", var_names, "`)", collapse = ", "),
      ")")
      dat_sum = eval(parse(text = sum_text))
      class(dat_sum) = "data.frame"
      dat_list = list(date = dat_sum[,1], num = dat_sum[,-1,drop=FALSE])
      if(!is.null(subsetpars)){
         if(is.list(subsetpars)){
            dat_list$num = dat_list$num[,grep(subsetpars$grep, names(dat_list$num)),drop=FALSE]
            colnames(dat_list$num) = gsub(subsetpars$gsubpattern, subsetpars$gsubreplace,
                                          colnames(dat_list$num))
         } else
            dat_list$num = dat_list$num[,grep(subsetpars, names(dat_list$num)),drop=FALSE]
      }
      if(!is.null(scaling)) dat_list$num = dat_list$num * scaling
      if(percentage) dat_list$num = t(apply(as.matrix(dat_list$num), 1, function(x) x/sum(x)*100))
      dat_list
   }
