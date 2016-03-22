## Public version of the source code
This repo is a public version of the source code for the [New Zealand Tourism Dashboard](https://mbienz.shinyapps.io/tourism_dashboard_prod/). The Dashboard is built with R, JavaScript and Shiny. If you got this far, we have to assume you can look after yourself with R and navigating this GitHub repo; we can’t give detailed guidance or help. We won’t be accepting pull requests, and the issues tab in this repo will not be closely monitored.

The app itself is contained in the `./shiny/new/` folder and can be run from the root directory with:

```R
shiny::runApp("shiny/new")
```

The `integrate.R` script and most of the code in the `./prep/` folder will not run because it depends on databases only available in the MBIE domain. It is included here for transparency.

This code is published under a [Creative Commons license](http://creativecommons.org/licenses/by/3.0/nz/). While all care and diligence has been used, MBIE gives no warranty it is error free and will not be liable for any loss or damage suffered by the use directly, or indirectly, of the information.

# New Zealand Tourism Dashboard
Consolidated app that gives one-stop access by theme (rather than data collection) to the most interesting tourism data. The repository is organised into [prep](prep/) (run via [integrate](integrate.R)) and [shiny](shiny/) folders.

## integrate and prep
### integrate
Refer to comments within [the file](integrate.R).

### prep
The scripts in `prep` are run via `integrate` to produce the required data files for the shiny app.

The process is slightly different than usual, as the prep scripts contain one or more functions, rather than bare code.

Sourcing a prep script does not run the prep code, it simply loads the function(s). The function(s) must then be called, passing along the required arguments (in all cases `dataset_names`, refer to the **Define Datasets** section of [integrate.R](integrate.R)), e.g.

```R
sourceprep("economic_contribution.R")
economic_contribution("TSA")
```

Doing this results in a cleaner workspace environment and clearer scoping rules reducing conflicts and other issues arising from possible cross-contamination.

The [old prep scripts](prep/old/) are retained for reference purposes but are not used in the new shiny app.

## shiny
The shiny app is in [shiny/new/](shiny/new/). It can be broadly organised into `ui`, `server`, `shared load` and `supporting` files.

### ui
The following files can be thought to be `ui` files: [ui.R](shiny/new/ui.R), [ui_pack.R](shiny/new/ui_pack.R), [ui_doctabs.R](shiny/new/ui_doctabs.R), [mbie-styles.R](shiny/new/mbie-styles.R), [mbie-styles.css](shiny/new/mbie-styles.css) and [tdstyles.css](shiny/new/tdstyles.css).

All files in the [www folder](shiny/new/www/) are also loaded through `ui.R` via `head`, but are better thought to be `supporting` files, rather than strictly `ui` files.

#### ui.R
For this app, the `ui.R` is only used to specify the `head` and the broad tab layout of the app. The contents of each tab is populated by calling a function defined in `ui_pack.R`. This separation makes it easier to rearrange or rename the tabs, and to find the content code for a specific tab (by searching for the function name in `ui_pack.R`).

#### ui_pack.R
Contains a number of functions producing the contents of each tab. Each function returns a set of HTML shiny tags via `tagList`. If it helps understanding, the contents of `tagList` in each function could be copied and pasted into `ui.R` instead of the function call, and the result will be identical (technically, there would be differences in scoping, but this shouldn't lead to any differences here), e.g.

In `ui.R`
```R
tabPanel("International Visitor Arrivals",
   oiva()
),
```

In `ui_pack.R`
```R
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
   )
)
```

The following in `ui.R` would lead to the same output:

```R
tabPanel("International Visitor Arrivals",
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
   )
),
```

#### ui_doctabs.R
Equivalent to `ui_pack.R` but containing the functions for the document/word-heavy tabs. Split out so such documents can be found and updated more easily, as well as making it easier to run spell-checkers.

#### mbie-styles
The mbie-styles files are used to give the shiny app a look consistent with the rest of the MBIE website. To function they also require [mbie-logo.png](shiny/new/www/mbie-logo.png) and the [fonts folder](shiny/new/www/fonts).

[mbie-styles.R](shiny/new/mbie-styles.R) also contains a function for adding a smart-MBIE-banner to the top of the shiny app, that will only display if the shiny app is not embedded in an `iframe`. This enables the same shiny app to be used for both embedding in an MBIE page, and also as a standalone page that retains some MBIE signage.

#### tdstyles
Contains all other required css.

### server
The `server` files consist of: [server.R](shiny/new/server.R) and [server_pack.R](shiny/new/server_pack.R).

Conceptually they work like `ui.R` and `ui_pack.R`, though mechanically there are differences due to the different scoping rules at play.

`ui` involves calling a function to define the user interface, and this call is made in the same scope (environment) that the various supporting functions are defined (that is, the same scope that `source(...)` is run). This means we do not have to worry about passing any arguments, as all the functions reside in the same scope.

`server` involves defining a function, which accepts the server arguments (`input`, `output` and `session`), and calls the various server-side code within this function. This function is defined in the same scope as the various supporting functions, but the arguments (`input`, `output` and `session`) reside within the scope of the server function, which the supporting functions cannot directly see.

To achieve the same effect as the `ui` pair (that is, `pack` function contents are equivalent to copy+paste), we grab the scope (environment) within the server function (`env_serv = environment()`) and pass this to each `server_pack` function we call. Each `server_pack` function then takes this environment and wraps its contents inside a `with(env_serv, {...})`.

Unlike most other variables in R, where passing a variable as an argument results in a copy of that variable within the function scope, passing an environment passes a reference, not a copy, thus any changes made to the environment inside the `with(env_serv, {...})` will affect the environment itself, not a copy. For this reason, care is required where the same variable name is used in different `server_pack` functions as cross-contamination may occur. It may sometimes be necessary to wrap the inside of the `with` with a `local` (or `observe`), e.g. `with(env_serv, local({...}))`), to create a local environment (which is nested within `env_serv` and thus has access to its contents, while keeping any work variables to itself).

### shared load
The `shared_load` consists of a single file, [shared_load.R](shiny/new/shared_load.R), which contains all loading work required for both `ui` and `server`. These include:

* Loading packages
* Loading supporting functions used in both `ui` and `server`
* Loading datasets and manipulation of these datasets
* Definitions of categories (typically related to the loaded datasets)

By putting these into a single shared script, we reduce duplication and ensure that both `ui` and `server` are using the same datasets and definitions.

### supporting
All files not covered in the above categories belong here, the most significant supporting files are: [helper_funcs.R](shiny/new/helper_funcs.R), [plots_funcs.R](shiny/new/plots_funcs.R) and [rs_funcs.R](shiny/new/rs_funcs.R).

* `helper_funcs` contain a vast and diverse range of functions. Refer to the in-file comments for more documentation.
* `plots_funcs` are like `helper_funcs` but are strictly for creating plots.
* `rs_funcs` are functions specific to the creation of Regional Summaries. They have their own file as Regional Summaries was a standalone shiny app before it was migrated into the Tourism Dashboard.

In addition a number of JavaScript libraries are used, these are all found in the [www folder](shiny/new/www/):

* `dygraph-extra.js` is used to add an export to png functionality to dygraphs. The script used is a modified version of the [original](http://cavorite.com/labs/js/dygraphs-export/).
* `jszip.min.js` ([website](https://stuk.github.io/jszip/)) is used by `dygraph-extra.js` to automatically zip together multiple png files to download multiple plots at the same time.
* `accounting.min.js` ([website](http://openexchangerates.github.io/accounting.js/)) provides a number of functions for formatting numbers and currency. It is used to format numbers in dygraphs.
* `jquery-ui-1-11-4.min.js` adds [jQuery UI](http://jqueryui.com/). Shiny comes with a limited set of jQuery UI already, but this adds the entire set. Currently [tooltip](http://jqueryui.com/tooltip/) is used to add better tooltips, and [accordion](http://jqueryui.com/accordion/) is used for the show/hide inputs feature, as well as the examples in the front/about page.
* `iframeResizer.contentWindow.min.js` ([website](http://davidjbradshaw.github.io/iframe-resizer/)) makes it possible to automatically resize iframe height to fit the shiny app contents. This script is only part of the code for this to work, code must also be run in the page that has the iframe. This latter part is handled by MBIE web services.

This work is licensed under a [Creative Commons Attribution 3.0 New Zealand License](http://creativecommons.org/licenses/by/3.0/nz/).
