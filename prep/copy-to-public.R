
if(!"tourism-dashboard-public" %in% dir("..")){
   stop("You need to have a repository called 'tourism-dashboard-public' on your
drive hosting your main working 'tourism-dashboard' (or whatever you called it)
repository.  And that repository needs to have: 
  https://github.com/nz-mbie/tourism-dashboard-public.git
set up as a remote it can push to.")
}

# get all the files to copy over
all_files <- dir(recursive = TRUE)

# remove fonts and logos
all_files <- all_files[!grepl("Gustan", all_files)]
all_files <- all_files[!grepl("mbie-logo", all_files)]
all_files <- all_files[!grepl("bach-pano-banner", all_files)]

# add to files that begin with dots and were hence ignored
all_files <- c(all_files, ".gitignore", ".Rprofile")


dir.create("../tourism-dashboard-public/prep")
dir.create("../tourism-dashboard-public/shiny")
dir.create("../tourism-dashboard-public/shiny/new")
dir.create("../tourism-dashboard-public/shiny/new/www")
dir.create("../tourism-dashboard-public/shiny/new/data")
dir.create("../tourism-dashboard-public/shiny/new/www/fonts")

file.copy(all_files, to = paste0("../tourism-dashboard-public/", all_files), 
          overwrite = TRUE)

## Add some text to public README
rmpath = "../tourism-dashboard-public/README.md"
rmori = readLines(rmpath)
rmnew = c(
   "## Public version of the source code",
   "This repo is a public version of the source code for the [New Zealand Tourism Dashboard](https://mbienz.shinyapps.io/tourism_dashboard_prod/). The Dashboard is built with R, JavaScript and Shiny. If you got this far, we have to assume you can look after yourself with R and navigating this GitHub repo; we can’t give detailed guidance or help. We won’t be accepting pull requests, and the issues tab in this repo will not be closely monitored.",
   "",
   "The app itself is contained in the `./shiny/new/` folder and can be run from the root directory with:",
   "",
   "```R",
   "shiny::runApp(\"shiny/new\")",
   "```",
   "",
   "The `integrate.R` script and most of the code in the `./prep/` folder will not run because it depends on databases only available in the MBIE domain. It is included here for transparency.",
   "",
   "This code is published under a [Creative Commons license](http://creativecommons.org/licenses/by/3.0/nz/). While all care and diligence has been used, MBIE gives no warranty it is error free and will not be liable for any loss or damage suffered by the use directly, or indirectly, of the information.",
   "",
   rmori
)
writeLines(rmnew, rmpath)
