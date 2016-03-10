
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
