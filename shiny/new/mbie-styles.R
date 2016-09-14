mbie_header =
   ## Adds a basic MBIE header, that will only display
   ##  if the page is not embedded as an iframe.
   ## Requires: www/mbie-logo.png
   function() div(id = "mbie-header",
      div(class = "mbie-topbar"),
      div(class = "mbie-brand",
         tags$a(class = "mbie-brand", href = "http://www.mbie.govt.nz/",
                title = "Ministry of Business, Innovation & Employment home page.",
                tags$img(src = "mbie-logo.png",
                         alt = "Ministry of Business, Innovation & Employment"))
      )
   )
