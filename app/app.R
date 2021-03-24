# load the tooltips
source("R/tooltips.R")

# increase maximum upload size
options(shiny.maxRequestSize = 30*1024^2)

# run app
shinyApp(ui=ui, server=server)