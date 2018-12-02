library(shiny)
source("UI/ui.r")
source("Server/server.r")

shinyApp(ui = ui, server = server)