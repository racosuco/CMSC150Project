library(shiny)
library(shinydashboard)
source("Server/script/PolynomialRegression.r")
source("Server/script/QSI.r")

server <- function(input, output) {
  output$PolyReg <- renderTable({
    req(input$prfile, input$prdegree)
    tryCatch(
      {
        listOfXY = read.csv(input$prfile$datapath, header = FALSE, sep = ",")
        colnames(listOfXY) = c("x", "y")
      },
      error = function(e){
        stop(safeError(e))
      }
    )

    PRList = PolynomialRegression(input$prdegree, listOfXY)
    output$funcString = renderText({
      return(PRList$polynomial_string)
    })
    return(listOfXY)
  })
}