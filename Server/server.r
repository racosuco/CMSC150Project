library(shiny)
library(shinydashboard)
source("Server/script/PolynomialRegression.r")
source("Server/script/QSI.r")

server <- function(input, output) {
  output$PolyReg <- renderTable({
    req(input$prfile, input$prdegree, input$prestimate)
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
    output$funcEstimate = renderText({
      ans = PRList$polynomial_function(input$prestimate)
      retString = paste("f(", input$prestimate, "): ", ans, sep = "")
      return(retString)
    })
    return(listOfXY)
  })
  
  output$QSI <- renderTable({
    req(input$qsifile, input$qsiestimate)
    tryCatch(
      {
        listOfXY = read.csv(input$qsifile$datapath, header = FALSE, sep = ",")
        colnames(listOfXY) = c("x", "y")
      },
      error = function(e){
        stop(safeError(e))
      }
    )
    listOfXY = listOfXY[order(listOfXY[[1]]),]
    QSIList = QSI(listOfXY)
    x = c(listOfXY[[1]])
    output$funcStringList = renderPrint({
      tempList = QSIList$polStringList
      for(i in 1:(length(x)-1)){
        print(paste(x[i], " to ", x[i+1]), quote = FALSE)
        cat(paste(tempList[i], "\n\n")) 
      }
    })
    output$qsiEstimate = renderText({
      index = 0
      for(i in 1:(length(x)-1)){
        if(input$qsiestimate >= x[i] && input$qsiestimate <= x[i+1]){
          index = i
        }
      }
      if(index == 0){
        return("Value of x not within bounds!")
      } else{
        ans = QSIList$polFuncList[[index]](input$qsiestimate)
        retString = paste("f(", input$qsiestimate, "): ", ans, sep = "")
        return(retString)
      }
    })
    return(listOfXY)
  })
}