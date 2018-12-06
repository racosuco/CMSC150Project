library(shiny)
library(shinydashboard)
library(rhandsontable)
source("Server/script/PolynomialRegression.r")
source("Server/script/QSI.r")
source("Server/script/Simplex.r")

plants = c("Denver", "Phoenix", "Dallas")
DF <- as.data.frame(matrix(0, ncol = 7, nrow = 5))
DF[,1] = c(plants, "Total", "Demand")
DF[5,] = c("Demand", 180, 80, 200, 160, 220, "")
colnames(DF) <- c("Plants","W1", "W2", "W3", "W4", "W5", "Total")

DF2 <- as.data.frame(matrix(0, ncol = 7, nrow = 4))
DF2[1,] = c(0, 10, 8, 6, 5, 4, 310)
DF2[2,] = c(0, 6, 5, 4, 3, 6, 260)
DF2[3,] = c(0, 3, 4, 4, 5, 9, 280)
DF2[,1] = c(plants, "Shipping")
colnames(DF2) <- c("Plants", "W1 Cost", "W2 Cost", "W3 Cost", "W4 Cost", "W5 Cost", "Supply")
DF2[4,7] = ""

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
      if(is.na(PRList)){
        return("Error")
      }
      return(PRList$polynomial_string)
    })
    output$funcEstimate = renderText({
      if(is.na(PRList)){
        return("Degree out of bounds!")
      }
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
  
  output$Simplex <- renderRHandsontable({
    rhandsontable(DF, width = 550, height = 300) %>%
      hot_col(c(1, 7), readOnly = TRUE) %>%
      hot_row(c(4), readOnly = TRUE) %>%
      hot_cell(6, "Total", readOnly = TRUE)
  })
  
  output$Simplex2 <- renderRHandsontable({
    rhandsontable(DF2, width = 550, height = 300) %>%
    hot_col(c(1:6), readOnly = TRUE) 
  })
}