library(shiny)
library(shinydashboard)
library(rhandsontable)
source("Server/script/PolynomialRegression.r")
source("Server/script/QSI.r")
source("Server/script/Simplex.r")

plants = c("Denver", "Phoenix", "Dallas")
compute <- as.data.frame(matrix(0, ncol = 6, nrow = 5), stringsAsFactors = FALSE)
compute[5,] = c(180, 80, 200, 160, 220, 0)
colnames(compute) <- c("W1", "W2", "W3", "W4", "W5", "Total")
rownames(compute) <- c(plants, "Total", "Demand")

shippingCost <- as.data.frame(matrix(0, ncol = 6, nrow = 4), stringsAsFactors = FALSE)
shippingCost[1,] = c(10, 8, 6, 5, 4, 310)
shippingCost[2,] = c(6, 5, 4, 3, 6, 260)
shippingCost[3,] = c(3, 4, 5, 5, 9, 280)
colnames(shippingCost) <- c("W1 Cost", "W2 Cost", "W3 Cost", "W4 Cost", "W5 Cost", "Supply")
rownames(shippingCost) <- c(plants, "ShipCost")

server <- function(input, output, session) {
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
  
  values = reactiveValues(compute = compute, shippingCost = shippingCost, tables = c())
  
  observe({
    if(!is.null(input$compute)){
      values$compute = hot_to_r(input$compute)
    }
  })
  
  observe({
    if(!is.null(input$shippingCost)){
      values$shippingCost = hot_to_r(input$shippingCost)
    }
  })
  
  output$compute = renderRHandsontable({
      values$compute[1, "Total"] = sum(values$compute[1,1:5])
      values$compute[2, "Total"] = sum(values$compute[2,1:5])
      values$compute[3, "Total"] = sum(values$compute[3,1:5])
      values$compute["Total", "W1"] = sum(values$compute[1:3, "W1"])
      values$compute["Total", "W2"] = sum(values$compute[1:3, "W2"])
      values$compute["Total", "W3"] = sum(values$compute[1:3, "W3"])
      values$compute["Total", "W4"] = sum(values$compute[1:3, "W4"])
      values$compute["Total", "W5"] = sum(values$compute[1:3, "W5"])
      values$compute["Total","Total"] = sum(values$compute[1:3, "Total"])
      rhandsontable(values$compute) %>%
      hot_row(c(1:4), readOnly = TRUE) %>%
      hot_col(c(6), readOnly = TRUE)
  })
  
  output$shippingCost = renderRHandsontable({
    vecOfVarM = t(as.matrix(values$shippingCost[1:3, 1:5]))
    vecOfVar = as.numeric(as.vector(vecOfVarM))
    vecOfSupply = as.numeric(unname(unlist(values$shippingCost[1:3,"Supply"])))
    vecOfDemand = as.numeric(unname(unlist(values$compute["Demand",1:5])))
    vecOfDemand = vecOfDemand * -1
    listOfAnswer = initMatrix(vecOfVar, vecOfSupply, vecOfDemand)
    tempMatrix = matrix(listOfAnswer$vectorValues, ncol = 5, nrow = 3, byrow = TRUE)
    values$compute[1:3, 1:5] = as.vector(tempMatrix) 
    values$shippingCost["ShipCost", "W1 Cost"] = values$shippingCost[1:3, "W1 Cost"] %*% values$compute[1:3, "W1"]
    values$shippingCost["ShipCost", "W2 Cost"] = values$shippingCost[1:3, "W2 Cost"] %*% values$compute[1:3, "W2"]
    values$shippingCost["ShipCost", "W3 Cost"] = values$shippingCost[1:3, "W3 Cost"] %*% values$compute[1:3, "W3"]
    values$shippingCost["ShipCost", "W4 Cost"] = values$shippingCost[1:3, "W4 Cost"] %*% values$compute[1:3, "W4"]
    values$shippingCost["ShipCost", "W5 Cost"] = values$shippingCost[1:3, "W5 Cost"] %*% values$compute[1:3, "W5"]
    values$shippingCost["ShipCost", "Supply"] = sum(values$shippingCost["ShipCost", 1:5])
    values$tables = listOfAnswer$matrixIterations
    
    updateSelectInput(session, "tableau", label = "Select Iteration", choices = c("Hide", c(1:length(listOfAnswer$matrixIterations))))

    rhandsontable(values$shippingCost) %>%
    hot_row(c(4), readOnly = TRUE)
  })
  
  output$tables = renderPrint({
    if(input$tableau != "Hide"){
      return(values$tables[[as.integer(input$tableau)]])
    }
    return(NA)
  })
}