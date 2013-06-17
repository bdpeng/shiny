library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  MtoD <- function(res, n = 21, conversion = 0.43, interpolated = 1, predicted = 1){
    
    if(any(is.na(res))){
      res <- na.omit(res)
    } else {
      res <- res
    }
    
    std.M <- sd(as.numeric(res))
    
    # R is column M in the spreadsheet
    for (i in 1:nrow(res)){
      if(i == 1){
        R <- rep(((1+res[i])^(1/n)-1),n)
        res.cum <- c(1, rep(NA,(n-1)), (1+Return.cumulative(res[1])))
        timestamp <- rep(index(res)[i], n)
      } else {
        tmp1 <- rep(((1+res[i])^(1/n)-1),n)
        tmp2 <- c(rep(NA,(n-1)),(1+Return.cumulative(res[1:i])))
        tmp3 <- rep(index(res)[i], n)
        R <- c(R,tmp1)
        res.cum <- c(res.cum, tmp2)
        timestamp <- c(timestamp, tmp3)
      } 
    }
    
    # R.cum is column N in the spreadsheet
    # R.interpolated is column O in the spreadsheet
    R.cum <- R.interpolated <- R.hat <- R.predicted <- R.delta <- R
    
    interpolated <- 1
    for (i in 1:length(R)){
      R.cum[i] <- Return.cumulative(R[1:i])
      R.interpolated[i] <- interpolated*(1+R.cum[i])
    }
    
    #  conversion <- 0.43
    random.scale <- 1 - conversion
    
    # R.random is column P in the speadsheet
    R.random <- random.scale*qnorm(runif(length(R)),0,std.M/sqrt(n))
    
    # R.predicted is column Q in the spreadsheet
    # R.hat is column column R in the spreadsheet
    # R.delta is column S in the spreadsheet
    
    predicted <- 1
    for (i in 1:length(R)){
      if(i == 1){
        R.hat[i] <- R[i]+conversion*(interpolated - predicted) + R.random[i]*random.scale
        R.predicted[i] <- predicted*(1+R.hat[i])
      } else {
        R.hat[i] <- R[i]+conversion*(R.interpolated[i-1] - R.predicted[i-1]) + R.random[i]*random.scale
        R.predicted[i] <- R.predicted[i-1]*(1+R.hat[i])
      }
    }
    
    R.delta <- R.predicted - R.interpolated
    
    std.D <- sd(R.hat)

    T.eff <- n + 2*(random.scale/(1-random.scale)^2)*((n-1)*(1-random.scale)-random.scale*(1-random.scale*(n-1)))
    std.MtoD <- std.M/sqrt(T.eff)
    list <- list(std.M = std.M, std.D = std.D, std.MtoD = std.MtoD, res.cum = res.cum, R.predicted = R.predicted, timestamp = timestamp)
    return(list)
    
  }
  
  choice.update <- reactive({  
    choice <- switch(input$choice,
                     "UTA" = "UTA",
                     "DEXUS" = "DEXUS",
                     "IFM" = "IFM",
                     "AT" = "AT")
  })
  
  
  
  n.update <- reactive({  
    n <- input$n
  })
  
  conversion.update <- reactive({  
    conversion <- input$conversion
  })
  
  
  
  output$plot <- renderPlot({

    choice <- choice.update()
    n <- n.update()
    conversion <- conversion.update()
    
    res <- xts(residuals[,-1],as.Date(as.yearmon(residuals[,1])))/100
    
    res <- res[, choice]
    
    md <- MtoD(res, n = n, conversion = conversion, interpolated = 1, predicted = 1)
    
    plot(md$res.cum, col = 2, type = "p", main = "Monthly residuals VS. Predicted daily residuals", xaxt = "n", ylab = "Residual Cumlative Returns")
    lines(md$R.predicted, col = 4)
    axis(1,at=1:length(md$R.predicted), labels = as.yearmon(md$timestamp), tick = F)
    
    
  })
  
  output$summary <- renderPrint({
    
    choice <- choice.update()
    n <- n.update()
    conversion <- conversion.update()
    
    res <- xts(residuals[,-1],as.Date(as.yearmon(residuals[,1])))/100
    
    res <- res[, choice]
    
    md <- MtoD(res, n = n, conversion = conversion, interpolated = 1, predicted = 1)
    
    cat(paste("Monthly standard deviation of residuals is\n", round(md$std.M,8), sep = ""))
    cat(paste("\nDaily standard deviation of predicted residuals is\n", round(md$std.D,8), sep = ""))
    cat(paste("\nScaled daily standard deviation (From Monthly to Daily) is\n", round(md$std.MtoD,8), "\n\n", sep = ""))
    
  })

})

