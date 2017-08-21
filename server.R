server <- function(input, output, session) {
  
  #update once at beginning of session
  sim <- which.is.max(mat.sim[1,])
  
  poke.m <- melt(pokeCopy[c(1, sim),], id.vars = "Name")
  
  poke.m.num <- poke.m[7:20,]
  poke.m.num$value <- as.numeric(poke.m.num$value)
  
  poke.m.fac <- poke.m[-(7:20),]
  poke.m.fac <- poke.m.fac[-(1:2),]
  poke.m.fac <- spread(poke.m.fac, key = Name, value = value)
  
  output$pokeInfo <- renderPrint({
    cat("The most similar Pokémon to Bulbasaur is: ")
  })
  
  output$pokeSim <- renderText({
    as.character(pokeCopy[sim, 2])
  })
  
  output$plot1 <- renderPlotly({
    ggplotly(ggplot(poke.m.num, aes(x = variable, y = value, fill = Name)) + geom_bar(stat="identity", position=position_dodge(), colour = "black") + coord_flip() + labs(y="Value", x="Stat"))
  })
  
  output$table <- renderTable({
    poke.m.fac
  })
  
  #update from menu
  observeEvent(input$pokemon, handlerExpr = {
    
    index <- which(sapply(pokeCopy[,2], FUN=function(X) input$pokemon %in% X))
    
    sim <- which.is.max(mat.sim[index,])
    
    poke.m <- melt(pokeCopy[c(index, sim),], id.vars = "Name")
    
    poke.m.num <- poke.m[7:20,]
    poke.m.num$value <- as.numeric(poke.m.num$value)
    
    poke.m.fac <- poke.m[-(7:20),]
    poke.m.fac <- poke.m.fac[-(1:2),]
    poke.m.fac <- spread(poke.m.fac, key = Name, value = value)
    
    output$pokeInfo <- renderPrint({
      cat("The most similar Pokémon to ", input$pokemon, " is:")
    })
    
    output$pokeSim <- renderText({
      as.character(pokeCopy[sim, 2])
    })
    
    output$plot1 <- renderPlotly({
      ggplotly(ggplot(poke.m.num, aes(x = variable, y = value, fill = Name)) + geom_bar(stat="identity", position=position_dodge(), colour = "black") + coord_flip() + labs(y="Value", x="Stat"))
    })
    
    output$table <- renderTable({
      poke.m.fac
    })
    
  })
  
  #update from text input 
  observeEvent(input$searchInput, handlerExpr = {
    
    #error handling for empty input
    if (input$textIn == "")
      return(NULL)
    
    #error handling for non-valid input
    index <- try(which(sapply(pokeCopy[,2], FUN=function(X) input$textIn %in% X)))
    
    if(inherits(index ,'try-error')){
      return(NULL)
    }
    
    sim <- which.is.max(mat.sim[index,])
    
    poke.m <- melt(pokeCopy[c(index, sim),], id.vars = "Name")
    
    poke.m.num <- poke.m[7:20,]
    poke.m.num$value <- as.numeric(poke.m.num$value)
    
    poke.m.fac <- poke.m[-(7:20),]
    poke.m.fac <- poke.m.fac[-(1:2),]
    poke.m.fac <- spread(poke.m.fac, key = Name, value = value)
    
    output$pokeInfo <- renderPrint({
      cat("The most similar Pokémon to ", input$textIn, " is:")
    })
    
    output$pokeSim <- renderText({
      as.character(pokeCopy[sim, 2])
    })
    
    output$plot1 <- renderPlotly({
      ggplotly(ggplot(poke.m.num, aes(x = variable, y = value, fill = Name)) + geom_bar(stat="identity", position=position_dodge(), colour = "black") + coord_flip() + labs(y="Value", x="Stat"))
    })
    
    output$table <- renderTable({
      poke.m.fac
    })
    
  })
  
  #generate pokémon  
  observeEvent(input$newPoke, handlerExpr = {
    pok <- sample(choices$var, 1)
    index <- which(sapply(pokeCopy[,2], FUN=function(X) pok %in% X))
    
    sim <- which.is.max(mat.sim[index,])
    
    poke.m <- melt(pokeCopy[c(index, sim),], id.vars = "Name")
    
    poke.m.num <- poke.m[7:20,]
    poke.m.num$value <- as.numeric(poke.m.num$value)
    
    poke.m.fac <- poke.m[-(7:20),]
    poke.m.fac <- poke.m.fac[-(1:2),]
    poke.m.fac <- spread(poke.m.fac, key = Name, value = value)
    
    output$pokeInfo <- renderPrint({
      cat("The most similar Pokémon to ", pok, " is:")
    })
    
    output$pokeSim <- renderText({
      as.character(pokeCopy[sim, 2])
    })
    
    output$plot1 <- renderPlotly({
      ggplotly(ggplot(poke.m.num, aes(x = variable, y = value, fill = Name)) + geom_bar(stat="identity", position=position_dodge(), colour = "black") + coord_flip() + labs(y="Value", x="Stat"))
    })
    
    output$table <- renderTable({
      poke.m.fac
    })
  })
  
  #heatmap
  #font params
  f1 <- list(
    family = "Old Standard TT, serif",
    size = 8.5,
    color = "black"
  )
  #x-axis params
  x <- list(
    autotick = T,
    ticks = "outside",
    tickcolor = toRGB("blue"),
    tickfont = f1,
    showticklabels = TRUE,
    tickangle = 60
  )
  #y-axis params
  y <- list(
    autotick = T,
    ticks = "outside",
    tickcolor = toRGB("red"),
    showticklabels = TRUE,
    tickangle = 20,
    tickfont = f1
  )
  
  output$plot2 <- renderPlotly({
    plot_ly(z = as.matrix(mat.sim[1:input$size,1:input$size]), x = rownames(mat.sim), y = colnames(mat.sim), type = "heatmap") %>% layout(xaxis = x, yaxis = y)
  })
  
  #data table
  output$table2 <- renderDataTable({
    pokeCopy
  })
  
  #randomize heatmap
  observeEvent(input$random, handlerExpr = {
    output$plot2 <- renderPlotly({
      samp <- sample(1:751, input$size, replace=FALSE)
      mat.sim.samp <- mat.sim[samp,samp]
      plot_ly(z = as.matrix(mat.sim.samp), x = rownames(mat.sim.samp), y = colnames(mat.sim.samp), type = "heatmap") %>% layout(xaxis = x, yaxis = y)
    }) 
  })
  
  #reset heatmap
  observeEvent(input$normal, handlerExpr = {
    output$plot2 <- renderPlotly({
      plot_ly(z = as.matrix(mat.sim[1:input$size,1:input$size]), x = rownames(mat.sim), y = colnames(mat.sim), type = "heatmap") %>% layout(xaxis = x, yaxis = y)
    })   
  })
  
}