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
  
  #data table
  output$table2 <- renderDataTable({
    pokeCopy
  })
  
}
