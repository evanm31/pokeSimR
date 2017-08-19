library(shiny)
library(tidyr)
library(dplyr)
library(dummies)
library(lsa)
library(nnet)
library(reshape2)
library(plotly)
library(jsonlite)
library(shinysky)

poke <- read.csv("/home/evan/Pokemon.csv")
poke <- poke[- grep("Mega", poke$Name),]
poke$Legendary <- plyr::revalue(poke$Legendary, c("False"="0", "True"="1"))
poke$Legendary <- as.integer(poke$Legendary)
poke$Legendary <- poke$Legendary - 1
poke$Name <- as.character(poke$Name)
info <- poke[,1:2]
pokeCopy <- poke

name <- t(as.data.frame(info$Name))
names(name) <- info$Name

choices = data.frame(
  var = names(name),
  num = 1:length(names(name))
)

choices$var <- as.character(choices$var)

#list of choices for selectInput
mylist <- as.list(choices$num)
names(mylist) <- as.character(choices$var)

normalize.it <- function(vec) {
  y <- (vec - min(vec))/(max(vec) - min(vec))
  y
}

poke[,5:13] <- lapply(poke[,5:13], FUN = normalize.it)
poke <- data.frame(poke)

poke <- dummy.data.frame(poke, names=c("Generation" , "Legendary" , "Type.1", "Type.2"), sep="_")
poke <- plyr::rename(poke, c("Generation_0" = "Generation_1", "Generation_0.2" = "Generation_2","Generation_0.4" = "Generation_3","Generation_0.6" = "Generation_4","Generation_0.8" = "Generation_5","Generation_1" = "Generation_6"))

cos <- as.matrix(poke[,-(1:2)])
mat.sim <- as.data.frame(cosine(t(cos)))
diag(mat.sim) <- 0
rownames(mat.sim) <- choices$var
colnames(mat.sim) <- choices$var

ui <- shinyUI(
  pageWithSidebar(
    headerPanel("PokéSim R"),
  
  #loading message
  sidebarPanel(
    tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
    
  conditionalPanel(condition="input.tabselected==1",
        helpText("This app finds closeness between Pokémon through a cosine similarity measure calculated from a numeric matrix of stats to quantify relationships between different Pokémon."),
        hr(),
        selectInput(
          "pokemon",
          label = h3("Select Pokémon:"),
          choices = choices$var),
        shinysky::textInput.typeahead(
          id="textIn",
          placeholder="Enter a Pokémon",
          local=data.frame(name=choices$var),
          valueKey = "name",
          tokens=choices$var,
          template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p> <p class='repo-description'></p>")
        ),
        actionButton(inputId = "searchInput", "Find Similarity (Input)"),
        actionButton(inputId = "newPoke", "Generate Pokémon"),
        radioButtons("type", "Similarity type:",
                     c("Maximum" = "max",
                       "Minimum" = "min"))),
  
  conditionalPanel(condition="input.tabselected==2",
                   helpText("Heatmaps allow us to view the relationships between Pokémon on a broad scale; notice the trend of higher similarity between generations compared to the randomized version."),
                   hr(),
                   sliderInput("size", "Pokémon in plot:",
                               min = 5, max = 751, value = 250, step = 1),
                   actionButton(inputId = "random", "Randomize Pokémon Order"),
                   actionButton(inputId = "normal", "Reset Pokémon Order")),
  
  conditionalPanel(condition="input.tabselected==3",
                   helpText("Feel free to browse through the original dataset if you'd like!"),
                   hr())),
  
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Similarity Generator", textOutput("pokeInfo"), textOutput("pokeSim"), plotlyOutput("plot1"), tableOutput("table"), value = 1,  conditionalPanel(condition="input.tabselected==1")),
                       tabPanel("Similarity Heatmap", plotlyOutput("plot2"), value = 2,  conditionalPanel(condition="input.tabselected==2")),
                       tabPanel("Explore the Data", dataTableOutput("table2"), value = 3,  conditionalPanel(condition="input.tabselected==3")),
                       id = "tabselected")
      )
   )
  )