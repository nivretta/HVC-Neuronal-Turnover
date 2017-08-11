library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)

##initialize variables
#time interval of one day
dt <- 1
#total of 75 days
tFinal <- 75
if (!exists("progenitors")) {
  
  t <- seq(0, tFinal, by= dt) #an array of t value
  b <- c(1:length(t)) #an array of "breeding season" values
  progenitors <- c(1:length(t)) #new stem cells & neural progenitor cells in VZ (express nestin)
  migrators <- c(1:length(t)) #new daughters of neural progenitor cells that have begun to migrate to HVC (express doublecortin)
  newNeuronHVC <- c(1:length(t)) #proportion of migrators that successfully get to HVC (express NeuN)
  neuronNumHVC <- seq(200000, (tFinal+200000), by= dt) #total number of neurons in HVC
  apoptosisHVC <- c(1:length(t)) #death in HVC
  proliferationVZ <- c(1:length(t)) #proliferation in VZ
  migrationSuccess <- c(1:length(t))
  apoptosis <- c(1:length(t))
  preserveHVC <- c(1:length(t))
}


#minVZ <- 7800  #minimum number of newly proliferated cells in VZ during LD, taken from Reg I data
#migrationDuration <- 10  #days
#changeDuration <- 2  #days it takes to change from stem cell to migrator that expresses doublecortin
#incorporationSuccess <- 0.5  #success of migrated neurons' incorporation


ui <- fluidPage( 
  titlePanel("HVC Cell Populations"),
  
  sidebarLayout(
    sidebarPanel(
      
      #minimum number of newly proliferated cells in VZ during LD, default 7800 taken from Reg I data
      numericInput("minVZ", label = "Minimum number of newly proliferated cells in VZ during LD", value = 7800, min = 0, max = 20000, step = 1),
      
      #migration duration in days from VZ to HVC, default is 10 days  
      #numericInput("migrationDuration", label = "Days to migrate from VZ to HVC", value = 10, min = 0, max = 100, step = 1),
      
      #days it takes to change from stem cell to migrator that expresses doublecortin, default is 2 days
      #numericInput("changeDuration", label = "Days to change from migrating stem cell to doublecortin expressing", value = 2, min = 0, max = 100, step = 1),
      
      #proportion of migrated neurons that successfully incorporation, default is 0.5
      numericInput("incorporationSuccess", label = "Proportion of migrated neurons that successfully incorporation", value = 0.5, min = 0, max = 1, step = 0.01),
      
      numericInput("higherRate", label = "Higher proliferation rate", value = 1.5, min = 0, max = 5, step = 0.01),
      numericInput("lowerRate", label = "lower proliferation rate", value = 1, min = 0, max = 5, step = 0.01)
      
    ),
    
    mainPanel(
      
      plotOutput("coolplot")
      
    )
  )
)


server <- function(input, output) {
    
  ##updating cell populations
     
  for (n in 1:(length(t))) { 
    
    #breeding vs non breeding conditions
    if (t[n] < 15) { 
      b[n] = 1 
    } else if (t[n] >=15 & t[n] <= 45) { 
      b[n] = 0 
    } else {
      b[n] = 1 } 
  }
  
    proliferationVZ <- reactive({
    
      for (n in 1:(length(t))){
      
      if (b[n] == 0 & t[n] >=16 & t[n] <=25) {
      proliferationVZ[n] = input$higherRate  #rate of VZ prolif during LD --> SD
    } else if (b[n] == 1 & t[n] >=46 & t[n] <=56) {
      proliferationVZ[n] = input$lowerRate  #rate of VZ prolif during SD --> LD
    } else {
      proliferationVZ[n] = 1 } #rate of VZ prolif during stable state
    }
      
    })
    
    progenitors <- reactive({
      for (n in 1:(length(t))){
      progenitors[n] = input$minVZ * proliferationVZ[n] 
      }
    })
    
    for (n in 1:(length(t))) { 
    
      if (b[n] == 1) { 
      migrators[n] = (progenitors[n] * 0.5)  #neural progenitor daughters that migrate out during breeding condition
    } else {
      migrators[n] = (progenitors[n] * 0.5) }
    
    
    if (b[n] == 1) { 
      migrationSuccess[n] = 0.5  
    } else {
      migrationSuccess[n] = 0.47}
    
    
    newNeuronHVC[n] = migrators[n] * migrationSuccess[n]  
    
    
    if (b[n] == 0 & t[n] >=16 & t[n] <=25) {
      apoptosisHVC[n] = .0125  #apoptosis rate during LD-->SD   
    } else if (b[n] == 1 & t[n] >=46 & t[n] <=56) {
      #apoptosis rate during SD-->LD, less due to effect of testosterone
      apoptosisHVC[n] = .0025  
    } else {
      apoptosisHVC[n] = .0049}  #apoptosis rate during stable state
    
    
    #if b[n] == 0 && t[n] >=16 && t[n] <=25
    #    preserveHVC[n] = 0  #T does not save neurons from death during LD-->SD   
    #elseif b[n] == 1 && t[n] >=46 && t[n] <=56
    #    preserveHVC[n] = .01  #T "saves" mature neurons in HVC from death during SD --> LD 
    #else
    #     preserveHVC[n] = 0  #during stable state
    # end
    
    }
    
    neuronNumHVC <- reactive({
      for (n in 1:(length(t))) {
      neuronNumHVC[n+1] = neuronNumHVC[n] + (newNeuronHVC[n] * input$incorporationSuccess) - (apoptosisHVC[n] * neuronNumHVC[n]) 
    }
    })
    
    for (n in 1:(length(t))) {
    apoptosis[n] = apoptosisHVC[n] * neuronNumHVC[n] 
    }
    
  
  

  output$coolplot <- renderPlot({
    
    p1 <- qplot(seq_along(progenitors), progenitors) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("number of progenitors in VZ")
    
    p2 <- qplot(seq_along(newNeuronHVC), newNeuronHVC) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("HVC New Neuron Number")
    
    p3 <- qplot(seq_along(neuronNumHVC), neuronNumHVC) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("HVC Total Neuron Number")
    
    p4 <- qplot(seq_along(apoptosis), apoptosis) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("Density of Cell Death in HVC")
    
    grid.arrange(p1, p2, p3, p4, ncol = 2)
    
  })

}

shinyApp(ui = ui, server = server)
