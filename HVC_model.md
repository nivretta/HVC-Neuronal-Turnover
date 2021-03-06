HVC Model
================
Nivretta Thatra
16/05/2017

Four different cell populations (VZ progenitors, migrators ie the daughters of the progenitors that migrate out of VZ, new neurons in HVC, total neurons in HVC) change in number over time in the GWS song circuit. Here I attempt to quantify to what exent different variables (breeding season (binary, either 0 or 1), stable or plastic states of circuit (probably receptor sensitivity to T?), proliferation in VZ, success of migrators' migration to HVC, apoptosis in HVC) affect these cell population numbers.

``` r
library(ggplot2)
library(gridExtra)
```

### Initialize variables and vectors

``` r
#time interval of one day
dt = 1
#total of 75 days
tFinal = 75

#list of numeric t values
t = seq(0, tFinal, by= dt)

b = c(1:length(t)) #list of "breeding season" values
progenitors = c(1:length(t)) #new stem cells & neural progenitor cells in VZ (express nestin)
migrators = c(1:length(t)) #new daughters of neural progenitor cells that have begun to migrate to HVC (express doublecortin)
newNeuronHVC = c(1:length(t)) #proportion of migrators that successfully get to HVC (express NeuN)
neuronNumHVC = seq(200000, (tFinal+200000), by= dt) #total number of neurons in HVC, with baseline 200,000
apoptosisHVC = c(1:length(t)) #death in HVC
proliferationVZ = c(1:length(t)) #proliferation in VZ
migrationSuccess = c(1:length(t))
apoptosis = c(1:length(t))

minVZ = 7800  #minimum number of newly proliferated cells in VZ during LD, taken from Reg I data
migrationDuration = 10  # duration of migration in days
incorporationSuccess = 0.5  #success of migrated neurons' incorporation

##unused variable but useful to know
#changeDuration = 2  #days it takes to change from stem cell to migrator that expresses doublecortin

#unused list, set up so to create 'older' generation of HVC neurons. Doesn't make much sense now
#preserveHVC = c(1:length(t))
```

### Set up a mock rapid regression and rapid growth experiment; hypothesis 1

Proliferation in VZ occurrs at the same rapid rate during LD -&gt; SD (supported by rapid regression data) and SD -&gt; LD (hypothesis, proved wrong by rapid growth project). The model shows HVC neuron number increase in LD due primarily to a neuroprotective effect localized to HVC neurons. There is also a secondary neuroprotective effect; the success of progenitor cell migration is slightly lower in SD than LD.

``` r
for (n in 1:(length(t))){ 
    
    #assign 'breeding seasons' to time points 
    if (t[n] < 15) { 
        b[n] = 1 
    } else if (t[n] >=15 & t[n] <= 45) { 
        b[n] = 0 
    } else {
        b[n] = 1 } 

    #days 16-25 and 46-56 are 'plastic state' days, ie when the neural circuitry is transitioning between seasons
    if (b[n] == 0 & t[n] >=16 & t[n] <=25) {
        proliferationVZ[n] = 1.5  #rate of VZ prolif during LD --> SD
    } else if (b[n] == 1 & t[n] >=46 & t[n] <=56) {
        proliferationVZ[n] = 1.5  #rate of VZ prolif during SD --> LD
    } else {
        proliferationVZ[n] = 1 } #rate of VZ prolif during stable state
    
    
    progenitors[n] = minVZ * proliferationVZ[n] 

    #if we hyopthesize that the number of progenitors' daughters that migrate out of VZ is affected by breeding season, we can test that here
    if (b[n] == 1) { 
        migrators[n] = (progenitors[n] * 0.5)  #neural progenitor daughters that migrate out during breeding condition
    } else {
        migrators[n] = (progenitors[n] * 0.5) }
    
    #if we hypothesize that some migrating progenitor daughters don't make it to HVC as a function of breeding season, we can test that here 
    if (b[n] == 1) { 
        migrationSuccess[n] = 0.5  
    } else {
        migrationSuccess[n] = 0.47}
    
    newNeuronHVC[n] = migrators[n] * migrationSuccess[n]  

    #Effect of breeding season on apoptosis...neuroprotective effect of T in breeding season is primarily found here
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
    #    preserveHVC[n] = 0  #during stable state
    
    neuronNumHVC[n+1] = neuronNumHVC[n] + (newNeuronHVC[n] * incorporationSuccess) - (apoptosisHVC[n] * neuronNumHVC[n]) 
    
    apoptosis[n] = apoptosisHVC[n] * neuronNumHVC[n] 
}
```

### Plotting cell populations according to hypothesis 1

``` r
p1 <- qplot(seq_along(progenitors), progenitors) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("number of progenitors in VZ")

p2 <- qplot(seq_along(newNeuronHVC), newNeuronHVC) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("HVC New Neuron Number")

p3 <- qplot(seq_along(neuronNumHVC), neuronNumHVC) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("HVC Total Neuron Number")

p4 <- qplot(seq_along(apoptosis), apoptosis) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("Density of Cell Death in HVC")


source("multiplot.R")
graph <- multiplot(p1, p2, p3, p4, cols=2)
```

![](HVC_model_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Set up a mock rapid regression and rapid growth experiment; hypothesis 2

Proliferation in VZ increases during LD -&gt; SD (supported by rapid regression data) and drops SD -&gt; LD (supported by rapid growth project). The model shows HVC neuron number increase in LD due to: 1- a neuroprotective effect localized to HVC neurons. 2- a neuroprotective effect for migrating progenitor cells or incorporation of new neurons into HVC (basically an effect on new neurons) also accounts for growth of HVC.

``` r
for (n in 1:(length(t))){ 
    
    #assign 'breeding seasons' to time points 
    if (t[n] < 15) { 
        b[n] = 1 
    } else if (t[n] >=15 & t[n] <= 45) { 
        b[n] = 0 
    } else {
        b[n] = 1 } 

    #days 16-25 and 46-56 are 'plastic state' days, ie when the neural circuitry is transitioning between seasons
    if (b[n] == 0 & t[n] >=16 & t[n] <=25) {
        proliferationVZ[n] = 1.5  #rate of VZ prolif during LD --> SD
    } else if (b[n] == 1 & t[n] >=46 & t[n] <=56) {
        proliferationVZ[n] = 0.65  #rate of VZ prolif during SD --> LD
    } else {
        proliferationVZ[n] = 1 } #rate of VZ prolif during stable state
    
    
    progenitors[n] = minVZ * proliferationVZ[n] 

    #if we hyopthesize that the number of progenitors' daughters that migrate out of VZ is affected by breeding season, we can test that here
    if (b[n] == 1) { 
        migrators[n] = (progenitors[n] * 0.5)  #neural progenitor daughters that migrate out during breeding condition
    } else {
        migrators[n] = (progenitors[n] * 0.5) }
    
    #if we hypothesize that some migrating progenitor daughters don't make it to HVC as a function of plastic v stable sates, we can test that here
    if (b[n] == 0 & t[n] >=16 & t[n] <=25) {
        migrationSuccess[n] = 0.47  #migration success during LD-->SD, decreased slightly due to loss of T   
    } else if (b[n] == 1 & t[n] >=46 & t[n] <=56) {
        migrationSuccess[n] = 0.9  #migration success during SD-->LD, increased due to effect of testosterone
    } else {
        migrationSuccess[n] = 0.5}  #migration success during stable state
    
    newNeuronHVC[n] = migrators[n] * migrationSuccess[n]  

    #Effect of breeding season on apoptosis...one neuroprotective effect of T 
    if (b[n] == 0 & t[n] >=16 & t[n] <=25) {
        apoptosisHVC[n] = .0125  #apoptosis rate during LD-->SD   
    } else if (b[n] == 1 & t[n] >=46 & t[n] <=56) {
        apoptosisHVC[n] = .001  #apoptosis rate during SD-->LD, less due to effect of testosterone
    } else {
        apoptosisHVC[n] = .0049} #apoptosis rate during stable state
    
  
    
    neuronNumHVC[n+1] = neuronNumHVC[n] + (newNeuronHVC[n] * incorporationSuccess) - (apoptosisHVC[n] * neuronNumHVC[n]) 
    
    apoptosis[n] = apoptosisHVC[n] * neuronNumHVC[n] 
}
```

### Plotting cell populations according to hypothesis 2

``` r
p5 <- qplot(seq_along(progenitors), progenitors) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("number of progenitors in VZ")

p6 <- qplot(seq_along(newNeuronHVC), newNeuronHVC) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("HVC New Neuron Number")

p7 <- qplot(seq_along(neuronNumHVC), neuronNumHVC) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("HVC Total Neuron Number")

p8 <- qplot(seq_along(apoptosis), apoptosis) + xlab("0-15 end of LD, 16-45 SD, 46-75 LD") + ylab("Density of Cell Death in HVC")


source("multiplot.R")
graph <- multiplot(p5, p6, p7, p8, cols=2)
```

![](HVC_model_files/figure-markdown_github/unnamed-chunk-6-1.png)
