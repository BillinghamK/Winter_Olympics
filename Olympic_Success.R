###############################################################################
# Title:                 Winter Olympic Successes Using GDP And Population Data
# Author:                                                     Kieran Billingham
# Date Created:                                                        10/12/18
# Document width: 80 Characters
# Document Length: 790
# -----------------------------------------------------------------------------
# Notes:
# This code explores 3 datasets for interesting trends and looks to find a
# statistical relationship between successful olympic teams and the population
# and GDP of their National Olympic Comittee's Country
#
# Sources:
# [1] https://www.kaggle.com/the-guardian/olympic-games
# [2] https://data.worldbank.org/indicator/SP.POP.TOTL
# [3] https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?view=map
###############################################################################

# Clear Enviroment and Console ------------------------------------------------
rm(list=ls())
cat("\014")  

# Set Working Directory -------------------------------------------------------
setwd("~/Documents/MSc/Analytics for Data Science/Assessment One")  

# Install And Load Packages ---------------------------------------------------
Packages <- c('ggplot2',
              'tidyverse',
              'dplyr',
              'scales',
              'caTools')

# Find packages that are not currently installed and download from CRAN
NotInstalled <- Packages[!(Packages %in% installed.packages()[,'Package'])]
install.packages(NotInstalled)

# load and attach add-on packages
lapply(Packages, require, character.only = TRUE)

# Read Data And Format For Analysis -------------------------------------------
# [1] Medal Table Data
MedalTable <- read.csv('Winter Olympics.csv', na.strings = 'NA')

# [2] Population Data
PopulationTable <- read.csv('Global Population.csv', skip = 2)
Headers <-  as.character(unlist(PopulationTable[1, ]))
Headers[1] <- 'Country'                                  #Ensure Column Headers
Headers[2] <- 'CountryCode'                              #are Correct
Headers[3] <- 'IndicatorName'
Headers[4] <- 'IndicatorCode'
PopulationTable <-  PopulationTable[-1, ]
colnames(PopulationTable) <- Headers
PopulationTable$IndicatorName <- NULL                  #Remove Uniary Variables
PopulationTable$IndicatorCode <- NULL
PopulationTable$Country <- NULL               #Same information as country code

# [3] Gross Domestic Product Data
GDPTable <- read.csv('Global GDP.csv', skip = 3)
colnames(GDPTable) <- Headers
GDPTable$IndicatorName <- NULL                         #Remove Uniary Variables
GDPTable$IndicatorCode <- NULL
GDPTable$Country <- NULL                      #Same information as country code

# Variables to exlude : summaries of ecomonic areas
Excluded <- c('WLD', 'TLA', 'OSS', 'HIC', 'LAC', 'CSS', 'DED', 'TEC', 'PST',
              'ECA', 'IBT', 'TSA', 'LMY', 'ARB', 'MIC', 'IDA', 'IBD', 'SSF', 
              'EAS', 'TSS', 'UMC', 'SSA', 'ECS', 'CEB', 'NAC', 'MNA', 'LTE',
              'TMN', 'EUU', 'PRE', 'EAP', 'IDX', 'TEA', 'LDC', 'EMU', 'IDB', 
              'EAR', 'FCS', 'HPC', 'LMC', 'LIC', 'LCN', 'SST', 'OED')

###############################################################################
# Exploritory Analysis Of Data Sets -------------------------------------------
# [1] Medal Table Data --------------------------------------------------------

# Dimension of Dataset
dim1 <- dim(MedalTable)                       #5770 Observations of 9 Variables
# Glipmse 
glimpse1 <- glimpse(MedalTable)
# Summary Statistics Of Each Variable 
SummaryStats1 <- summary(MedalTable)

# Different levels in categorical columns -------------------------------------  
SportList <- levels(MedalTable$Sport)           #  7 Sports
DisciplineList <- levels(MedalTable$Discipline) # 15 Disciplines
EventList <-  levels(MedalTable$Event)          # 83 Events
HostList <- levels(MedalTable$City)             # 19 Hosts

# Gold, Silver and Bronze medal distribution per Country/NOC ------------------
# Gold
GoldMedals <- MedalTable[MedalTable[, "Medal"] == 'Gold',]     
GoldsPerCountry <- as.matrix(summary(GoldMedals$Country))      
GoldsPerCountry[order(-GoldsPerCountry[,1]),]
# CAN URS USA NOR GER SWE RUS AUT SUI FIN                   <- Top 20 countries
# GDR ITA KOR NED EUN FRA GBR CZE FRG JPN               <- RUS/URS complication

# Silver
SilverMedals <- MedalTable[MedalTable[, "Medal"] == 'Silver',]
SilverPerCountry <- as.matrix(summary(SilverMedals$Country))
SilverPerCountry[order(-SilverPerCountry[,1]),] 
# USA CAN NOR FIN SWE GER AUT URS RUS TCH                   <- Top 20 countries
# SUI ITA GDR NED FRA CHN KOR FRG JPN CZE               <- RUS/URS complication

# Bronze
BronzeMedals <- MedalTable[MedalTable[, "Medal"] == 'Bronze',]
BronzePerCountry <- as.matrix(summary(BronzeMedals$Country))
BronzePerCountry[order(-BronzePerCountry[,1]),] 
# FIN SWE USA SUI NOR CAN AUT GER URS FRA                   <- Top 20 countries
# RUS ITA TCH GDR FRG NED CHN CZE GBR JPN               <- RUS/URS complication

# Medals Awarded at Each Olympic Games ----------------------------------------
P1 <- ggplot(MedalTable, aes(x = City, fill=Sport)) +
  geom_bar(position = 'stack') +
  scale_x_discrete(limits = as.vector(HostList)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
P1

# Disciplines Grouped by Parent Sport -----------------------------------------
P2 <- ggplot(MedalTable, aes(x = Sport, fill = Discipline)) +
  geom_bar(position = 'stack') +
  coord_flip() +
  ggtitle("Medals Awarded for Disciplines Grouped by Sport\n") +
  xlab("Sport") +
  ylab("Medals Awarded") +
  theme_classic()
P2

# Medals Awarded Each Year
MedalCount <- MedalTable %>%
  group_by(Year, Medal) %>%
  summarize(
    Medals = length(Medal))

# How the games have changed with time ----------------------------------------
# Count number of Countries, Athletes, events etc
counts <- MedalTable %>%
  group_by(Year) %>%
  summarize(
    Athletes = length(unique(Athlete)),
    Countries = length(unique(Country)),
    Sports = length(unique(Sport)),
    Disciplines = length(unique(Discipline)),
    Events = length(unique(Event)))

# Plot count information 
# Athletes 
P3 <- ggplot(counts, aes(x=Year, y=Athletes)) +
  geom_point(alpha=0.4, size=2, colour='blue') +
  geom_line(colour='blue') +
  ggtitle("Number of Athletes Participating in Games") +
  xlab("Year") +
  ylab("No. of Atheletes") +
  theme_classic()

# Countries 
P4 <- ggplot(counts, aes(x=Year, y=Countries)) +
  geom_point(alpha=0.4, size=2, colour='blue') +
  geom_line(colour='blue') +
  ggtitle("Number of Countries Participating in Games") +
  xlab("Year") +
  ylab("No. of Countries") +
  theme_classic()

# Sports included
P5 <- ggplot(counts, aes(x=Year, y=Sports)) +
  geom_point(alpha=0.4, size=2, colour='blue') +
  geom_line(colour='blue') +
  ggtitle("Number of Sports Included in Games") +
  xlab("Year") +
  ylab("No. of Sports") + 
  annotate("text", x=c(1924,1964,1998)-8,
           y=c(6,6,7),
           label=c("Chamonix","Innsbruck 1964","Nagano 1998"),
           size=3) +
  theme_classic()

# Disciplines included
P6 <- ggplot(counts, aes(x=Year, y=Disciplines)) +
  geom_point(alpha=0.4, size=2, colour='blue') +
  geom_line(colour='blue') +
  ggtitle("Number of Disciplines Included in Games") +
  xlab("Year") +
  ylab("No. of Disciplines") + 
  annotate("text", x=c(1964,1992,1998)-8,
         y=c(10,12,14),
         label=c("Innsbruck 1964","Albertville 1992","Nagano 1998"),
         size=3) +
  theme_classic()

# Events included
P7 <- ggplot(counts, aes(x=Year, y=Events)) +
  geom_point(alpha=0.4,size=2, colour='blue') +
  geom_line(colour='blue') +
  ggtitle("Number of Events Included in Games") +
  xlab("Year") +
  ylab("No. of Events") +
  annotate("text", x=c(1964,1992)-8,                       #Add labels to POI's
           y=c(26,41)+1,
           label=c("Innsbruck 1964","Albertville 1992"),
           size=3) +
  theme_classic()

# Disciplines plotted with Events
P8 <- ggplot(counts, aes(x=Year, y=Disciplines, color = 'Disciplines')) +
  geom_point(size=2) +
  geom_line() +
  ggtitle("Number of Disciplines and Events") +
  xlab("Year") +
  ylab("No. of Disciplines") + 
  geom_point(aes(y = Events/2.8, colour = "Events")) +            #Adjust scale
  geom_line(aes(y = Events/2.8, colour = "Events")) +
  scale_y_continuous(sec.axis = sec_axis(~.*2.8, name = "No. of Events")) +
  theme_classic() +
  theme(legend.position = c(0.1, 0.7), legend.title = element_blank())

# Countries plotted with Athletes
P9 <- ggplot(counts, aes(x=Year, y=Countries, color='Countries')) +
  geom_point(size=2) +
  geom_line() +
  ggtitle("Number of Countries Participating in Games") +
  xlab("Year") +
  ylab("No. of Countries") +
  geom_point(aes(y = Athletes/8, colour = "Athletes")) +
  geom_line(aes(y = Athletes/8, colour = "Athletes")) +
  theme_classic() +
  scale_y_continuous(sec.axis = sec_axis(~.*8, name = "No. of Athletes")) +
  
  theme(legend.position = c(0.1, 0.7), legend.title = element_blank())

# Arange plots for report
grid.arrange(P5,P8, ncol=1)
P9

# Male and Female participation -----------------------------------------------
# Table counting number of athletes by Year and Sex
countsGender <- MedalTable %>% 
  group_by(Year, Gender) %>%
  summarize(Athletes = length(unique(Athlete)))

# Plot gender with time
P10 <- ggplot(countsGender, aes(x=Year,y=Athletes,group=Gender,color=Gender)) +
  geom_point(size=2) +
  geom_line()  +
  ggtitle("Number of Female and Male Olmypians") +
  theme_classic()
P10

# Gender Proportion with time
# Normalise the data for each year to see if proportions are converging, or not
# Use only NOC with teams gof reater than 40 athletes
NOCCount <- MedalTable %>% filter(Year %in%
                                    c(1964,1972,1980,1988,1998,2006,2014)) %>%
  group_by(Year, Country, Gender) %>%
  summarize(Count = length(unique(Athlete))) %>%
  spread(Gender, Count) %>%
  mutate(Total = sum(Men,Women,na.rm=T)) %>%
  filter(Total > 20)
# Remove 'NA's
NOCCount$Men[is.na(NOCCount$Men)] <- 0
NOCCount$Women[is.na(NOCCount$Women)] <- 0
NOCCount$Year <- as.factor(NOCCount$Year)
# Normalise
NOCCount$MenProp <- NOCCount$Men/NOCCount$Total
NOCCount$WomenProp <- NOCCount$Women/NOCCount$Total
NOCCount$TotalProp <- NOCCount$Total/NOCCount$Total

# Plot female vs. male athletes by NOC / Year
P11 <- ggplot(NOCCount, aes(x=WomenProp, y=Country, color=Year)) +
  geom_point(alpha=1, size=4) +
  ggtitle("Proportion of Female Athletes Competing Over Time") +
  xlab('Proportion of Female Athletes') +
  geom_vline(aes(xintercept=0.5),color="black", linetype="dashed", size=.5)+
  theme_classic() +
  theme(legend.position = c(0.93, 0.33))
P11


# -----------------------------------------------------------------------------
# [2] Population Data ---------------------------------------------------------
# Dimension of Dataset
dim2 <- dim(PopulationTable)                 #264 observations of 60 variables

# Glipmse 
glimpse2 <- glimpse(PopulationTable)

# Summary Statistics Of Each Variable 
SummaryStats2 <- summary(PopulationTable)

# Restructuring Population Table ----------------------------------------------
# Transpose so that time runs down through observarions
PopulationTable <- as.data.frame(t(PopulationTable))
Header <-  as.character(unlist(PopulationTable[1, ]))        #Re-assign Headers
PopulationTable <-  PopulationTable[-1, ]
colnames(PopulationTable) <- Header
rowname <- rownames(PopulationTable)

# Change Variable class to numeric 
PopulationTemp <- lapply(PopulationTable,
                        function(x) as.numeric(as.character(x)))
PopulationTable <- as.data.frame(PopulationTemp)
rownames(PopulationTable) <- rowname

# Remove excluded Columns
columns <- names(PopulationTable)
Valid <- setdiff(columns, Excluded)
PopulationTable <- PopulationTable[,(Valid)]

# Global Population -----------------------------------------------------------
GPopulation <-  as.data.frame(rowSums(PopulationTable, na.rm = TRUE, dims = 1))
GPopulation$Year <- as.numeric(as.character(rownames(GPopulation)))
colnames(GPopulation) <- c('GPopulation','Year')

P12 <- ggplot(GPopulation, (aes(x=Year, y=(GPopulation/10^9)))) +
  geom_point(alpha=0.4,size=2, colour='blue') +
  geom_line(group=1, colour='blue') +
  ggtitle("Global Population between 1960 and 2017") +
  xlab("Year") +
  ylab("Global Population in Billions") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
P12

# The above plot gives values far greater than is known for population trends
# Consider an error in the data - spot check some known values.
GBR2017 <- PopulationTable[c('2017'), c('GBR')]          #Great Britian in 2017
USA2005 <- PopulationTable[c('2005'), c('USA')]                    #USA in 2005
ITA1990 <- PopulationTable[c('1990'), c('ITA')]                  #Italy in 1990
CHN1982 <- PopulationTable[c('1982'), c('CHN')]                  #China in 1982
# All match with accepted values found on online sources
# It may be possible that on some occasions more than one Country has included
# a person in their population calculation.

# Population Growth for Large Countries ---------------------------------------
# Make new data table for each country of interest
RUS <- select(PopulationTable, RUS)/10^6
RUS$Country <- 'RUS'
RUS$Year <- as.numeric(as.character(rownames(RUS)))
colnames(RUS) <- c('Population', 'Country', 'Year')

USA <- select(PopulationTable, USA)/10^6
USA$Country <- 'USA'
USA$Year <- as.numeric(as.character(rownames(USA)))
colnames(USA) <- c('Population', 'Country', 'Year')

CHN <- select(PopulationTable, CHN)/10^6
CHN$Country <- 'CHN'
CHN$Year <- as.numeric(as.character(rownames(CHN)))
colnames(CHN) <- c('Population', 'Country', 'Year')

IND <- select(PopulationTable, IND)/10^6
IND$Country <- 'IND'
IND$Year <- as.numeric(as.character(rownames(IND)))
colnames(IND) <- c('Population', 'Country', 'Year')

#Bind the datasets together so that they can be plotted as series then plot 
PopSubset <- rbind(RUS, USA, CHN, IND)

P13 <- ggplot(PopSubset, aes(x=Year, y=Population, colour=Country)) +
  geom_line() +
  ggtitle("Population between 1960 and 2017") +
  xlab("Year") +
  ylab("Population in Millions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_classic()

# Population growth for smaller countries -------------------------------------
GBR <- select(PopulationTable, GBR)/10^6                # Put Pop into Millions
GBR$Country <- 'GBR'                                  # Make Country a Variable
GBR$Year <- as.numeric(as.character(rownames(GBR)))       #Make Year a variable
colnames(GBR) <- c('Population', 'Country', 'Year')             #Rename columns 

ITA <- select(PopulationTable, ITA)/10^6
ITA$Country <- 'ITA'
ITA$Year <- as.numeric(as.character(rownames(ITA)))
colnames(ITA) <- c('Population', 'Country', 'Year')

ZAF <- select(PopulationTable, ZAF)/10^6
ZAF$Country <- 'ZAF'
ZAF$Year <- as.numeric(as.character(rownames(ZAF)))
colnames(ZAF) <- c('Population', 'Country', 'Year')

IRN <- select(PopulationTable, IRN)/10^6
IRN$Country <- 'IRN'
IRN$Year <- as.numeric(as.character(rownames(IRN)))
colnames(IRN) <- c('Population', 'Country', 'Year')

#Bind the datasets together so that they can be plotted as series then plot 
PopSubset2 <- rbind(GBR, ITA, ZAF, IRN)

P14 <- ggplot(PopSubset2, aes(x=Year, y=Population, colour=Country)) +
  geom_line() +
  ggtitle("Population between 1960 and 2017") +
  xlab("Year") +
  ylab("Population in Millions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_classic()

grid.arrange(P13,P14, ncol=1)     #Arange the plots together to aid comparisons 

# -----------------------------------------------------------------------------
# [3] GDP Data ----------------------------------------------------------------

# Dimension of Dataset
dim3 <- dim(GDPTable)                         #264 observations of 60 variables
# Glipmse 
glimpse3 <- glimpse(GDPTable)              #Shows that data needs restructuring
# Summary Statistics Of Each Variable 
SummaryStats3 <- summary(GDPTable)                                #Yearly Stats 

# Restructuring GDP Table -----------------------------------------------------
# Transpose so that time runs down through observarions
GDPTable <- as.data.frame(t(GDPTable))
Header <-  as.character(unlist(GDPTable[1, ]))              #Re-assign Headers
GDPTable <-  GDPTable[-1, ]
colnames(GDPTable) <- Header
rowname <- rownames(GDPTable)

# Change Variable class to numeric 
GDPTemp <-lapply(GDPTable, function(x) as.numeric(as.character(x)))
GDPTable <- as.data.frame(GDPTemp)
rownames(GDPTable) <- rowname

# Remove excluded Columns
columns <- names(GDPTable)
Valid <- setdiff(columns, Excluded)
GDPTable <- GDPTable[,(Valid)]

SummaryStats4 <- summary(GDPTable)                      #Stats for Each country 

# Mean GDP for each Country ---------------------------------------------------
# Whilst inspecting table, a lot of variables were seen to have high sparcity
# Any with more than 0.95 Sparsity will be removed
GDPTable <- GDPTable[, colMeans(is.na(GDPTable)) <= .01] 

# Make Dataframe of the GDP means to plot
GDPCountryMean <- as.data.frame(colMeans(GDPTable, na.rm = TRUE )/10^9)
GDPCountryMean$Country <- rownames(GDPCountryMean)
colnames(GDPCountryMean) <- c('MeanGDP', 'Country')
# Save complete version for nomrls distribution analysis
GDPCountryMeanN <- GDPCountryMean

# Take global mean away from each country
GlobalMean <- mean(GDPCountryMean$MeanGDP)
GDPCountryMean$MeanGDP <- GDPCountryMean$MeanGDP - GlobalMean
sd(GDPCountryMean$MeanGDP)

# Plot
# Exclude any value more than 0.5 SD away from mean from plot.
# Add density to show the poor distribution of GDP
P15 <- ggplot(GDPCountryMean, aes(x=MeanGDP)) + 
  geom_histogram(aes(y=..density..),binwidth=10) +
  geom_vline(aes(xintercept=0),color="blue", linetype="dashed", size=.5)+
  geom_density(alpha=.2, fill="red") +
  xlim(-sd(GDPCountryMean$MeanGDP)/2,sd(GDPCountryMean$MeanGDP)/2) +     
  xlab("Divergence from global mean GDP (Billion's of Dollars)") +
  ylab('Number of Countries') +
  ggtitle('GDP Distribution within 0.5 s.d of mean GDP')+ 
  theme_classic()
P15
# Global GDP mean over time ---------------------------------------------------
GDPYearMean <- as.data.frame(rowMeans(GDPTable, na.rm = TRUE )/10^9)
GDPYearMean$Year <- rownames(GDPYearMean)
colnames(GDPYearMean) <- c('MeanGDP', 'Year')
GDPYearMean$Year <- (as.numeric(as.character(GDPYearMean$Year)))

# Plot
P16 <- ggplot(GDPYearMean, aes(x=Year, y=MeanGDP)) +
  geom_point(alpha=0.5,size=2, colour='blue') +
  geom_line(group=1, colour='blue')+
  xlab('Year') +
  ylab('Global GDP') +
  ggtitle('The increase of Mean GDP against time') +
  theme_classic()
P16
# More Economically Developed Examples ----------------------------------------
# Less Economically Developed Examples ----------------------------------------

###############################################################################
# Joining Datasets ------------------------------------------------------------
# Look more in depth at the last 5 years of olympic history for the top 20
# performing IOC's that have matching ISO codes. i.e. the olympic team is tied
# to an economic area and population (excluding RUS and URS for simplification)

OlympicYears <- c('2014', '2010', '2006','2002', '1998')

Top20Teams <-  c('CAN', 'CHN', 'USA', 'NOR', 'GER',
                 'SWE', 'RUS','AUT', 'SUI', 'FIN',
                 'GDR', 'ITA', 'KOR', 'NED', 'EUN', 
                 'FRA', 'GBR', 'CZE', 'FRG', 'JPN') 

ISOCodesP <- names(PopulationTable)
ISOCodesG <- names(GDPTable)
ValidOlympicTeams <- Top20Teams[(Top20Teams %in% ISOCodesP)]
ValidOlympicTeams <- ValidOlympicTeams[(ValidOlympicTeams %in% ISOCodesG)]

# Filter medal table ----------------------------------------------------------
MedalTable$Year <- as.factor(MedalTable$Year)

DFMedal <- MedalTable[MedalTable$Year %in% OlympicYears,]
DFMedal <- DFMedal[DFMedal$Country %in% ValidOlympicTeams,]

# Filter Population table and produce for each olympic year -------------------
PopulationTable$Year <- as.factor(rownames(PopulationTable))
DFPop <- PopulationTable[PopulationTable$Year %in% OlympicYears,]
DFPop <- DFPop[,(ValidOlympicTeams)]

Pop1998 <- as.data.frame(t(DFPop['1998',]))
Pop1998$Country <- rownames(Pop1998)
Pop1998$Year <- '1998'
colnames(Pop1998) <- c('Population','Country','Year')

Pop2002 <- as.data.frame(t(DFPop['2002',]))
Pop2002$Country <- rownames(Pop2002)
Pop2002$Year <- '2002'
colnames(Pop2002) <- c('Population','Country','Year')

Pop2006 <- as.data.frame(t(DFPop['2006',]))
Pop2006$Country <- rownames(Pop2006)
Pop2006$Year <- '2006'
colnames(Pop2006) <- c('Population','Country','Year')

Pop2010 <- as.data.frame(t(DFPop['2010',]))
Pop2010$Country <- rownames(Pop2010)
Pop2010$Year <- '2010'
colnames(Pop2010) <- c('Population','Country','Year')

Pop2014 <- as.data.frame(t(DFPop['2014',]))
Pop2014$Country <- rownames(Pop2014)
Pop2014$Year <- '2014'
colnames(Pop2014) <- c('Population','Country','Year')

Population <- rbind(Pop1998, Pop2002, Pop2006, Pop2010, Pop2014)
Population$Country <- as.factor(Population$Country)

# Filter GDP Table and produce for each olympic year --------------------------
GDPTable$Year <- as.factor(rownames(GDPTable))
DFGDP <- GDPTable[GDPTable$Year %in% OlympicYears,]
DFGDP <- DFGDP[,(ValidOlympicTeams)]

GDP1998 <- as.data.frame(t(DFGDP['1998',]))
GDP1998$Country <- rownames(GDP1998)
GDP1998$Year <- '1998'
colnames(GDP1998) <- c('GDP','Country','Year')

GDP2002 <- as.data.frame(t(DFGDP['2002',]))
GDP2002$Country <- rownames(GDP2002)
GDP2002$Year <- '2002'
colnames(GDP2002) <- c('GDP','Country','Year')

GDP2006 <- as.data.frame(t(DFGDP['2006',]))
GDP2006$Country <- rownames(GDP2006)
GDP2006$Year <- '2006'
colnames(GDP2006) <- c('GDP','Country','Year')

GDP2010 <- as.data.frame(t(DFGDP['2010',]))
GDP2010$Country <- rownames(GDP2010)
GDP2010$Year <- '2010'
colnames(GDP2010) <- c('GDP','Country','Year')

GDP2014 <- as.data.frame(t(DFGDP['2014',]))
GDP2014$Country <- rownames(GDP2014)
GDP2014$Year <- '2014'
colnames(GDP2014) <- c('GDP','Country','Year')

GDP <- rbind(GDP1998, GDP2002, GDP2006, GDP2010, GDP2014)
GDP$Country <- as.factor(GDPulation$Country)

# Merge Population and GDP data -----------------------------------------------
CountryInformation <- merge(GDP, Population,by=c("Year","Country"))

# Merge Country Information with Medal Table ----------------------------------
Dataset <- merge( DFMedal, CountryInformation, by=c("Year", "Country"))

###############################################################################
# Statistical Analysis Of Dataset ---------------------------------------------
# [1] Checking for a normal distribution --------------------------------------

# Define Kurtosis and Skew functions along with associated standard errors-----
kurtosis <- function(x) {
  n <- length(x)
  i <- sum(((x-mean(x))/sd(x))^4) - (3*(n-1)^2)/((n-2)*(n-3))
  y <- (n*(n+1))/((n-1)*(n-2)*(n-3)) * i
  return(y)
}

skew <- function(x) {
  n <- length(x)
  y <- n/((n-1)*(n-2))* sum(((x-mean(x))/sd(x))^3)
  return(y)
}

kurtosis_SE <- function(x) {
  n <- length(x)
  sqrt(
    (24*n*(n-1)^2)/((n-2)*(n-3)*(n+5)*(n+3))
  )
}

skew_SE <- function(x) {
  n <- length(x)
  sqrt(
    (6*n*(n-1))/((n-2)*(n+1)*(n+3))
  )
}

# A. Population ---------------------------------------------------------------
Population1 <- Dataset$Population                             #Get numeric list

PopulationMean <- mean(Population1)                                   #Get mean
PopulationSD <- sd(Population1)                         #Get Standard Deviation

PopulationKurtosis <- kurtosis(Population1)                      #Get Kurtosis
PopulationSkew <- skew(Population1)                                  #Get Skew

PopulationKurtError <- kurtosis_SE(Population1)
PopulationSkewError <- skew_SE(Population1)

PopulationKurtZ <- PopulationKurtosis/PopulationKurtError               #128.09     
PopulationSkewZ <- PopulationSkew/PopulationSkewError                    #58.90
# significance limit set to 0.05, so for nomal distribution Z scores should be
# between -1.96 and 1.96. This data is outside the acceptance zone.
# Reject the Null Hypothesis.

# B. GDP ----------------------------------------------------------------------
GDP1 <- Dataset$GDP 

GDPMean <- mean(GDP1)                                                 #Get mean
GDPSD <- sd(GDP1)                                       #Get Standard Deviation

GDPKurtosis <- kurtosis(GDP1)                                     #Get Kurtosis
GDPSkew <- skew(GDP1)                                                 #Get Skew

GDPKurtError <- kurtosis_SE(GDP1)
GDPSkewError <- skew_SE(GDP1)

GDPKurtZ <- GDPKurtosis/GDPKurtError                                     #33.69     
GDPSkewZ <- GDPSkew/GDPSkewError                                         #27.08
# significance limit set to 0.05, so for nomal distribution Z scores should be
# between -1.96 and 1.96. This data is outside the acceptance zone.
# Reject the Null Hypothesis.

# C. Whole set ----------------------------------------------------------------
# Normality testing whole GDP data 
normal1 <- GDPCountryMeanN
normal1$MeanGDP <- log(normal1$MeanGDP*10^9)          # Return to Dollar format
loggedmean <- mean(normal1$MeanGDP)

# Plot distribution
Pnormal <- ggplot(normal1, aes(x=MeanGDP)) + 
  geom_histogram(aes(y=..density..),binwidth=0.5) +
  geom_vline(aes(xintercept=loggedmean),color="blue",
             linetype="dashed", size=.5) +
  geom_density(alpha=.2, fill="red") +
  xlab("Base 10 log of Mean GDP of countries  log(Dollars)") +
  ylab('Number of Countries') +
  ggtitle('GDP Distribution of countries')+ 
  theme_classic()
Pnormal

#Get Kurtosis and Skew
GDPKurtosis <- kurtosis(normal1$MeanGDP)                          #Get Kurtosis
GDPSkew <- skew(normal1$MeanGDP)                                      #Get Skew

GDPKurtError <- kurtosis_SE(normal1$MeanGDP)
GDPSkewError <- skew_SE(normal1$MeanGDP)

GDPKurtZ <- GDPKurtosis/GDPKurtError                                     # 4.66                                      
GDPSkewZ <- GDPSkew/GDPSkewError                                          #0.70

# Look to perform Non-Parametric Testing --------------------------------------
# [2]  Wilcoxon Signed Rank ---------------------------------------------------

# Count medals given to each country, each year
MedalByMetalYearCountry <- Dataset %>% count(Country, Year, Medal)
MedalByYearCountry <- Dataset %>% count(Country, Year)

# seperate out so that they can be input into Wilcoxon
Medals1998 <-(MedalByYearCountry[MedalByYearCountry$Year %in% '1998','n'])
Medals1998 <- Medals1998$n
Medals2002 <-(MedalByYearCountry[MedalByYearCountry$Year %in% '2002','n'])
Medals2002 <- Medals2002$n
Medals2006 <-(MedalByYearCountry[MedalByYearCountry$Year %in% '2006','n'])
Medals2006 <- Medals2006$n
Medals2010 <-(MedalByYearCountry[MedalByYearCountry$Year %in% '2010','n'])
Medals2010 <- Medals2010$n
Medals2014 <-(MedalByYearCountry[MedalByYearCountry$Year %in% '2014','n'])
Medals2014 <- Medals2014$n

# Perform wilconxon tests on 1998 pairs 
WT98_02 <- wilcox.test(Medals1998,Medals2002, paired=T)    #V=23, p-value=0.683

WT98_06 <- wilcox.test(Medals1998, Medals2006,paired=T)    #V=26, p-value=0.563

WT98_10 <- wilcox.test(Medals1998, Medals2010, paired=T)   #V=29, p-value=0.454

WT98_14 <- wilcox.test(Medals1998, Medals2014, paired=T)   #V=24, p-value=0.272

# Perform Wilcoxon tests on 2002 pairs
WT02_06 <- wilcox.test(Medals2002, Medals2006, paired=T)   #V=30, p-value=0.824

WT02_10 <- wilcox.test(Medals2002, Medals2010, paired=T)   #V=25, p-value=0.289

WT02_14 <- wilcox.test(Medals2002, Medals2014, paired=T)   #V=21, p-value=0.157

# Perform Wilcoxon tests on 2006 pairs
WT06_10 <- wilcox.test(Medals2006, Medals2010, paired=T)   #V=22, p-value=0.157

WT06_14 <- wilcox.test(Medals2006, Medals2014, paired=T)   #V=22, p-value=0.327

# Perform Wilcoxon test on 2010 pair
WT10_14 <- wilcox.test(Medals2010, Medals2014, paired=T)   #V=38, p-value=1.000

# No P-Value is below 0.005 significance level - could all come from the same
# sample. i.e, no change in medal distribution over these Olympiads for top 
# performers

#Testing for an error in 2010/2014
mean(Medals2010)
mean(Medals2014)              
sum(Medals2010)
sum(Medals2014)

# [3]  Logistic Regression ----------------------------------------------------

# Create Binary Target Variable -----------------------------------------------
# Use Gold Medal as a target varaible =1 whilst other medals =0

# Get names of Dataset columns and recode Medal as character
names <- names(Dataset)
Dataset$Medal <- as.character(Dataset$Medal)

# Create Gold Medal subset with flag
Dataset2 <- subset(Dataset, Dataset$Medal %in% c("Gold") )
Dataset2$Gold <- 1

# Join subset back to main dataset and set empty Gold flags to 0
dataset <- merge(Dataset,Dataset2, by=names, all.x=TRUE)
dataset[is.na(dataset)] <- 0

#Factorise Target variable and remove ID's ------------------------------------
dataset$Gold <-  factor(dataset$Gold, levels = c(0, 1))
dataset$Athlete <- NULL
dataset$Medal <- NULL
dataset$Year <- NULL
dataset$Event <- NULL      # Remove event - inclusion makes the bins too small.
# If this was to be indcluded in model, a much larger dataset would be required


# Splitting the dataset into the Training set and Test set --------------------
set.seed(123)
split <-  sample.split(dataset$Gold, SplitRatio = 0.6)
training_set <-  subset(dataset, split == TRUE)
test_set <-  subset(dataset, split == FALSE)


# Feature Scaling
training_set[6:7] <-  scale(training_set[6:7])
test_set[6:7] <-  scale(test_set[6:7])

# Fitting Logistic Regression to the Training set -----------------------------
classifier <-  glm(formula = Gold ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred <-  predict(classifier, type = 'response', newdata = test_set)
y_pred <-  ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm <-  table(test_set[, 8], y_pred > 0.5)

Accuracy1    <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])       #0.744
Error1       <- (cm[1,2]+cm[2,1])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])       #0.256
Sensitivity1 <- (cm[2,2]/(cm[2,2]+cm[1,2]))                               #0.680
Specificity1 <- (cm[1,1]/(cm[2,1]+cm[1,1]))                               #0.761
Precision1   <- (cm[2,2]/(cm[2,2]+cm[2,1]))                               #0.431 

# Looking at classifier
sumClass <- summary(classifier)

# Looking at just Population and GDP ------------------------------------------
classifier2 <-  glm(formula = Gold ~ Population + GDP,
                   family = binomial,
                   data = training_set)

sumClass2 <- summary(classifier2)

###############################################################################
# END ----------------------------------------------------------------------790

              