# Reading in the cleaned CSV files from the Data folder
# Make sure working directory is set to top level of project

wd <- "C:/Users/amyja/OneDrive/Desktop/MScDissertation"
setwd(wd)

Single_2011 <- read.csv("./Data/Single_2011_clean.csv")
Continuous_2011 <- read.csv("./Data/Continuous_2011_clean.csv")

Single_2012 <- read.csv("./Data/Single_2012_clean.csv")
Continuous_2012 <- read.csv("./Data/Continuous_2012_clean.csv")

Single_2013 <- read.csv("./Data/Single_2013_clean.csv")
Continuous_2013 <- read.csv("./Data/Continuous_2013_clean.csv")

Harvest_2012 <- read.csv("./Data/Harvest_Data_2012.csv")
Harvest_2013 <- read.csv("./Data/Harvest_Data_2013.csv")


Single_2011$Genotype<-droplevels(factor(Single_2011$Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE))
Continuous_2011$Genotype<-droplevels(factor(Continuous_2011$Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE))

Single_2012$Genotype<-droplevels(factor(Single_2012$Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE))
Continuous_2012$Genotype<-droplevels(factor(Continuous_2012$Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE))

Single_2013$Genotype<-droplevels(factor(Single_2013$Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE))
Continuous_2013$Genotype<-droplevels(factor(Continuous_2013$Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE))


