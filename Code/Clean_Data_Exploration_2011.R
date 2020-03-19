library(ggplot2)
library(gridExtra)
library(ggpubr)
library(corrplot)
library(MASS)
library(waffle)

# Exploratory analysis on the 2011 data

###################################################################################################
# Single_2011 data

# Splitting the Single_2011 dataframe into three subsets for easier plotting

# A list of all 105 genotypes
character.genotypes <- unique(as.character(Single_2011$Genotype))

# Thresholds used to split the data into three subsets
threshold1 <- as.numeric(character.genotypes[35])
threshold2 <- as.numeric(character.genotypes[70])
threshold3 <- 120

subset1 <- as.character(1:threshold1)
subset2 <- as.character((threshold1+1):threshold2)
subset3 <- c(as.character((threshold2+1):threshold3),"Gig","Gol")

subset.index1 <- which(Single_2011$Genotype %in% subset1)
subset.index2 <- which(Single_2011$Genotype %in% subset2)
subset.index3 <- which(Single_2011$Genotype %in% subset3)

data.subset1.2011 <- Single_2011[subset.index1,]
data.subset2.2011 <- Single_2011[subset.index2,]
data.subset3.2011 <- Single_2011[subset.index3,]

# droplevels is used to drop unused levels from a factor
data.subset1.2011$Genotype <- droplevels(data.subset1.2011$Genotype)
data.subset2.2011$Genotype <- droplevels(data.subset2.2011$Genotype)
data.subset3.2011$Genotype <- droplevels(data.subset3.2011$Genotype)

###################################################################################################

# Stem Diameter

# Finding the maximum and minimum stem diameter value (3.2, 9.2)
ylim <- range(Single_2011$meanStemDiam, na.rm=TRUE)

# Plotting stem diameters for all genotypes (multiple points per genotype)
p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$meanStemDiam, 
            xlab="Genotype", 
            ylab="",
            na.rm=TRUE,
            ylim=c(2,10)
            )+ggtitle("Stem Diameters for each Genotype (2011)")+geom_point(colour="mediumvioletred")+theme_gray()+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$meanStemDiam, xlab="Genotype", 
            ylab="Stem Diameter (mm)", 
            na.rm=TRUE,
            ylim=c(2,10))+geom_point(colour="mediumvioletred")+theme_gray()
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$meanStemDiam, xlab="Genotype", 
            ylab="", 
            na.rm=TRUE,
            ylim=c(2,10))+geom_point(colour="mediumvioletred")+theme_gray()+theme(axis.text.x=element_text(angle=45,hjust=1))
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# Clump Diameter

# Finding the maximum and minimum clump diameter value (0, 39)
ylim <- range(Single_2011$meanClumpDiam)

# Clump diameters for all genotypes (multiple points per genotype)
p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$meanClumpDiam, xlab="Genotype", 
            ylab ="",
            ylim=c(0,40))+ggtitle("Clump Diameters for each Genotype (2011)")+geom_point(colour="mediumseagreen")+theme_gray()+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$meanClumpDiam, xlab="Genotype", 
            ylab="Clump Diameter (cm)",
            ylim=c(0,40))+geom_point(colour="mediumseagreen")+theme_gray()
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$meanClumpDiam, xlab="Genotype", 
            ylab="",
            ylim=c(0,40))+geom_point(colour="mediumseagreen")+theme_gray()+theme(axis.text.x=element_text(angle=45,hjust=1))
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# The stem diameter and clump diameter graphs are difficult to read with multiple points per genotype
# We calculate the overall mean and the sample variance for each genotype.

# Creating an empty dataframe to hold the condensed data
condensed.data <- data.frame(character.genotypes)

# Number of genotypes (105)
N.genotypes <- length(character.genotypes)

# Populating the dataframe with empty vectors of length N.genotypes
condensed.data$StemDiameterMean <- numeric(N.genotypes)
condensed.data$StemDiameterSD <- numeric(N.genotypes)
condensed.data$ClumpDiameterMean <- numeric(N.genotypes)
condensed.data$ClumpDiameterSD <- numeric(N.genotypes)
condensed.data$TransectCount <- numeric(N.genotypes)
condensed.data$TallestStemLigule <- numeric(N.genotypes)
condensed.data$TallestStemFlowerBase <- numeric(N.genotypes)

# Populating the dataframe
for (i in unique(Single_2011$Genotype)){
  index <- which(Single_2011$Genotype==i)
  StemDiams <- as.matrix(Single_2011[index,c("StemDiam1","StemDiam2","StemDiam3")])
  ClumpDiams <- as.matrix(Single_2011[index,c("ClumpDiam1","ClumpDiam2")])
  TransectDensity <- as.matrix(Single_2011[index,c("TransectCount")])
  TallestStemLigule <- as.matrix(Single_2011[index,c("TallestStemLigule")])
  TallestStemFlowerBase <- as.matrix(Single_2011[index,c("TallestStemFlowerbase")])
  condensed.data$StemDiameterMean[which(condensed.data$character.genotypes==as.character(i))]<-mean(StemDiams)
  condensed.data$StemDiameterSD[which(condensed.data$character.genotypes==as.character(i))]<-sd(StemDiams)
  condensed.data$ClumpDiameterMean[which(condensed.data$character.genotypes==as.character(i))]<-mean(ClumpDiams)
  condensed.data$ClumpDiameterSD[which(condensed.data$character.genotypes==as.character(i))]<-sd(ClumpDiams)
  condensed.data$TransectCount[which(condensed.data$character.genotypes==as.character(i))]<-mean(TransectDensity)
  condensed.data$TallestStemLigule[which(condensed.data$character.genotypes==as.character(i))] <- mean(TallestStemLigule)
  condensed.data$TallestStemFlowerBase[which(condensed.data$character.genotypes==as.character(i))] <- mean(TallestStemFlowerBase)
}

# 1 NA value in meanStemDiam as the 3 measurements are 5.0, NA, NA 
# 2 instances of genotype 78, one with a mean of 5.12 another with mean 5.0
# Overall mean for genotype 78 is 5.06
condensed.data$StemDiameterMean[is.na(condensed.data$StemDiameterMean)] <- 5.06

# Ordering the genotypes in numerical rather than lexicographical order
character.genotypes <- factor(character.genotypes,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
condensed.data$character.genotypes <- character.genotypes

###################################################################################################

# Ordering data by meanStemDiam
genotypes.ordered.by.meanStemDiam<-factor(character.genotypes,levels=character.genotypes[order(condensed.data$StemDiameterMean)])
condensed.data$genotypes.ordered.by.meanStemDiam<-genotypes.ordered.by.meanStemDiam

# Ordering data by meanClumpDiam
genotypes.ordered.by.meanClumpDiam <- factor(character.genotypes, levels=character.genotypes[order(condensed.data$ClumpDiameterMean)])
condensed.data$genotypes.ordered.by.meanClumpDiam <- genotypes.ordered.by.meanClumpDiam

# Ordering data by transectCount
genotypes.ordered.by.transectCount <- factor(character.genotypes, levels=character.genotypes[order(condensed.data$TransectCount)])
condensed.data$genotypes.ordered.by.transectCount <- genotypes.ordered.by.transectCount

# Ordering data by TallestStemLigule
genotypes.ordered.by.tallestStemLigule <- factor(character.genotypes, levels=character.genotypes[order(condensed.data$TallestStemLigule)])
condensed.data$genotypes.ordered.by.tallestStemLigule <- genotypes.ordered.by.tallestStemLigule

# Ordering data by TallestStemFlowerBase
genotypes.ordered.by.tallestStemFlowerBase <- factor(character.genotypes, levels=character.genotypes[order(condensed.data$TallestStemFlowerBase)])
condensed.data$genotypes.ordered.by.tallestStemFlowerBase <- genotypes.ordered.by.tallestStemFlowerBase

###################################################################################################

# Stem Diameter ORDERED

# Splitting the data into three subsets for easier plotting - ORDERED
temp <- sort(condensed.data$StemDiameterMean,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2011 <- condensed.data[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.data[condensed.subset.index2,]
condensed.data.subset3.2011 <- condensed.data[condensed.subset.index3,]

condensed.data.subset1.2011$genotypes.ordered.by.meanStemDiam <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.meanStemDiam)
condensed.data.subset2.2011$genotypes.ordered.by.meanStemDiam <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.meanStemDiam)
condensed.data.subset3.2011$genotypes.ordered.by.meanStemDiam <- droplevels(condensed.data.subset3.2011$genotypes.ordered.by.meanStemDiam)

# Finding the maximum and minimum stem diameter values (4.02, 7.85)
ylim <- range(condensed.data$StemDiameterMean)

# Plotting mean stem diameters for all genotypes with error bars ORDERED
p1 <-
  qplot(
    condensed.data.subset1.2011$genotypes.ordered.by.meanStemDiam,
    condensed.data.subset1.2011$StemDiameterMean,
    xlab = "Genotype",
    ylab = "",
    ylim = c(3, 10),
  ) + geom_errorbar(
    aes(
      x = condensed.data.subset1.2011$genotypes.ordered.by.meanStemDiam,
      ymin = condensed.data.subset1.2011$StemDiameterMean -
        condensed.data.subset1.2011$StemDiameterSD,
      ymax = condensed.data.subset1.2011$StemDiameterMean +
        condensed.data.subset1.2011$StemDiameterSD
    ),
    width = 0.25,
    show.legend = FALSE) + ggtitle("Average Stem Diameter for each Genotype (2011)") + geom_point(colour = "mediumvioletred") + theme_gray() + theme(plot.title = element_text(hjust=0.5))
                                                                                                                         
p2 <-
  qplot(
    condensed.data.subset2.2011$genotypes.ordered.by.meanStemDiam,
    condensed.data.subset2.2011$StemDiameterMean,
    xlab = "Genotype",
    ylab = "Stem Diameter (mm)",
    ylim = c(3, 10)
  ) + geom_point(colour = "mediumvioletred") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset2.2011$genotypes.ordered.by.meanStemDiam,
      ymin =
        condensed.data.subset2.2011$StemDiameterMean - condensed.data.subset2.2011$StemDiameterSD,
      ymax =
        condensed.data.subset2.2011$StemDiameterMean + condensed.data.subset2.2011$StemDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
p3 <-
  qplot(
    condensed.data.subset3.2011$genotypes.ordered.by.meanStemDiam,
    condensed.data.subset3.2011$StemDiameterMean,
    xlab = "Genotype",
    ylab = "",
    ylim = c(3, 10)
  ) + geom_point(colour = "mediumvioletred") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset3.2011$genotypes.ordered.by.meanStemDiam,
      ymin =
        condensed.data.subset3.2011$StemDiameterMean - condensed.data.subset3.2011$StemDiameterSD,
      ymax =
        condensed.data.subset3.2011$StemDiameterMean + condensed.data.subset3.2011$StemDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
grid.arrange(p1, p2, p3, nrow = 3)


###################################################################################################

# Clump Diameter ORDERED

# Splitting the data into three subsets for easier plotting ORDERED
temp <- sort(condensed.data$ClumpDiameterMean,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2011 <- condensed.data[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.data[condensed.subset.index2,]
condensed.data.subset3.2011 <- condensed.data[condensed.subset.index3,]

condensed.data.subset1.2011$genotypes.ordered.by.meanClumpDiam <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.meanClumpDiam)
condensed.data.subset2.2011$genotypes.ordered.by.meanClumpDiam <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.meanClumpDiam)
condensed.data.subset3.2011$genotypes.ordered.by.meanClumpDiam <- droplevels(condensed.data.subset3.2011$genotypes.ordered.by.meanClumpDiam)

# Finding the maximum and minimum clump diameter value (2.0, 27.3)
ylim <- range(condensed.data$ClumpDiameterMean)

p1 <-
  qplot(
    condensed.data.subset1.2011$genotypes.ordered.by.meanClumpDiam,
    condensed.data.subset1.2011$ClumpDiameterMean,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 40)) + geom_errorbar(
    aes(
      x = condensed.data.subset1.2011$genotypes.ordered.by.meanClumpDiam,
      ymin = condensed.data.subset1.2011$ClumpDiameterMean -
        condensed.data.subset1.2011$ClumpDiameterSD,
      ymax = condensed.data.subset1.2011$ClumpDiameterMean +
        condensed.data.subset1.2011$ClumpDiameterSD
    ),
    width = 0.25,
    show.legend = FALSE
  ) + ggtitle("Average Clump Diameter for each Genotype (2011)") + geom_point(colour = "mediumseagreen") + theme_gray() + theme(plot.title = element_text(hjust=0.5))

p2 <-                                                                                                            
  qplot(
    condensed.data.subset2.2011$genotypes.ordered.by.meanClumpDiam,
    condensed.data.subset2.2011$ClumpDiameterMean,
    xlab = "Genotype",
    ylab = "Clump Diameter (cm)",
    ylim = c(0, 40)
  ) + geom_point(colour = "mediumseagreen") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset2.2011$genotypes.ordered.by.meanClumpDiam,
      ymin =
        condensed.data.subset2.2011$ClumpDiameterMean - condensed.data.subset2.2011$ClumpDiameterSD,
      ymax =
        condensed.data.subset2.2011$ClumpDiameterMean + condensed.data.subset2.2011$ClumpDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
p3 <-
  qplot(
    condensed.data.subset3.2011$genotypes.ordered.by.meanClumpDiam,
    condensed.data.subset3.2011$ClumpDiameterMean,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 40)
  ) + geom_point(colour = "mediumseagreen") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset3.2011$genotypes.ordered.by.meanClumpDiam,
      ymin =
        condensed.data.subset3.2011$ClumpDiameterMean - condensed.data.subset3.2011$ClumpDiameterSD,
      ymax =
        condensed.data.subset3.2011$ClumpDiameterMean + condensed.data.subset3.2011$ClumpDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
grid.arrange(p1, p2, p3, nrow = 3)

###################################################################################################

# Transect Density Count

#Finding maximum and minimum value of transect count (1, 14)
ylim <- range(Single_2011$TransectCount, na.rm=TRUE)

# Plotting all values of transect density (multiple points per genotype)
p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$TransectCount, xlab="Genotype", 
              ylab ="",
              na.rm = TRUE,
              ylim=c(0,15))+geom_point(colour="darkorange3")+ggtitle("Transect Density Count for Each Genotype (2011)")+theme_gray()+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$TransectCount, xlab="Genotype", 
              ylab="Transect Count",
              na.rm = TRUE,
              ylim=c(0,15))+geom_point(colour="darkorange3")+theme_gray()
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$TransectCount, xlab="Genotype", 
              ylab="",
              na.rm = TRUE,
              ylim=c(0,15))+geom_point(colour="darkorange3")+theme_gray()
grid.arrange(p1, p2, p3, nrow=3)

# Transect Density Count ORDERED 

# Removing NA values (2)
condensed.transect.df <- data.frame(condensed.data$character.genotypes)
condensed.transect.df$TransectCount <- condensed.data$TransectCount
condensed.transect.df$genotypes.ordered.by.transectCount <- condensed.data$genotypes.ordered.by.transectCount
condensed.transect.df <- na.omit(condensed.transect.df)

temp <- sort(condensed.transect.df$TransectCount, index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:102]

condensed.data.subset1.2011 <- condensed.transect.df[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.transect.df[condensed.subset.index2,]
condensed.data.subset3.2011 <- condensed.transect.df[condensed.subset.index3,]

condensed.data.subset1.2011$genotypes.ordered.by.transectCount <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.transectCount)
condensed.data.subset2.2011$genotypes.ordered.by.transectCount <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.transectCount)
condensed.data.subset3.2011$genotypes.ordered.by.transectCount <- droplevels(condensed.data.subset3.2011$genotypes.ordered.by.transectCount)

#Finding maximum and minimum value of mean transect count (1.33, 12.67)
ylim <- range(condensed.transect.df$TransectCount)

p1 <-
  qplot(
    condensed.data.subset1.2011$genotypes.ordered.by.transectCount,
    condensed.data.subset1.2011$TransectCount,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 15),
    na.rm = TRUE
  ) + ggtitle("Average Transect Count for each Genotype (2011)") + geom_point(colour = "darkorange3") + theme_gray() + theme(plot.title = element_text(hjust=0.5))
                                                                                                                            
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.transectCount,
            condensed.data.subset2.2011$TransectCount,
            xlab="Genotype",
            ylab="Transect Count",
            ylim=c(0,15),
            na.rm=TRUE) + geom_point(colour="darkorange3") + theme_gray()

p3 <- qplot(condensed.data.subset3.2011$genotypes.ordered.by.transectCount,
            condensed.data.subset3.2011$TransectCount,
            xlab="Genotype",
            ylab="",
            ylim=c(0,15),
            na.rm=TRUE) + geom_point(colour="darkorange3") + theme_gray()
grid.arrange(p1, p2, p3, nrow = 3)

###################################################################################################

# Tallest Stem Ligule

# Finding the maximum and miniumum tallest stem ligule (4.0, 125.0) 
ylim <- range(Single_2011$TallestStemLigule)

#Plotting all tallest stem ligule values for all genotypes (multiple points per genotype)
p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$TallestStemLigule, xlab="Genotype", 
              ylab="",
              ylim=c(0, 150))+ggtitle("Tallest Stem Ligules for each Genotype (2011)")+geom_point(colour="darkolivegreen")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$TallestStemLigule, xlab="Genotype", 
              ylab="Tallest Stem Ligule (cm)", 
              ylim=c(0,150))+geom_point(colour="darkolivegreen")
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$TallestStemLigule, xlab="Genotype", 
              ylab="",
              ylim=c(0,150))+geom_point(colour="darkolivegreen")
grid.arrange(p1, p2, p3, nrow=3)

# Ordering the data
# Splitting the data into three subsets for easier plotting
temp <- sort(condensed.data$TallestStemLigule,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2011 <- condensed.data[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.data[condensed.subset.index2,]
condensed.data.subset3.2011 <- condensed.data[condensed.subset.index3,]

condensed.data.subset1.2011$genotypes.ordered.by.tallestStemLigule <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.tallestStemLigule)
condensed.data.subset2.2011$genotypes.ordered.by.tallestStemLigule <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.tallestStemLigule)
condensed.data.subset3.2011$genotypes.ordered.by.tallestStemLigule <- droplevels(condensed.data.subset3.2011$genotypes.ordered.by.tallestStemLigule)

# Finding maximum and minumum tallest stem ligule for condensed data (7.75, 111)
ylim <- range(condensed.data$TallestStemLigule)

# Plotting mean tallest stem ligule values for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.tallestStemLigule, condensed.data.subset1.2011$TallestStemLigule, xlab="Genotype", 
            ylab="",
            ylim=c(0, 120))+ggtitle("Average Tallest Stem Ligule for each Genotype (2011)")+geom_point(colour="darkolivegreen")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.tallestStemLigule, condensed.data.subset2.2011$TallestStemLigule, xlab="Genotype", 
            ylab="Tallest Stem Ligule (cm)", 
            ylim=c(0,120))+geom_point(colour="darkolivegreen")
p3 <- qplot(condensed.data.subset3.2011$genotypes.ordered.by.tallestStemLigule, condensed.data.subset3.2011$TallestStemLigule, xlab="Genotype", 
            ylab="",
            ylim=c(0,120))+geom_point(colour="darkolivegreen")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# Tallest Stem Flowerbase

# Finding maximum and minimum values for tallest stem flowerbase (29.0, 124.0)
ylim <- range(Single_2011$TallestStemFlowerbase, na.rm=TRUE)

# Plotting all tallest stem flowerbase values for all genotypes (multiple points per genotype)
p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$TallestStemFlowerbase, xlab="Genotype", 
            ylab="",
            ylim=c(25, 130),
            na.rm=TRUE)+ggtitle("Tallest Stem Flowerbases for each Genotype (2011)")+geom_point(colour="blue3")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$TallestStemFlowerbase, xlab="Genotype", 
            ylab="Tallest Stem Flowerbase (cm)", 
            ylim=c(25, 130))+geom_point(colour="blue3")
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$TallestStemFlowerbase, xlab="Genotype", 
            ylab="", 
            ylim=c(25, 130))+geom_point(colour="blue3")
grid.arrange(p1, p2, p3, nrow=3)

# Removing NA values
condensed.flowerbase.df <- data.frame(condensed.data$character.genotypes)
condensed.flowerbase.df$TallestStemFlowerBase <- condensed.data$TallestStemFlowerBase
condensed.flowerbase.df$genotypes.ordered.by.tallestStemFlowerBase <- condensed.data$genotypes.ordered.by.tallestStemFlowerBase
condensed.flowerbase.df <- na.omit(condensed.flowerbase.df)

# 56 rows
dim(condensed.flowerbase.df)

temp <- sort(condensed.flowerbase.df$TallestStemFlowerBase, index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:28]
condensed.subset.index2 <- temp[29:56]

condensed.data.subset1.2011 <- condensed.flowerbase.df[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.flowerbase.df[condensed.subset.index2,]

condensed.data.subset1.2011$genotypes.ordered.by.tallestStemFlowerBase <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.tallestStemFlowerBase)
condensed.data.subset2.2011$genotypes.ordered.by.tallestStemFlowerBase <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.tallestStemFlowerBase)

# Maximum and minimum value for tallest stem flowerbase (52.7, 106.2)
ylim <- range(condensed.flowerbase.df$TallestStemFlowerBase)

p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.tallestStemFlowerBase, condensed.data.subset1.2011$TallestStemFlowerBase, xlab="Genotype", 
            ylab="Tallest Stem Flowerbase (cm)",
            ylim=c(40, 120))+ggtitle(" Average Tallest Stem Flowerbase for each Genotype (2011)")+geom_point(colour="blue3")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.tallestStemFlowerBase, condensed.data.subset2.2011$TallestStemFlowerBase, xlab="Genotype", 
            ylab="Tallest Stem Flowerbase (cm)", 
            ylim=c(40, 120))+geom_point(colour="blue3")
grid.arrange(p1, p2, nrow=3)

###################################################################################################
###################################################################################################

# FloweringIntensityLessThan50, FloweringIntensityGreaterThan50
# FloweringIntensityGreaterThan80, SenescedGreaterThan80

"""
There are 8 possible combinations of the four variables:
- FloweringIntensityLessThan50
- FloweringIntensityGreaterThan50
- FloweringIntensityGreaterThan80
- SenescedGreaterThan80

<50     >50     >80     S<80
NA      NA      NA      NA
Y       NA      NA      NA
NA      Y       NA      NA
NA      NA      Y       NA
NA      NA      NA      Y
Y       NA      NA      Y
NA      Y       NA      Y
NA      NA      Y       Y

We will categorise the genotypes based off these 8 classifications
"""

colnames(Single_2011)

# FloweringIntensityLessThan50:     column 19
# FloweringIntensityGreaterThan50:  column 20
# FloweingIntensityGreaterThan80:   column 21
# SenescedGreaterThan80:            column 22

group0 <- is.na(Single_2011[19]) & is.na(Single_2011[20]) & is.na(Single_2011[21]) & is.na(Single_2011[22])
group1 <- !is.na(Single_2011[19]) & is.na(Single_2011[20]) & is.na(Single_2011[21]) & is.na(Single_2011[22])
group2 <- is.na(Single_2011[19]) & !is.na(Single_2011[20]) & is.na(Single_2011[21]) & is.na(Single_2011[22])
group3 <- is.na(Single_2011[19]) & is.na(Single_2011[20]) & !is.na(Single_2011[21]) & is.na(Single_2011[22])
group4 <- is.na(Single_2011[19]) & is.na(Single_2011[20]) & is.na(Single_2011[21]) & !is.na(Single_2011[22])
group5 <- !is.na(Single_2011[19]) & is.na(Single_2011[20]) & is.na(Single_2011[21]) & !is.na(Single_2011[22])
group6 <- is.na(Single_2011[19]) & !is.na(Single_2011[20]) & is.na(Single_2011[21]) & !is.na(Single_2011[22])
group7 <- is.na(Single_2011[19]) & is.na(Single_2011[20]) & !is.na(Single_2011[21]) & !is.na(Single_2011[22])

group.factor <- numeric(dim(Single_2011)[1])
group.factor[group0] <- 0
group.factor[group1] <- 1
group.factor[group2] <- 2
group.factor[group3] <- 3
group.factor[group4] <- 4
group.factor[group5] <- 5
group.factor[group6] <- 6
group.factor[group7] <- 7
group.factor <- factor(group.factor,levels=0:7,ordered=TRUE)

#Adding the group number into a new column called Group in the Single_2011 DataFrame
Single_2011$Group <- group.factor

length(which(group0)) #87
length(which(group1)) #89
length(which(group2)) #48
length(which(group3)) #32
length(which(group4)) #17
length(which(group5)) #8
length(which(group6)) #9
length(which(group7)) #12

# Creating a waffle plot of the group distribution of plants 
vals <- c(87, 89, 48, 32, 17, 8, 9, 12)
val_names <- sprintf("%s (%s)", c("Group 0", "Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6", "Group7"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
waffle::waffle(vals, colors=c("#F8B8B3", "#F7FDAF", "#AAF3A2", "#92DEFC", "#AEC5F1", "#B1AF70", "#FF6961", "#D291BC")) 
  
###################################################################################################

# Final data cleanup before linear modelling
Single_2011$meanClumpDiam[which(Single_2011$meanClumpDiam==0.0)]<-NA

###################################################################################################

# A scatter plot matrix shows correlation between variables using the pairs() function
pairs(Single_2011[,c("meanStemDiam", "meanClumpDiam", "TransectCount", "TallestStemLigule", "TallestStemFlowerbase")], col="deeppink4", cex.labels = 1.25, lower.panel=NULL)

# A correlation plot shows the correlation between variables
res <- cor(Single_2011[,c("meanStemDiam", "meanClumpDiam", "TransectCount", "TallestStemLigule", "TallestStemFlowerbase")], use="pairwise.complete.obs")
round(res,2)
corrplot(res, type="upper", order="hclust", tl.col="black", tl.srt=45)


# We don't build a linear model from the 2011 data as there is no 
# Y variable (Yield) to regress upon

###################################################################################################
###################################################################################################

# Continuous_2011 data
colnames(Continuous_2011)

# A list of all genotypes (identical to the single data)
character.genotypes <- unique(as.character(Continuous_2011$Genotype))

# Splitting the continuous data into three subsets for easier plotting
threshold1 <- as.numeric(character.genotypes[35])
threshold2 <- as.numeric(character.genotypes[70])
threshold3 <- 120

subset1 <- as.character(1:threshold1)
subset2 <- as.character((threshold1+1):threshold2)
subset3 <- c(as.character((threshold2+1):threshold3),"Gig","Gol")

subset.index1 <- which(Continuous_2011$Genotype %in% subset1)
subset.index2 <- which(Continuous_2011$Genotype %in% subset2)
subset.index3 <- which(Continuous_2011$Genotype %in% subset3)

data.subset1.2011 <- Continuous_2011[subset.index1,]
data.subset2.2011 <- Continuous_2011[subset.index2,]
data.subset3.2011 <- Continuous_2011[subset.index3,]

data.subset1.2011$Genotype <- droplevels(data.subset1.2011$Genotype)
data.subset2.2011$Genotype <- droplevels(data.subset2.2011$Genotype)
data.subset3.2011$Genotype <- droplevels(data.subset3.2011$Genotype)

###################################################################################################

# Maximum Shoot Count

# Range of shoot count values (2.0, 223.0)
ylim <- range(Continuous_2011$maxShootCount)

p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$maxShootCount, xlab="Genotype", 
            ylab ="",
            ylim=c(0,250))+ggtitle("Maximum Shoot Count for each Genotype (2011)")+geom_point(colour="purple3")+theme_gray()+theme(plot.title = element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$maxShootCount, xlab="Genotype", 
            ylab="Maximum Shoot Count",
            ylim=c(0,250))+geom_point(colour="purple3")+theme_gray()
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$maxShootCount, xlab="Genotype", 
            ylab="",
            ylim=c(0,250))+geom_point(colour="purple3")+theme_gray()+theme(axis.text.x=element_text(angle=45,hjust=1))
grid.arrange(p1, p2, p3, nrow=3)


###################################################################################################

# Maximum Canopy Height

#Range of canopy height values (5,80)
ylim <- range(Continuous_2011$maxCanopyHeight)

p1 <- qplot(data.subset1.2011$Genotype, data.subset1.2011$maxCanopyHeight, xlab="Genotype", 
            ylim=c(0,100),
            ylab="")+ggtitle("Maximum Canopy Heights for each Genotype (2011)")+geom_point(colour="chocolate4")+theme_gray()+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2011$Genotype, data.subset2.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="Maximum Canopy Height (cm)",
            ylim=c(0,100))+geom_point(colour="chocolate4")+theme_gray()
p3 <- qplot(data.subset3.2011$Genotype, data.subset3.2011$maxShootCount, xlab="Genotype", 
            ylim=c(0,100),
            ylab="")+geom_point(colour="chocolate4")+theme_gray()+theme(axis.text.x=element_text(angle=45,hjust=1))
grid.arrange(p1, p2, p3, nrow=3)


###################################################################################################
###################################################################################################

# Condensed dataframe for continuous data 
condensed.data.c <- data.frame(character.genotypes)

# Number of genotypes (105)
N.genotypes <- length(character.genotypes)

#Populating the dataframe with empty vectors of length N.genotypes
condensed.data.c$maxShootCount <- numeric(N.genotypes)
condensed.data.c$DoYMaxShootCount <- numeric(N.genotypes)
condensed.data.c$maxCanopyHeight <- numeric(N.genotypes)
condensed.data.c$DoYMaxCanopyHeight <- numeric(N.genotypes)
condensed.data.c$FloweringStageDoYFirstA <- numeric(N.genotypes)
condensed.data.c$FloweringStageDoYFirstF <- numeric(N.genotypes)

# Populating the dataframe
for (i in unique(Continuous_2011$Genotype)) {
  index <- which(Continuous_2011$Genotype==i)
  maxShootCounts <- as.matrix(Continuous_2011[index,c("maxShootCount")])
  DoYMaxShootCounts <- as.matrix(Continuous_2011[index,c("DoYmaxShootCount")])
  maxCanopyHeights <- as.matrix(Continuous_2011[index,c("maxCanopyHeight")])
  DoYMaxCanopyHeights <- as.matrix(Continuous_2011[index,c("DoYmaxCanopyHeight")])
  FloweringStageDoYFirstFs <- as.matrix(Continuous_2011[index,c("FloweringStageDoYFirstF")])
  FloweringStageDoYFirstAs <- as.matrix(Continuous_2011[index,c("FloweringStageDoYFirstA")])
  condensed.data.c$maxShootCount[which(condensed.data.c$character.genotypes==as.character(i))] <- mean(maxShootCounts)
  condensed.data.c$DoYMaxShootCount[which(condensed.data.c$character.genotypes==as.character(i))] <- mean(DoYMaxShootCounts)
  condensed.data.c$maxCanopyHeight[which(condensed.data.c$character.genotypes==as.character(i))] <- mean(maxCanopyHeights)
  condensed.data.c$DoYMaxCanopyHeight[which(condensed.data.c$character.genotypes==as.character(i))] <- mean(DoYMaxCanopyHeights)
  condensed.data.c$FloweringStageDoYFirstF[which(condensed.data.c$character.genotypes==as.character(i))] <- mean(FloweringStageDoYFirstFs)
  condensed.data.c$FloweringStageDoYFirstA[which(condensed.data.c$character.genotypes==as.character(i))] <- mean(FloweringStageDoYFirstAs)
}

# Ordering the genotypes in numerical rather than lexicographical order
character.genotypes <- factor(character.genotypes,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
condensed.data.c$character.genotypes <- character.genotypes

###################################################################################################

# Ordering genotypes by maxShootCount
genotypes.ordered.by.maxShootCount<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.c$maxShootCount)])
condensed.data.c$genotypes.ordered.by.maxShootCount<-genotypes.ordered.by.maxShootCount

# Ordering genotypes by maxCanopyHeight
genotypes.ordered.by.maxCanopyHeight<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.c$maxCanopyHeight)])
condensed.data.c$genotypes.ordered.by.maxCanopyHeight<-genotypes.ordered.by.maxCanopyHeight

# Ordering genotypes by DoYFirstFloweringStageA
genotypes.ordered.by.FloweringStageDoYFirstA<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.c$FloweringStageDoYFirstA)])
condensed.data.c$genotypes.ordered.by.FloweringStageDoYFirstA<-genotypes.ordered.by.FloweringStageDoYFirstA

# Ordering genotypes by DoyFirstFloweringStageF
genotypes.ordered.by.FloweringStageDoYFirstF<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.c$FloweringStageDoYFirstF)])
condensed.data.c$genotypes.ordered.by.FloweringStageDoYFirstF<-genotypes.ordered.by.FloweringStageDoYFirstF

###################################################################################################

# Max Shoot Count - ORDERED
temp <- sort(condensed.data.c$maxShootCount,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2011 <- condensed.data.c[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.data.c[condensed.subset.index2,]
condensed.data.subset3.2011 <- condensed.data.c[condensed.subset.index3,]

condensed.data.subset1.2011$genotypes.ordered.by.maxShootCount <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.maxShootCount)
condensed.data.subset2.2011$genotypes.ordered.by.maxShootCount <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.maxShootCount)
condensed.data.subset3.2011$genotypes.ordered.by.maxShootCount <- droplevels(condensed.data.subset3.2011$genotypes.ordered.by.maxShootCount)

# Maximum and minimum shoot count (6, 158)
ylim <- range(condensed.data.c$maxShootCount)

# Plotting mean max shoot count values for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.maxShootCount, condensed.data.subset1.2011$maxShootCount, xlab="Genotype", 
            ylab="",
            ylim=c(0, 175))+ggtitle("Average Maximum Shoot Count for each Genotype (2011)")+geom_point(colour="purple3")+theme(plot.title = element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.maxShootCount, condensed.data.subset2.2011$maxShootCount, xlab="Genotype", 
            ylab="Maximum Shoot Count", 
            ylim=c(0,175))+geom_point(colour="purple3")
p3 <- qplot(condensed.data.subset3.2011$genotypes.ordered.by.maxShootCount, condensed.data.subset3.2011$maxShootCount, xlab="Genotype", 
            ylab="",
            ylim=c(0,175))+geom_point(colour="purple3")
grid.arrange(p1, p2, p3, nrow=3)


###################################################################################################

# Max Canopy Height - ORDERED
temp <- sort(condensed.data.c$maxCanopyHeight,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2011 <- condensed.data.c[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.data.c[condensed.subset.index2,]
condensed.data.subset3.2011 <- condensed.data.c[condensed.subset.index3,]

condensed.data.subset1.2011$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.maxCanopyHeight)
condensed.data.subset2.2011$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.maxCanopyHeight)
condensed.data.subset3.2011$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset3.2011$genotypes.ordered.by.maxCanopyHeight)

# Maximum and minimum max canopy height (5, 75)
ylim <- range(condensed.data.c$maxCanopyHeight)

# Plotting mean max canopy height values for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset1.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="",
            ylim=c(0, 100))+ggtitle("Average Maximum Canopy Height for each Genotype (2011)")+geom_point(colour="chocolate4")+theme(plot.title = element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset2.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="Maximum Canopy Height (cm)", 
            ylim=c(0,100))+geom_point(colour="chocolate4")
p3 <- qplot(condensed.data.subset3.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset3.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="",
            ylim=c(0,100))+geom_point(colour="chocolate4")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# Stage F is more useful to plot than stage A
# FloweringStageDoYFirstF ORDERED

# Removing NAs from condensed data
condensed.stagef.df <- data.frame(condensed.data.c$character.genotypes)
condensed.stagef.df$FloweringStageDoYFirstF <- condensed.data.c$FloweringStageDoYFirstF
condensed.stagef.df$genotypes.ordered.by.floweringStageDoYFirstF <- condensed.data.c$genotypes.ordered.by.FloweringStageDoYFirstF
condensed.stagef.df <- na.omit(condensed.stagef.df)

# 55 rows
dim(condensed.stagef.df)

temp <- sort(condensed.stagef.df$FloweringStageDoYFirstF, index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:27]
condensed.subset.index2 <- temp[28:55]

condensed.data.subset1.2011 <- condensed.stagef.df[condensed.subset.index1,]
condensed.data.subset2.2011 <- condensed.stagef.df[condensed.subset.index2,]

condensed.data.subset1.2011$genotypes.ordered.by.floweringStageDoYFirstF <- droplevels(condensed.data.subset1.2011$genotypes.ordered.by.floweringStageDoYFirstF)
condensed.data.subset2.2011$genotypes.ordered.by.floweringStageDoYFirstF <- droplevels(condensed.data.subset2.2011$genotypes.ordered.by.floweringStageDoYFirstF)

# (221, 285)
ylim <- range(condensed.stagef.df$FloweringStageDoYFirstF)

p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.floweringStageDoYFirstF, condensed.data.subset1.2011$FloweringStageDoYFirstF, 
            xlab="Genotype", 
            ylab="Day of Year",
            ylim=c(200, 300))+ggtitle(" Average Day of Year to Reach Flowering Stage F for each Genotype (2011)")+geom_point(colour="goldenrod2")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.floweringStageDoYFirstF, condensed.data.subset2.2011$FloweringStageDoYFirstF, xlab="Genotype", 
            ylab="Day of Year", 
            ylim=c(200, 300))+geom_point(colour="goldenrod2")
grid.arrange(p1, p2, nrow=2)

###################################################################################################

# A scatter plot matrix to show correlation between the continuous variables using the pairs() function
pairs(Continuous_2011[,c("maxCanopyHeight", "FloweringStageDoYFirstA", "FloweringStageDoYFirstF")], col="deeppink4", cex.labels=1.5, lower.panel=NULL)
