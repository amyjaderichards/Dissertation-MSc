library(ggplot2)
library(gridExtra)
library(ggpubr)
library(corrplot)
library(MASS)
library(waffle)

# Exploratory analysis on the 2012 data

###################################################################################################
# Single_2012 Data

# Splitting the Single_2012 dataframe into three subsets for easier plotting

# A list of all 105 genotypes
character.genotypes <- unique(as.character(Single_2012$Genotype))

threshold1 <- as.numeric(character.genotypes[35])
threshold2 <- as.numeric(character.genotypes[70])
threshold3 <- 120

subset1.2012 <- as.character(1:threshold1)
subset2.2012 <- as.character((threshold1+1):threshold2)
subset3.2012 <- c(as.character((threshold2+1):threshold3),"Gig","Gol")

subset.index1.2012 <- which(Single_2012$Genotype %in% subset1.2012)
subset.index2.2012 <- which(Single_2012$Genotype %in% subset2.2012)
subset.index3.2012 <- which(Single_2012$Genotype %in% subset3.2012)

data.subset1.2012 <- Single_2012[subset.index1.2012,]
data.subset2.2012 <- Single_2012[subset.index2.2012,]
data.subset3.2012 <- Single_2012[subset.index3.2012,]

data.subset1.2012$Genotype <- droplevels(data.subset1.2012$Genotype)
data.subset2.2012$Genotype <- droplevels(data.subset2.2012$Genotype)
data.subset3.2012$Genotype <- droplevels(data.subset3.2012$Genotype)

###################################################################################################

# Stem Diameter

# Finding the maximum and minimum stem diameter value (2.2, 8.0)
ylim <- range(Single_2012$meanStemDiam)

# Plotting stem diameters for all genotypes (multiple points per genotype)
p1 <- qplot(data.subset1.2012$Genotype, data.subset1.2012$meanStemDiam, 
            xlab="Genotype", 
            ylab="",
            ylim=c(0,10))+ggtitle("Stem Diameters for each Genotype (2012)")+geom_point(colour="mediumvioletred")+theme_gray()+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2012$Genotype, data.subset2.2012$meanStemDiam, xlab="Genotype", 
            ylab="Stem Diameter (cm)", 
            ylim=c(0,10))+geom_point(colour="mediumvioletred")+theme_gray()
p3 <- qplot(data.subset3.2012$Genotype, data.subset3.2012$meanStemDiam, xlab="Genotype", 
            ylab="", 
            ylim=c(0,10))+geom_point(colour="mediumvioletred")+theme_gray()
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# Creating an empty dataframe with a row for each genotype
# This dataframe will hold the average for each variable
condensed.data.2012 <- data.frame(character.genotypes)

# The number of genotypes (105)
N.genotypes <- length(character.genotypes)

# Creating empty columns of length N.genotypes
condensed.data.2012$StemDiameterMean <- numeric(N.genotypes)
condensed.data.2012$StemDiameterSD <- numeric(N.genotypes)
condensed.data.2012$ClumpDiameter <- numeric(N.genotypes)
condensed.data.2012$TransectCount <- numeric(N.genotypes)
condensed.data.2012$TallestStemLigule <- numeric(N.genotypes)
condensed.data.2012$TallestStemFlowerBase <- numeric(N.genotypes)
condensed.data.2012$TallestStemTrueLeaf <- numeric(N.genotypes)

#Populating the dataframe with the overall means for each genotype
for (i in unique(Single_2012$Genotype)){
  index<-which(Single_2012$Genotype==i)
  StemDiams<-as.matrix(Single_2012[index,c("StemDiam1","StemDiam2","StemDiam3")])
  ClumpDiameter <- as.matrix(Single_2012[index,c("ClumpDiam")])
  TransectCount <- as.matrix(Single_2012[index,c("TransectCount")])
  TallestStemLigule <- as.matrix(Single_2012[index,c("TallestStemLigule")])
  TallestStemFlowerBase <- as.matrix(Single_2012[index,c("TallestStemFlower")])
  TallestStemTrueLeaf <- as.matrix(Single_2012[index,c("TallestStemTrueLeaf")])
  condensed.data.2012$StemDiameterMean[which(condensed.data.2012$character.genotypes==as.character(i))] <- mean(StemDiams)
  condensed.data.2012$StemDiameterSD[which(condensed.data.2012$character.genotypes==as.character(i))] <- sd(StemDiams)
  condensed.data.2012$ClumpDiameter[which(condensed.data.2012$character.genotypes==as.character(i))] <- mean(ClumpDiameter)
  condensed.data.2012$TransectCount[which(condensed.data.2012$character.genotypes==as.character(i))] <- mean(TransectCount)
  condensed.data.2012$TallestStemLigule[which(condensed.data.2012$character.genotypes==as.character(i))] <- mean(TallestStemLigule)
  condensed.data.2012$TallestStemFlowerBase[which(condensed.data.2012$character.genotypes==as.character(i))] <- mean(TallestStemFlowerBase)
  condensed.data.2012$TallestStemTrueLeaf[which(condensed.data.2012$character.genotypes==as.character(i))] <- mean(TallestStemTrueLeaf)
}

# The only NA values in the condensed.data.2012 dataframe are from the TallestStemFlowerBase column as these are the plants that didn't reach flowering stage

# Ordering the genotypes in numerical rather than lexicographical order
character.genotypes <- factor(character.genotypes,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
condensed.data.2012$character.genotypes <- character.genotypes

###################################################################################################
# Ordering genotypes by meanStemDiam
genotypes.ordered.by.StemDiam<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2012$StemDiameterMean)])
condensed.data.2012$genotypes.ordered.by.StemDiam<-genotypes.ordered.by.StemDiam

# Ordering genotypes by meanClumpDiam
genotypes.ordered.by.ClumpDiam <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2012$ClumpDiameter)])
condensed.data.2012$genotypes.ordered.by.ClumpDiam <- genotypes.ordered.by.ClumpDiam

# Ordering genotypes by transectCount
genotypes.ordered.by.TransectCount <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2012$TransectCount)])
condensed.data.2012$genotypes.ordered.by.TransectCount <- genotypes.ordered.by.TransectCount

# Ordering genotypes by TallestStemLigule
genotypes.ordered.by.TallestStemLigule <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2012$TallestStemLigule)])
condensed.data.2012$genotypes.ordered.by.TallestStemLigule <- genotypes.ordered.by.TallestStemLigule

# Ordering genotypes by TallestStemFlowerBase
genotypes.ordered.by.TallestStemFlowerBase <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2012$TallestStemFlowerBase)])
condensed.data.2012$genotypes.ordered.by.TallestStemFlowerBase <- genotypes.ordered.by.TallestStemFlowerBase

# Ordering genotypes by TallestStemTrueLeaf
genotypes.ordered.by.TallestStemTrueLeaf <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2012$TallestStemTrueLeaf)])
condensed.data.2012$genotypes.ordered.by.TallestStemTrueLeaf <- genotypes.ordered.by.TallestStemTrueLeaf

##################################################################################################

# Stem Diameter - ORDERED
temp <- sort(condensed.data.2012$StemDiameterMean,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.StemDiam <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.StemDiam)
condensed.data.subset2.2012$genotypes.ordered.by.StemDiam <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.StemDiam)
condensed.data.subset3.2012$genotypes.ordered.by.StemDiam <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.StemDiam)

# Finding the maximum and minimum stem diameter values (2.66, 6.98)
ylim <- range(condensed.data.2012$StemDiameterMean)

# Plotting mean stem diameters for all genotypes with error bars - RANKED
p1 <-
  qplot(
    condensed.data.subset1.2012$genotypes.ordered.by.StemDiam,
    condensed.data.subset1.2012$StemDiameterMean,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 10),
  ) + geom_errorbar(
    aes(
      x = condensed.data.subset1.2012$genotypes.ordered.by.StemDiam,
      ymin = condensed.data.subset1.2012$StemDiameterMean -
        condensed.data.subset1.2012$StemDiameterSD,
      ymax = condensed.data.subset1.2012$StemDiameterMean +
        condensed.data.subset1.2012$StemDiameterSD
    ),
    width = 0.25,
    show.legend = FALSE
  ) + ggtitle("Average Stem Diameter for each Genotype (2012)") + geom_point(colour = "mediumvioletred") + theme_gray() + theme(plot.title=element_text(hjust=0.5))
p2 <-
  qplot(
    condensed.data.subset2.2012$genotypes.ordered.by.StemDiam,
    condensed.data.subset2.2012$StemDiameterMean,
    xlab = "Genotype",
    ylab = "Stem Diameter (mm)",
    ylim = c(0, 10)
  ) + geom_point(colour = "mediumvioletred") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset2.2012$genotypes.ordered.by.StemDiam,
      ymin =
        condensed.data.subset2.2012$StemDiameterMean - condensed.data.subset2.2012$StemDiameterSD,
      ymax =
        condensed.data.subset2.2012$StemDiameterMean + condensed.data.subset2.2012$StemDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
p3 <-
  qplot(
    condensed.data.subset3.2012$genotypes.ordered.by.StemDiam,
    condensed.data.subset3.2012$StemDiameterMean,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 10)
  ) + geom_point(colour = "mediumvioletred") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset3.2012$genotypes.ordered.by.StemDiam,
      ymin =
        condensed.data.subset3.2012$StemDiameterMean - condensed.data.subset3.2012$StemDiameterSD,
      ymax =
        condensed.data.subset3.2012$StemDiameterMean + condensed.data.subset3.2012$StemDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
grid.arrange(p1, p2, p3, nrow = 3)


###################################################################################################

# Clump Diameter ORDERED

temp <- sort(condensed.data.2012$ClumpDiameter,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.ClumpDiam <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.ClumpDiam)
condensed.data.subset2.2012$genotypes.ordered.by.ClumpDiam <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.ClumpDiam)
condensed.data.subset3.2012$genotypes.ordered.by.ClumpDiam <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.ClumpDiam)

# Finding the maximum and minimum clump diameter value (100, 420)
ylim <- range(condensed.data.2012$ClumpDiameter)

p1 <-
  qplot(
    condensed.data.subset1.2012$genotypes.ordered.by.ClumpDiam,
    condensed.data.subset1.2012$ClumpDiameter,
    xlab = "Genotype",
    ylab = "",
    ylim = c(50, 450),
  ) + ggtitle("Average Clump Diameter for each Genotype (2012)") + geom_point(colour="mediumseagreen") + theme(plot.title = element_text(hjust=0.5))
p2 <-
  qplot(
    condensed.data.subset2.2012$genotypes.ordered.by.ClumpDiam,
    condensed.data.subset2.2012$ClumpDiameter,
    xlab = "Genotype",
    ylab = "Clump Diameter (cm)",
    ylim = c(50, 450)
  ) + geom_point(colour = "mediumseagreen") + theme_gray()
p3 <-
  qplot(
    condensed.data.subset3.2012$genotypes.ordered.by.ClumpDiam,
    condensed.data.subset3.2012$ClumpDiameter,
    xlab = "Genotype",
    ylab = "",
    ylim = c(50, 450)
  ) + geom_point(colour = "mediumseagreen") + theme_gray() 
grid.arrange(p1, p2, p3, nrow = 3)

###################################################################################################

# Transect Count ORDERED
# Splitting the data into three subsets for easier plotting
temp <- sort(condensed.data.2012$TransectCount,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.TransectCount <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.TransectCount)
condensed.data.subset2.2012$genotypes.ordered.by.TransectCount <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.TransectCount)
condensed.data.subset3.2012$genotypes.ordered.by.TransectCount <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.TransectCount)

# Range of transect count values (4, 28)
ylim <- range(condensed.data.2012$TransectCount)

p1 <-
  qplot(
    condensed.data.subset1.2012$genotypes.ordered.by.TransectCount,
    condensed.data.subset1.2012$TransectCount,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 30),
  ) + ggtitle("Average Transect Count for each Genotype (2012)") + geom_point(colour = "darkorange3") + theme_gray() + theme(plot.title =element_text(hjust = 0.5))
p2 <-
  qplot(
    condensed.data.subset2.2012$genotypes.ordered.by.TransectCount,
    condensed.data.subset2.2012$TransectCount,
    xlab = "Genotype",
    ylab = "Stem Diameter (mm)",
    ylim = c(0, 30)
  ) + geom_point(colour = "darkorange3") + theme_gray()
p3 <-
  qplot(
    condensed.data.subset3.2012$genotypes.ordered.by.TransectCount,
    condensed.data.subset3.2012$TransectCount,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 30)
  ) + geom_point(colour = "darkorange3") + theme_gray() 
grid.arrange(p1, p2, p3, nrow = 3)


###################################################################################################

# TallestStemLigule ORDERED
temp <- sort(condensed.data.2012$TallestStemLigule,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.TallestStemLigule <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.TallestStemLigule)
condensed.data.subset2.2012$genotypes.ordered.by.TallestStemLigule <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.TallestStemLigule)
condensed.data.subset3.2012$genotypes.ordered.by.TallestStemLigule <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.TallestStemLigule)

# Range of tallest stem ligule values (14.7, 172)
ylim <- range(condensed.data.2012$TallestStemLigule)

p1 <- qplot(condensed.data.subset1.2012$genotypes.ordered.by.TallestStemLigule, condensed.data.subset1.2012$TallestStemLigule, xlab="Genotype", 
            ylab="",
            ylim=c(0, 180))+ggtitle("Average Tallest Stem Ligule for each Genotype (2012)")+geom_point(colour="darkolivegreen")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2012$genotypes.ordered.by.TallestStemLigule, condensed.data.subset2.2012$TallestStemLigule, xlab="Genotype", 
            ylab="Tallest Stem Ligule (cm)", 
            ylim=c(0,180))+geom_point(colour="darkolivegreen")
p3 <- qplot(condensed.data.subset3.2012$genotypes.ordered.by.TallestStemLigule, condensed.data.subset3.2012$TallestStemLigule, xlab="Genotype", 
            ylab="",
            ylim=c(0,180))+geom_point(colour="darkolivegreen")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# TallestStemFlower ORDERED 

# Removing NA values
condensed.flowerbase.df <- data.frame(condensed.data.2012$character.genotypes)
condensed.flowerbase.df$TallestStemFlowerBase <- condensed.data.2012$TallestStemFlowerBase
condensed.flowerbase.df$genotypes.ordered.by.tallestStemFlowerBase <- condensed.data.2012$genotypes.ordered.by.TallestStemFlowerBase
condensed.flowerbase.df <- na.omit(condensed.flowerbase.df)

# 69 rows
dim(condensed.flowerbase.df)

temp <- sort(condensed.flowerbase.df$TallestStemFlowerBase, index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:69]

condensed.data.subset1.2012 <- condensed.flowerbase.df[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.flowerbase.df[condensed.subset.index2,]

condensed.data.subset1.2012$genotypes.ordered.by.tallestStemFlowerBase <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.tallestStemFlowerBase)
condensed.data.subset2.2012$genotypes.ordered.by.tallestStemFlowerBase <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.tallestStemFlowerBase)

# Maximum and minimum value for tallest stem flowerbase (95.7, 174.3)
ylim <- range(condensed.flowerbase.df$TallestStemFlowerBase)

p1 <- qplot(condensed.data.subset1.2012$genotypes.ordered.by.tallestStemFlowerBase, condensed.data.subset1.2012$TallestStemFlowerBase, xlab="Genotype", 
            ylab="Tallest Stem Flowerbase (cm)",
            ylim=c(80, 180))+ggtitle(" Average Tallest Stem Flowerbase for each Genotype (2012)")+geom_point(colour="blue3")+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(condensed.data.subset2.2012$genotypes.ordered.by.tallestStemFlowerBase, condensed.data.subset2.2012$TallestStemFlowerBase, xlab="Genotype", 
            ylab="Tallest Stem Flowerbase (cm)", 
            ylim=c(80, 180))+geom_point(colour="blue3")
grid.arrange(p1, p2, nrow=2)

###################################################################################################

# TallestStemTrueLeaf ORDERED
temp <- sort(condensed.data.2012$TallestStemTrueLeaf,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.TallestStemTrueLeaf <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.TallestStemTrueLeaf)
condensed.data.subset2.2012$genotypes.ordered.by.TallestStemTrueLeaf <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.TallestStemTrueLeaf)
condensed.data.subset3.2012$genotypes.ordered.by.TallestStemTrueLeaf <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.TallestStemTrueLeaf)

# Range of true leaf values (14.7, 149)
ylim <- range(condensed.data.2012$TallestStemTrueLeaf, na.rm=TRUE)

p1 <- qplot(condensed.data.subset1.2012$genotypes.ordered.by.TallestStemTrueLeaf, condensed.data.subset1.2012$TallestStemTrueLeaf, xlab="Genotype", 
            ylab="",
            ylim=c(10, 160))+ggtitle("Average Tallest Stem True Leaf for each Genotype (2012)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="aquamarine4")
p2 <- qplot(condensed.data.subset2.2012$genotypes.ordered.by.TallestStemTrueLeaf, condensed.data.subset2.2012$TallestStemTrueLeaf, xlab="Genotype", 
            ylab="Tallest Stem True Leaf (cm)", 
            ylim=c(10,160))+geom_point(colour="aquamarine4")
p3 <- qplot(condensed.data.subset3.2012$genotypes.ordered.by.TallestStemTrueLeaf, condensed.data.subset3.2012$TallestStemTrueLeaf, xlab="Genotype", 
            ylab="",
            ylim=c(10,160))+geom_point(colour="aquamarine4")
grid.arrange(p1, p2, p3, nrow=3)

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

colnames(Single_2012)

# If entries aren't a y they are blank so they are changed to NA values so that the is.na function below can be used 
Single_2012$FloweringIntensityGreaterThan50[Single_2012$FloweringIntensityGreaterThan50==""] <- NA
Single_2012$FloweringIntensityGreaterThan50[Single_2012$FloweringIntensityGreaterThan50==" "] <- NA
Single_2012$FloweringIntensityLessThan50[Single_2012$FloweringIntensityLessThan50==""] <- NA
Single_2012$FloweringIntensityLessThan50[Single_2012$FloweringIntensityLessThan50==" "] <- NA
Single_2012$FloweringIntensityGreaterThan80[Single_2012$FloweringIntensityGreaterThan80==""] <- NA
Single_2012$FloweringIntensityGreaterThan80[Single_2012$FloweringIntensityGreaterThan80==" "] <- NA
Single_2012$SenescedGreaterThan80[Single_2012$SenescedGreaterThan80==""] <- NA
Single_2012$SenescedGreaterThan80[Single_2012$SenescedGreaterThan80==" "] <- NA


# FloweringIntensityLessThan50:     column 19
# FloweringIntensityGreaterThan50:  column 20
# FloweingIntensityGreaterThan80:   column 21
# SenescedGreaterThan80:            column 22

group0 <- is.na(Single_2012[19]) & is.na(Single_2012[20]) & is.na(Single_2012[21]) & is.na(Single_2012[22])
group1 <- !is.na(Single_2012[19]) & is.na(Single_2012[20]) & is.na(Single_2012[21]) & is.na(Single_2012[22])
group2 <- is.na(Single_2012[19]) & !is.na(Single_2012[20]) & is.na(Single_2012[21]) & is.na(Single_2012[22])
group3 <- is.na(Single_2012[19]) & is.na(Single_2012[20]) & !is.na(Single_2012[21]) & is.na(Single_2012[22])
group4 <- is.na(Single_2012[19]) & is.na(Single_2012[20]) & is.na(Single_2012[21]) & !is.na(Single_2012[22])
group5 <- !is.na(Single_2012[19]) & is.na(Single_2012[20]) & is.na(Single_2012[21]) & !is.na(Single_2012[22])
group6 <- is.na(Single_2012[19]) & !is.na(Single_2012[20]) & is.na(Single_2012[21]) & !is.na(Single_2012[22])
group7 <- is.na(Single_2012[19]) & is.na(Single_2012[20]) & !is.na(Single_2012[21]) & !is.na(Single_2012[22])

group.factor <- numeric(dim(Single_2012)[1])
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
Single_2012$Group <- group.factor

length(which(group0)) #0
length(which(group1)) #0
length(which(group2)) #0
length(which(group3)) #0
length(which(group4)) #0
length(which(group5)) #85
length(which(group6)) #27
length(which(group7)) #189

# Creating a waffle plot for the group distribution of the plants
vals <- c(85, 27, 189)
val_names <- sprintf("%s (%s)", c("Group 5", "Group 6", "Group 7"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
waffle::waffle(vals,colors=c("#B1AF70", "#FF6961", "#D291BC"))

###################################################################################################

# Final changes before linear modelling

# Changing values of 0 in mean stem diam to NA
Single_2012$meanStemDiam[which(Single_2012$meanStemDiam==0)] <- NA
# Changing values of 0 in transect count to NA
Single_2012$TransectCount[which(Single_2012$TransectCount==0)] <- NA

###################################################################################################

# Adding Whole.dw to single.2012 data
Single_2012 <- merge(Single_2012, Harvest_2012[,c("UID", "Whole.DW")], by="UID")
# Changing Whole.DW to Yield
names(Single_2012)[names(Single_2012) == "Whole.DW"] <- "Yield"

# A scatterplot matrix to show the relationship between the single variables
pairs(Single_2012[,c("meanStemDiam", "ClumpDiam", "TransectCount", "TallestStemLigule", "TallestStemFlower", "TallestStemTrueLeaf", "Yield")], col="slateblue4", lower.panel=NULL, cex.labels=1)

###################################################################################################
###################################################################################################

# Continuous Data

# A list of all genotypes
character.genotypes <- unique(as.character(Continuous_2012$Genotype))

# Condensed dataframe for the continuous data
condensed.data.2012.c <- data.frame(character.genotypes)

# Numebr of genotypes (105)
N.genotypes <- length(character.genotypes)

colnames(Continuous_2012)

# Filling condensed.data.2012.c with empty vectors of length N.genotypes
condensed.data.2012.c$maxShootCount <- numeric(N.genotypes)
condensed.data.2012.c$DoYMaxShootCount <- numeric(N.genotypes)
condensed.data.2012.c$DoYFirst3Emergence <- numeric(N.genotypes)
condensed.data.2012.c$maxCanopyHeight <- numeric(N.genotypes)
condensed.data.2012.c$DoYMaxCanopyHeight <- numeric(N.genotypes)
condensed.data.2012.c$FloweringStageDoYFirstA <- numeric(N.genotypes)
condensed.data.2012.c$FloweringStageDoYFirstF <- numeric(N.genotypes)

# Populating the dataframe with values
for (i in unique(Continuous_2012$Genotype)) {
  index <- which(Continuous_2012$Genotype==i)
  maxShootCounts <- as.matrix(Continuous_2012[index,c("maxShootCount")])
  DoYMaxShootCounts <- as.matrix(Continuous_2012[index,c("DoYmaxShootCount")])
  maxCanopyHeights <- as.matrix(Continuous_2012[index,c("maxCanopyHeight")])
  DoYMaxCanopyHeights <- as.matrix(Continuous_2012[index,c("DoYmaxCanopyHeight")])
  DoYFirst3Emergences <- as.matrix(Continuous_2012[index,c("DoYFirst3Emergence")])
  FloweringStageDoYFirstFs <- as.matrix(Continuous_2012[index,c("FloweringStageDoYFirstF")])
  FloweringStageDoYFirstAs <- as.matrix(Continuous_2012[index,c("FloweringStageDoYFirstA")])
  condensed.data.2012.c$maxShootCount[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(maxShootCounts)
  condensed.data.2012.c$DoYMaxShootCount[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(DoYMaxShootCounts)
  condensed.data.2012.c$maxCanopyHeight[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(maxCanopyHeights)
  condensed.data.2012.c$DoYMaxCanopyHeight[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(DoYMaxCanopyHeights)
  condensed.data.2012.c$DoYFirst3Emergence[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(DoYFirst3Emergences)
  condensed.data.2012.c$FloweringStageDoYFirstF[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(FloweringStageDoYFirstFs)
  condensed.data.2012.c$FloweringStageDoYFirstA[which(condensed.data.2012.c$character.genotypes==as.character(i))] <- mean(FloweringStageDoYFirstAs)
}

# Ordering the genotypes in numerical rather than lexicographical order
character.genotypes <- factor(character.genotypes,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
condensed.data.2012.c$character.genotypes <- character.genotypes

###################################################################################################

# Genotypes ordered by maxShootCount
genotypes.ordered.by.maxShootCount<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2012.c$maxShootCount)])
condensed.data.2012.c$genotypes.ordered.by.maxShootCount<-genotypes.ordered.by.maxShootCount

# Genotypes ordered by maxCanopyHeight
genotypes.ordered.by.maxCanopyHeight<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2012.c$maxCanopyHeight)])
condensed.data.2012.c$genotypes.ordered.by.maxCanopyHeight<-genotypes.ordered.by.maxCanopyHeight

# Genotypes ordered by DoYFirst3Emergence
genotypes.ordered.by.DoYFirst3Emergence <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2012.c$DoYFirst3Emergence)])
condensed.data.2012.c$genotypes.ordered.by.DoYFirst3Emergence <- genotypes.ordered.by.DoYFirst3Emergence

# Genotypes ordered by DoYFirstFloweringStageA
genotypes.ordered.by.FloweringStageDoYFirstA<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2012.c$FloweringStageDoYFirstA)])
condensed.data.2012.c$genotypes.ordered.by.FloweringStageDoYFirstA<-genotypes.ordered.by.FloweringStageDoYFirstA

# Genotypes ordered by DoyFirstFloweringStageF
genotypes.ordered.by.FloweringStageDoYFirstF<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2012.c$FloweringStageDoYFirstF)])
condensed.data.2012.c$genotypes.ordered.by.FloweringStageDoYFirstF<-genotypes.ordered.by.FloweringStageDoYFirstF

###################################################################################################

# Maximum Shoot Count ORDERED
temp <- sort(condensed.data.2012.c$maxShootCount,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012.c[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012.c[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012.c[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.maxShootCount <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.maxShootCount)
condensed.data.subset2.2012$genotypes.ordered.by.maxShootCount <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.maxShootCount)
condensed.data.subset3.2012$genotypes.ordered.by.maxShootCount <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.maxShootCount)

# Maximum and minimum shoot count (1.33, 39.67)
ylim <- range(condensed.data.2012.c$maxShootCount, na.rm=TRUE)

# Plotting mean max shoot count values for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2012$genotypes.ordered.by.maxShootCount, condensed.data.subset1.2012$maxShootCount, xlab="Genotype", 
            ylab="",
            ylim=c(0, 50),
            na.rm=TRUE)+ggtitle("Average Maximum Shoot Count for each Genotype (2012)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="chartreuse4")
p2 <- qplot(condensed.data.subset2.2012$genotypes.ordered.by.maxShootCount, condensed.data.subset2.2012$maxShootCount, xlab="Genotype", 
            ylab="Maximum Shoot Count", 
            ylim=c(0,50),
            na.rm=TRUE)+geom_point(colour="chartreuse4")
p3 <- qplot(condensed.data.subset3.2012$genotypes.ordered.by.maxShootCount, condensed.data.subset3.2012$maxShootCount, xlab="Genotype", 
            ylab="",
            ylim=c(0,50),
            na.rm=TRUE)+geom_point(colour="chartreuse4")
grid.arrange(p1, p2, p3, nrow=3)


###################################################################################################

# maxCanopyHeight ORDERED

temp <- sort(condensed.data.2012.c$maxCanopyHeight,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012.c[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012.c[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012.c[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.maxCanopyHeight)
condensed.data.subset2.2012$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.maxCanopyHeight)
condensed.data.subset3.2012$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.maxCanopyHeight)

# Maximum and minimum canopy height (21.7, 136.7)
ylim <- range(condensed.data.2012.c$maxCanopyHeight)

# Plotting mean max shoot count values for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset1.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="",
            ylim=c(0, 150))+ggtitle("Average Maximum Canopy Height for each Genotype (2011)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="chocolate4")
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset2.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="Maximum Canopy Height (cm)", 
            ylim=c(0,150))+geom_point(colour="chocolate4")
p3 <- qplot(condensed.data.subset3.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset3.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="",
            ylim=c(0,150))+geom_point(colour="chocolate4")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# DoYFirst3Emergence
temp <- sort(condensed.data.2012.c$DoYFirst3Emergence,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1.2012 <- condensed.data.2012.c[condensed.subset.index1,]
condensed.data.subset2.2012 <- condensed.data.2012.c[condensed.subset.index2,]
condensed.data.subset3.2012 <- condensed.data.2012.c[condensed.subset.index3,]

condensed.data.subset1.2012$genotypes.ordered.by.DoYFirst3Emergence <- droplevels(condensed.data.subset1.2012$genotypes.ordered.by.DoYFirst3Emergence)
condensed.data.subset2.2012$genotypes.ordered.by.DoYFirst3Emergence <- droplevels(condensed.data.subset2.2012$genotypes.ordered.by.DoYFirst3Emergence)
condensed.data.subset3.2012$genotypes.ordered.by.DoYFirst3Emergence <- droplevels(condensed.data.subset3.2012$genotypes.ordered.by.DoYFirst3Emergence)

# Maximum and minimum day of 3 emergence (95.7, 156.7)
ylim <- range(condensed.data.2012.c$DoYFirst3Emergence)

# Plotting DoYFirst3Emergence average for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2012$genotypes.ordered.by.DoYFirst3Emergence, condensed.data.subset1.2012$DoYFirst3Emergence, xlab="Genotype", 
            ylab="",
            ylim=c(80, 180))+ggtitle("Average Day of Year to Reach 3 Emergence for Every Genotype (2012)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="darkorchid3")
p2 <- qplot(condensed.data.subset2.2012$genotypes.ordered.by.DoYFirst3Emergence, condensed.data.subset2.2012$DoYFirst3Emergence, xlab="Genotype", 
            ylab="Day of Year", 
            ylim=c(80,180))+geom_point(colour="darkorchid3")
p3 <- qplot(condensed.data.subset3.2012$genotypes.ordered.by.DoYFirst3Emergence, condensed.data.subset3.2012$DoYFirst3Emergence, xlab="Genotype", 
            ylab="",
            ylim=c(80,180))+geom_point(colour="darkorchid3")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################
###################################################################################################

#Combining single and continuous data to use as a dataframe for a linear model

# Number of plants (302)
N.genotypes <- length(Single_2012$Genotype) #288
N.genotypes2 <- length(Continuous_2012$Genotype) #302

Single.2012.lm.df <- data.frame(Single_2012$UID)
Continuous.2012.lm.df <- data.frame(Continuous_2012$UID)

# Populating the dataframe with values
Single.2012.lm.df$UID <- Single_2012$UID
Single.2012.lm.df$Genotype <- Single_2012$Genotype
Single.2012.lm.df$StemDiameter <- Single_2012$meanStemDiam
Single.2012.lm.df$ClumpDiameter <- Single_2012$ClumpDiam
Single.2012.lm.df$TransectCount <- Single_2012$TransectCount
Single.2012.lm.df$TallestStemLigule <- Single_2012$TallestStemLigule
Single.2012.lm.df$TallestStemFlowerBase <- Single_2012$TallestStemFlower
Single.2012.lm.df$TallestStemTrueLeaf <- Single_2012$TallestStemTrueLeaf

#Continuous.2012.lm.df$Genotype <- Continuous_2012$Genotype
Continuous.2012.lm.df$UID <- Continuous_2012$UID
Continuous.2012.lm.df$MaxShootCount <- Continuous_2012$maxShootCount
Continuous.2012.lm.df$DoYmaxShootCount <- Continuous_2012$DoYmaxShootCount
Continuous.2012.lm.df$MaxCanopyHeight <- Continuous_2012$maxCanopyHeight
Continuous.2012.lm.df$DoYmaxCanopyHeight <- Continuous_2012$DoYmaxCanopyHeight
Continuous.2012.lm.df$DoYFirst3Emergence <- Continuous_2012$DoYFirst3Emergence
Continuous.2012.lm.df$FloweringStageDoYFirstA <- Continuous_2012$FloweringStageDoYFirstA
Continuous.2012.lm.df$FloweringStageDoYFirstF <- Continuous_2012$FloweringStageDoYFirstF

# Merging Single and Continuous data into one dataframe
SC.2012.lm.df <- merge(Single.2012.lm.df, Continuous.2012.lm.df[,c("UID", "MaxCanopyHeight", "DoYmaxCanopyHeight", "DoYFirst3Emergence", "FloweringStageDoYFirstA", "FloweringStageDoYFirstF")], by="UID")

# Adding yield data
SC.2012.lm.df <- merge(SC.2012.lm.df, Harvest_2012[,c("UID", "Whole.DW")], by="UID")

# Renaming column Whole.DW to Yield
names(SC.2012.lm.df)[names(SC.2012.lm.df) == "Whole.DW"] <- "Yield"

# Removing uneccessary UID column
SC.2012.lm.df <- SC.2012.lm.df[,c(1, 3:15)]

# Removing rows with NAs in it
# If we are to use a variable in a linear model, all datasets need to be the same size
# in order to compare them

# 288 rows
dim(SC.2012.lm.df)

# Removing rows that contain NA values
SC.2012.lm.df <- na.omit(SC.2012.lm.df)

# 209 rows (79 rows deleted)
dim(SC.2012.lm.df)

# Correlation plot for linear model dataframe (continuous and single)
res2 <- cor(SC.2012.lm.df[,c("StemDiameter", "ClumpDiameter", "TransectCount", "TallestStemLigule", "TallestStemTrueLeaf", "TallestStemFlowerBase", "MaxCanopyHeight", "DoYmaxCanopyHeight", "DoYFirst3Emergence", "FloweringStageDoYFirstA", "FloweringStageDoYFirstF", "Yield")])
round(res2, 2)
corrplot(res2, method="square", type="upper", order="original", tl.col="black", tl.srt=45)


# Adding yield data to continuous dataframe
Continuous_2012 <- merge(Continuous_2012, Harvest_2012[,c("UID", "Whole.DW")], by="UID")

# Changing Whole.DW to Yield
names(Continuous_2012)[names(Continuous_2012) == "Whole.DW"] <- "Yield"

# Pairs plot just for continuous
pairs(Continuous_2012[,c("maxCanopyHeight", "FloweringStageDoYFirstA", "FloweringStageDoYFirstF", "DoYFirst3Emergence", "Yield")], col="Slateblue4", cex.labels = 1.25, lower.panel = NULL)


###################################################################################################
