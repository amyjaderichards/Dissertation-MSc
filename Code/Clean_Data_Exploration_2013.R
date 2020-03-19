install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggpubr")

library(ggplot2)
library(gridExtra)
library(ggpubr)

###################################################################################################

# Single_2013 Data

# 302 rows, 20 columns
dim(Single_2013)

# Splitting the Single_2013 dataframe into three subsets for easier plotting

character.genotypes <- unique(as.character(Single_2013$Genotype))

threshold1 <- as.numeric(character.genotypes[35])
threshold2 <- as.numeric(character.genotypes[70])
threshold3 <- 120

subset1.2013 <- as.character(1:threshold1)
subset2.2013 <- as.character((threshold1+1):threshold2)
subset3.2013 <- c(as.character((threshold2+1):threshold3),"Gig","Gol")

subset.index1.2013 <- which(Single_2013$Genotype %in% subset1.2013)
subset.index2.2013 <- which(Single_2013$Genotype %in% subset2.2013)
subset.index3.2013 <- which(Single_2013$Genotype %in% subset3.2013)

data.subset1.2013 <- Single_2013[subset.index1.2013,]
data.subset2.2013 <- Single_2013[subset.index2.2013,]
data.subset3.2013 <- Single_2013[subset.index3.2013,]

data.subset1.2013$Genotype <- droplevels(data.subset1.2013$Genotype)
data.subset2.2013$Genotype <- droplevels(data.subset2.2013$Genotype)
data.subset3.2013$Genotype <- droplevels(data.subset3.2013$Genotype)

###################################################################################################

# Stem Diameter

# Finding the maximum and minimum stem diameter value (2.2, 9.1)
ylim<-range(Single_2013$meanStemDiam)

# Plotting all mean stem diameter values (multiple points per genotype)
p1 <- qplot(data.subset1.2013$Genotype, data.subset1.2013$meanStemDiam, 
            xlab="Genotype", 
            ylab="",
            ylim=c(0,10))+ggtitle("Stem Diameters for each Genotype (2013)")+geom_point(colour="mediumvioletred")+theme_gray()+theme(plot.title=element_text(hjust=0.5))
p2 <- qplot(data.subset2.2013$Genotype, data.subset2.2013$meanStemDiam, xlab="Genotype", 
            ylab="Stem Diameter (mm)", 
            ylim=c(0,10))+geom_point(colour="mediumvioletred")+theme_gray()
p3 <- qplot(data.subset3.2013$Genotype, data.subset3.2013$meanStemDiam, xlab="Genotype", 
            ylab="", 
            ylim=c(0,10))+geom_point(colour="mediumvioletred")+theme_gray()+theme(axis.text.x=element_text(angle=45,hjust=1))
grid.arrange(p1, p2, p3, nrow=3)

# Are the unordered plots even useful?

###################################################################################################

# Condensed data frame to hold average values for each genotype for each single variable
condensed.data.2013 <- data.frame(character.genotypes)

# The number of genotypes (105)
N.genotypes <- length(character.genotypes)

# Creating empty columns of length N.genotypes
condensed.data.2013$StemDiameter <- numeric(N.genotypes)
condensed.data.2013$StemDiameterSD <- numeric(N.genotypes)
condensed.data.2013$ClumpDiameter <- numeric(N.genotypes)
condensed.data.2013$TransectCount <- numeric(N.genotypes)
condensed.data.2013$TallestStemLigule <- numeric(N.genotypes)
condensed.data.2013$TallestStemFlowerBase <- numeric(N.genotypes)
condensed.data.2013$TallestStemTrueLeaf <- numeric(N.genotypes)

#Populating the dataframe
for (i in unique(Single_2013$Genotype)){
  index<-which(Single_2013$Genotype==i)
  StemDiams<-as.matrix(Single_2013[index,c("StemDiam1","StemDiam2","StemDiam3")])
  ClumpDiameter <- as.matrix(Single_2013[index,c("ClumpDiam")])
  TransectCount <- as.matrix(Single_2013[index,c("TransectCount")])
  TallestStemLigule <- as.matrix(Single_2013[index,c("TallestStemLigule")])
  TallestStemFlowerBase <- as.matrix(Single_2013[index,c("TallestStemFlower")])
  TallestStemTrueLeaf <- as.matrix(Single_2013[index,c("TallestStemTrueLeaf")])
  condensed.data.2013$StemDiameter[which(condensed.data.2013$character.genotypes==as.character(i))] <- mean(StemDiams)
  condensed.data.2013$StemDiameterSD[which(condensed.data.2013$character.genotypes==as.character(i))] <- sd(StemDiams)
  condensed.data.2013$ClumpDiameter[which(condensed.data.2013$character.genotypes==as.character(i))] <- mean(ClumpDiameter)
  condensed.data.2013$TransectCount[which(condensed.data.2013$character.genotypes==as.character(i))] <- mean(TransectCount)
  condensed.data.2013$TallestStemLigule[which(condensed.data.2013$character.genotypes==as.character(i))] <- mean(TallestStemLigule)
  condensed.data.2013$TallestStemFlowerBase[which(condensed.data.2013$character.genotypes==as.character(i))] <- mean(TallestStemFlowerBase)
  condensed.data.2013$TallestStemTrueLeaf[which(condensed.data.2013$character.genotypes==as.character(i))] <- mean(TallestStemTrueLeaf)
}

# The only NA values in the condensed data are in the TallestStemFlowerBase column as expected

###################################################################################################

# The order of the genotypes by the variables

# Genotype order by StemDiam
genotypes.ordered.by.StemDiam<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2013$StemDiameter)])
condensed.data.2013$genotypes.ordered.by.StemDiam<-genotypes.ordered.by.StemDiam

# Genotype order by ClumpDiam
genotypes.ordered.by.ClumpDiam <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013$ClumpDiameter)])
condensed.data.2013$genotypes.ordered.by.ClumpDiam <- genotypes.ordered.by.ClumpDiam

# Genotype order by TransectCount
genotypes.ordered.by.TransectCount <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013$TransectCount)])
condensed.data.2013$genotypes.ordered.by.TransectCount <- genotypes.ordered.by.TransectCount

# Genotype order by TallestStemLigule
genotypes.ordered.by.TallestStemLigule <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013$TallestStemLigule)])
condensed.data.2013$genotypes.ordered.by.TallestStemLigule <- genotypes.ordered.by.TallestStemLigule

# Genotype order by TallestStemFlowerBase
genotypes.ordered.by.TallestStemFlowerBase <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013$TallestStemFlowerBase)])
condensed.data.2013$genotypes.ordered.by.TallestStemFlowerBase <- genotypes.ordered.by.TallestStemFlowerBase

# Genotype order by TallestStemTrueLeaf
genotypes.ordered.by.TallestStemTrueLeaf <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013$TallestStemTrueLeaf)])
condensed.data.2013$genotypes.ordered.by.TallestStemTrueLeaf <- genotypes.ordered.by.TallestStemTrueLeaf

genotypes.ordered.by.FloweringStageDoYFirstF <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013.c$FloweringStageDoYFirstF)])
condensed.data.2013.c$genotypes.ordered.by.FloweringStageDoYFirstF <- genotypes.ordered.by.FloweringStageDoYFirstF
###################################################################################################

#Plotting the overall mean stem diameter values for 2013

# Stem Diameter ORDERED

temp<-sort(condensed.data.2013$StemDiameter,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.StemDiam <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.StemDiam)
condensed.data.subset2.2013$genotypes.ordered.by.StemDiam <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.StemDiam)
condensed.data.subset3.2013$genotypes.ordered.by.StemDiam <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.StemDiam)

# Maximum and minimum value for average stem diameter (2.82, 7.75)
ylim <- range(condensed.data.2013$StemDiameter)

# Average stem diameter for each genotype with error bars
p1 <-
  qplot(
    condensed.data.subset1.2013$genotypes.ordered.by.StemDiam,
    condensed.data.subset1.2013$StemDiameter,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 10),
  ) + geom_errorbar(
    aes(
      x = condensed.data.subset1.2013$genotypes.ordered.by.StemDiam,
      ymin = condensed.data.subset1.2013$StemDiameter -
        condensed.data.subset1.2013$StemDiameterSD,
      ymax = condensed.data.subset1.2013$StemDiameter +
        condensed.data.subset1.2013$StemDiameterSD
    ),
    width = 0.25,
    show.legend = FALSE
  ) + ggtitle("Average Stem Diameter for each Genotype (2013)") + theme(plot.title =
                                                                          element_text(hjust = 0.5), axis.text.x=element_text(angle=45)) + geom_point(colour = "mediumvioletred") + theme_gray()
p2 <-
  qplot(
    condensed.data.subset2.2013$genotypes.ordered.by.StemDiam,
    condensed.data.subset2.2013$StemDiameter,
    xlab = "Genotype",
    ylab = "Stem Diameter (mm)",
    ylim = c(0, 10)
  ) + geom_point(colour = "mediumvioletred") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset2.2013$genotypes.ordered.by.StemDiam,
      ymin =
        condensed.data.subset2.2013$StemDiameter - condensed.data.subset2.2013$StemDiameterSD,
      ymax =
        condensed.data.subset2.2013$StemDiameter + condensed.data.subset2.2013$StemDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
p3 <-
  qplot(
    condensed.data.subset3.2013$genotypes.ordered.by.StemDiam,
    condensed.data.subset3.2013$StemDiameter,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 10)
  ) + geom_point(colour = "mediumvioletred") + theme_gray() + geom_errorbar(
    aes(
      x = condensed.data.subset3.2013$genotypes.ordered.by.StemDiam,
      ymin =
        condensed.data.subset3.2013$StemDiameter - condensed.data.subset3.2013$StemDiameterSD,
      ymax =
        condensed.data.subset3.2013$StemDiameter + condensed.data.subset3.2013$StemDiameterSD
    ),
    width = 0.25,
    show.legend =
      FALSE
  )
grid.arrange(p1, p2, p3, nrow = 3)


###################################################################################################

# ClumpDiameter ORDERED
temp<-sort(condensed.data.2013$ClumpDiameter,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.ClumpDiam <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.ClumpDiam)
condensed.data.subset2.2013$genotypes.ordered.by.ClumpDiam <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.ClumpDiam)
condensed.data.subset3.2013$genotypes.ordered.by.ClumpDiam <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.ClumpDiam)

# Finding the maximum and minimum clump diameter value (117, 577)
ylim <- range(condensed.data.2013$ClumpDiameter)

# Plotting average of clump diameter value for each genotype ORDERED
p1 <-
  qplot(
    condensed.data.subset1.2013$genotypes.ordered.by.ClumpDiam,
    condensed.data.subset1.2013$ClumpDiameter,
    xlab = "Genotype",
    ylab = "",
    ylim = c(100, 600),
  ) + ggtitle("Average Clump Diameter for each Genotype (2013)") + geom_point(colour = "mediumseagreen") + theme_gray()+theme(plot.title =element_text(hjust = 0.5))
p2 <-
  qplot(
    condensed.data.subset2.2013$genotypes.ordered.by.ClumpDiam,
    condensed.data.subset2.2013$ClumpDiameter,
    xlab = "Genotype",
    ylab = "Clump Diameter (cm)",
    ylim = c(100, 600)
  ) + geom_point(colour = "mediumseagreen") + theme_gray()
p3 <-
  qplot(
    condensed.data.subset3.2013$genotypes.ordered.by.ClumpDiam,
    condensed.data.subset3.2013$ClumpDiameter,
    xlab = "Genotype",
    ylab = "",
    ylim = c(100, 600)
  ) + geom_point(colour = "mediumseagreen") + theme_gray() 
grid.arrange(p1, p2, p3, nrow = 3)


###################################################################################################

# TransectCount
temp<-sort(condensed.data.2013$TransectCount,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.TransectCount <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.TransectCount)
condensed.data.subset2.2013$genotypes.ordered.by.TransectCount <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.TransectCount)
condensed.data.subset3.2013$genotypes.ordered.by.TransectCount <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.TransectCount)

# Range of transect count values (7.5, 28.3)
ylim <- range(condensed.data.2013$TransectCount)

p1 <-
  qplot(
    condensed.data.subset1.2013$genotypes.ordered.by.TransectCount,
    condensed.data.subset1.2013$TransectCount,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 30),
  ) + ggtitle("Average Transect Count for each Genotype (2013)") + geom_point(colour = "darkorange3") + theme_gray() + theme(plot.title =element_text(hjust = 0.5))
p2 <-
  qplot(
    condensed.data.subset2.2013$genotypes.ordered.by.TransectCount,
    condensed.data.subset2.2013$TransectCount,
    xlab = "Genotype",
    ylab = "Transect Count",
    ylim = c(0, 30)
  ) + geom_point(colour = "darkorange3") + theme_gray()
p3 <-
  qplot(
    condensed.data.subset3.2013$genotypes.ordered.by.TransectCount,
    condensed.data.subset3.2013$TransectCount,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 30)
  ) + geom_point(colour = "darkorange3") + theme_gray() 
grid.arrange(p1, p2, p3, nrow = 3)

###################################################################################################

# TallestStemLigule ORDERED
temp<-sort(condensed.data.2013$TallestStemLigule,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.TallestStemLigule <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.TallestStemLigule)
condensed.data.subset2.2013$genotypes.ordered.by.TallestStemLigule <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.TallestStemLigule)
condensed.data.subset3.2013$genotypes.ordered.by.TallestStemLigule <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.TallestStemLigule)

# Range of tallest stem ligule values (28.7, 237.0)
ylim <- range(condensed.data.2013$TallestStemLigule)

p1 <- qplot(condensed.data.subset1.2013$genotypes.ordered.by.TallestStemLigule, condensed.data.subset1.2013$TallestStemLigule, xlab="Genotype", 
            ylab="",
            ylim=c(20, 250))+ggtitle("Average Tallest Stem Ligule for each Genotype (2013)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="darkolivegreen")
p2 <- qplot(condensed.data.subset2.2013$genotypes.ordered.by.TallestStemLigule, condensed.data.subset2.2013$TallestStemLigule, xlab="Genotype", 
            ylab="Tallest Stem Ligule (cm)", 
            ylim=c(20, 250))+geom_point(colour="darkolivegreen")
p3 <- qplot(condensed.data.subset3.2013$genotypes.ordered.by.TallestStemLigule, condensed.data.subset3.2013$TallestStemLigule, xlab="Genotype", 
            ylab="",
            ylim=c(20, 250))+geom_point(colour="darkolivegreen")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# TallestStemFlowerBase ORDERED

# Removing NA values
condensed.flowerbase.df <- data.frame(condensed.data.2013$character.genotypes)
condensed.flowerbase.df$TallestStemFlowerBase <- condensed.data.2013$TallestStemFlowerBase
condensed.flowerbase.df$genotypes.ordered.by.tallestStemFlowerBase <- condensed.data.2013$genotypes.ordered.by.TallestStemFlowerBase
condensed.flowerbase.df <- na.omit(condensed.flowerbase.df)

# 31 rows
dim(condensed.flowerbase.df)

temp <- sort(condensed.flowerbase.df$TallestStemFlowerBase, index.return=TRUE)$ix
condensed.subset.index1<-temp[1:31]

condensed.data.subset1.2013 <- condensed.flowerbase.df[condensed.subset.index1,]

#condensed.data.subset1.2013$genotypes.ordered.by.tallestStemFlowerBase <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.tallestStemFlowerBase)

# Maximum and minimum value for tallest stem flowerbase (113, 216)
ylim <- range(condensed.flowerbase.df$TallestStemFlowerBase)

p1 <- qplot(condensed.data.subset1.2013$genotypes.ordered.by.tallestStemFlowerBase, condensed.data.subset1.2013$TallestStemFlowerBase, xlab="Genotype", 
            ylab="Tallest Stem Flowerbase (cm)",
            ylim=c(100, 250))+ggtitle(" Average Tallest Stem Flowerbase for each Genotype (2013)")+geom_point(colour="blue3")+theme(plot.title=element_text(hjust=0.5))
grid.arrange(p1, nrow=1)

###################################################################################################

# TallestStemTrueLeaf ORDERED

temp<-sort(condensed.data.2013$TallestStemTrueLeaf,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.TallestStemTrueLeaf <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.TallestStemTrueLeaf)
condensed.data.subset2.2013$genotypes.ordered.by.TallestStemTrueLeaf <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.TallestStemTrueLeaf)
condensed.data.subset3.2013$genotypes.ordered.by.TallestStemTrueLeaf <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.TallestStemTrueLeaf)

# Range of true leaf values (26.0, 200.0)
ylim <- range(condensed.data.2013$TallestStemTrueLeaf)


p1 <- qplot(condensed.data.subset1.2013$genotypes.ordered.by.TallestStemTrueLeaf, condensed.data.subset1.2013$TallestStemTrueLeaf, xlab="Genotype", 
            ylab="",
            ylim=c(10, 210))+ggtitle("Average Tallest Stem True Leaf for each Genotype (2013)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="aquamarine4")
p2 <- qplot(condensed.data.subset2.2013$genotypes.ordered.by.TallestStemTrueLeaf, condensed.data.subset2.2013$TallestStemTrueLeaf, xlab="Genotype", 
            ylab="Tallest Stem True Leaf (cm)", 
            ylim=c(10,210))+geom_point(colour="aquamarine4")
p3 <- qplot(condensed.data.subset3.2013$genotypes.ordered.by.TallestStemTrueLeaf, condensed.data.subset3.2013$TallestStemTrueLeaf, xlab="Genotype", 
            ylab="",
            ylim=c(10,210))+geom_point(colour="aquamarine4")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################
###################################################################################################


# FLOWERINGINTENSITYLESSTHAN50, FLOWERINGINTENSITYGREATERTHAN50
# FLOWERINGINTENSITYGREATERTHAN80, SENESCEDGREATERTHAN80

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

colnames(Single_2013)

# FloweringIntensityLessThan50:     column 17
# FloweringIntensityGreaterThan50:  column 18
# FloweingIntensityGreaterThan80:   column 19
# SenescedGreaterThan80:            column 20

# Changing blank entries to an NA so the is.a function works
Single_2013$FloweringIntensityGreaterThan50[Single_2013$FloweringIntensityGreaterThan50==""] <- NA
Single_2013$FloweringIntensityGreaterThan50[Single_2013$FloweringIntensityGreaterThan50==" "] <- NA
Single_2013$FloweringIntensityLessThan50[Single_2013$FloweringIntensityLessThan50==""] <- NA
Single_2013$FloweringIntensityLessThan50[Single_2013$FloweringIntensityLessThan50==" "] <- NA
Single_2013$FloweringIntensityGreaterThan80[Single_2013$FloweringIntensityGreaterThan80==""] <- NA
Single_2013$FloweringIntensityGreaterThan80[Single_2013$FloweringIntensityGreaterThan80==" "] <- NA
Single_2013$SenescedGreaterThan80[Single_2013$SenescedGreaterThan80==""] <- NA
Single_2013$SenescedGreaterThan80[Single_2013$SenescedGreaterThan80==" "] <- NA


group0 <- is.na(Single_2013[17]) & is.na(Single_2013[18]) & is.na(Single_2013[19]) & is.na(Single_2013[20])
group1 <- !is.na(Single_2013[17]) & is.na(Single_2013[18]) & is.na(Single_2013[19]) & is.na(Single_2013[20])
group2 <- is.na(Single_2013[17]) & !is.na(Single_2013[18]) & is.na(Single_2013[19]) & is.na(Single_2013[20])
group3 <- is.na(Single_2013[17]) & is.na(Single_2013[18]) & !is.na(Single_2013[19]) & is.na(Single_2013[20])
group4 <- is.na(Single_2013[17]) & is.na(Single_2013[18]) & is.na(Single_2013[19]) & !is.na(Single_2013[20])
group5 <- !is.na(Single_2013[17]) & is.na(Single_2013[18]) & is.na(Single_2013[19]) & !is.na(Single_2013[20])
group6 <- is.na(Single_2013[17]) & !is.na(Single_2013[18]) & is.na(Single_2013[19]) & !is.na(Single_2013[20])
group7 <- is.na(Single_2013[17]) & is.na(Single_2013[18]) & !is.na(Single_2013[19]) & !is.na(Single_2013[20])

group.factor<-numeric(dim(Single_2013)[1])
group.factor[group0]<-0
group.factor[group1]<-1
group.factor[group2]<-2
group.factor[group3]<-3
group.factor[group4]<-4
group.factor[group5]<-5
group.factor[group6]<-6
group.factor[group7]<-7
group.factor<-factor(group.factor,levels=0:7,ordered=TRUE)

#Adding the group number into a new column called Group in the Single_2011 DataFrame
Single_2013$Group <- group.factor


length(which(group0)) #0
length(which(group1)) #0
length(which(group2)) #0
length(which(group3)) #0
length(which(group4)) #0
length(which(group5)) #69
length(which(group6)) #23
length(which(group7)) #210

library(waffle)
vals <- c(69, 23, 210)
val_names <- sprintf("%s (%s)", c("Group 5", "Group 6", "Group 7"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names

waffle::waffle(vals,colors=c("#B1AF70", "#FF6961", "#D291BC"))

+ ggthemes::scale_fill_tableau(name=NULL)

###################################################################################################

# Pairs plot for single 2013 data - adding harvest data
Single_2013 <- merge(Single_2013, Harvest_2013[,c("UID", "Whole.DW")], by="UID")

# Changing Whole.DW to Yield
names(Single_2013)[names(Single_2013) == "Whole.DW"] <- "Yield"

pairs(Single_2013[,c("meanStemDiam", "ClumpDiam", "TransectCount", "TallestStemLigule", "TallestStemFlower", "TallestStemTrueLeaf", "Yield")], col="darkgreen", cex.labels=1, lower.panel=NULL)

# Corrplot for single 2013 data
res <- cor(Single_2013[,c("meanStemDiam", "ClumpDiam", "TransectCount", "TallestStemLigule", "TallestStemFlower", "TallestStemTrueLeaf")], use="pairwise.complete.obs")
round(res,2)
corrplot(res, type="upper", order="hclust", tl.col="black", tl.srt=45)

Continuous_2013 <- merge(Continuous_2013, Harvest_2013[,c("UID", "Whole.DW")], by="UID")

# Changing Whole.DW to Yield
names(Continuous_2013)[names(Continuous_2013) == "Whole.DW"] <- "Yield"

pairs(Continuous_2013[,c("maxCanopyHeight", "FloweringStageDoYFirstA", "FloweringStageDoYFirstF", "DoYFirst3Emergence", "Yield")], col="darkgreen", cex.labels = 1.25, lower.panel=NULL)


###################################################################################################

# Building a linear model

# Dataframe with one column of genotypes (every plant)
Single.2013.lm.df <- data.frame(Single_2013$Genotype)

# Number of plants (302)
N.genotypes <- length(Single_2013$Genotype)

Single.2013.lm.df$StemDiameter <- Single_2013$StemDiam
Single.2013.lm.df$ClumpDiameter <- Single_2013$ClumpDiam
Single.2013.lm.df$TransectCount <- Single_2013$TransectCount
Single.2013.lm.df$TallestStemLigule <- Single_2013$TallestStemLigule
Single.2013.lm.df$TallestStemFlowerBase <- Single_2013$TallestStemFlower
Single.2013.lm.df$TallestStemTrueLeaf <- Single_2013$TallestStemTrueLeaf

# Renaming column Single_2012.Genotype to Genotype
names(Single.2013.lm.df)[names(Single.2013.lm.df) == "Single_2013.Genotype"] <- "Genotype"

# Removing any rows that have an NA so that linear models can be compared
dim(Single.2013.lm.df) #302 rows
Single.2013.lm.df <- na.omit(Single.2013.lm.df)
dim(Single.2013.lm.df) #152 rows (150 rows deleted)

""" Too many rows deleted? :( """

###################################################################################################

# Linear models for single data


###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

# Continuous Data

character.genotypes <- unique(as.character(Continuous_2013$Genotype))

# Condensed dataframe for the continuous data
# Working out averages per genotype for each variable for ease of plotting
condensed.data.2013.c <- data.frame(character.genotypes)

# Number of plants (302)
N.genotypes <- length(character.genotypes)

colnames(Continuous_2013)

# Empty columns of variables 
condensed.data.2013.c$DoYFirst3Emergence <- numeric(N.genotypes)
condensed.data.2013.c$maxCanopyHeight <- numeric(N.genotypes)
condensed.data.2013.c$DoYMaxCanopyHeight <- numeric(N.genotypes)
condensed.data.2013.c$FloweringStageDoYFirstA <- numeric(N.genotypes)
condensed.data.2013.c$FloweringStageDoYFirstF <- numeric(N.genotypes)

# Populating the dataframe with values
for (i in unique(Continuous_2013$Genotype)) {
  print(i)
  index <- which(Continuous_2013$Genotype==i)
  maxCanopyHeights <- as.matrix(Continuous_2013[index,c("maxCanopyHeight")])
  DoYMaxCanopyHeights <- as.matrix(Continuous_2013[index,c("DoYmaxCanopyHeight")])
  DoYFirst3Emergences <- as.matrix(Continuous_2013[index,c("DoYFirst3Emergence")])
  FloweringStageDoYFirstFs <- as.matrix(Continuous_2013[index,c("FloweringStageDoYFirstF")])
  FloweringStageDoYFirstAs <- as.matrix(Continuous_2013[index,c("FloweringStageDoYFirstA")])
  condensed.data.2013.c$maxCanopyHeight[which(condensed.data.2013.c$character.genotypes==as.character(i))] <- mean(maxCanopyHeights)
  condensed.data.2013.c$DoYMaxCanopyHeight[which(condensed.data.2013.c$character.genotypes==as.character(i))] <- mean(DoYMaxCanopyHeights)
  condensed.data.2013.c$DoYFirst3Emergence[which(condensed.data.2013.c$character.genotypes==as.character(i))] <- mean(DoYFirst3Emergences)
  condensed.data.2013.c$FloweringStageDoYFirstF[which(condensed.data.2013.c$character.genotypes==as.character(i))] <- mean(FloweringStageDoYFirstFs)
  condensed.data.2013.c$FloweringStageDoYFirstA[which(condensed.data.2013.c$character.genotypes==as.character(i))] <- mean(FloweringStageDoYFirstAs)
}


###################################################################################################

# Genotype orders for continuous variables

# Genotypes ordered by maxCanopyHeight
genotypes.ordered.by.maxCanopyHeight<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2013.c$maxCanopyHeight)])
condensed.data.2013.c$genotypes.ordered.by.maxCanopyHeight<-genotypes.ordered.by.maxCanopyHeight

# Genotpes ordered by DoymaxCanopyHeight
genotypes.ordered.by.DoYmaxCanopyHeight <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013.c$DoYMaxCanopyHeight)])
condensed.data.2013.c$genotypes.ordered.by.DoYmaxCanopyHeight <- genotypes.ordered.by.DoYmaxCanopyHeight

# Genotypes ordered by DoYFirst3Emergence
genotypes.ordered.by.DoYFirst3Emergence <- factor(character.genotypes, levels=character.genotypes[order(condensed.data.2013.c$DoYFirst3Emergence)])
condensed.data.2013.c$genotypes.ordered.by.DoYFirst3Emergence <- genotypes.ordered.by.DoYFirst3Emergence

# Genotypes ordered by DoYFirstFloweringStageA
genotypes.ordered.by.FloweringStageDoYFirstA<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2013.c$FloweringStageDoYFirstA)])
condensed.data.2013.c$genotypes.ordered.by.FloweringStageDoYFirstA<-genotypes.ordered.by.FloweringStageDoYFirstA

# Genotypes ordered by DoyFirstFloweringStageF
genotypes.ordered.by.FloweringStageDoYFirstF<-factor(character.genotypes,levels=character.genotypes[order(condensed.data.2013.c$FloweringStageDoYFirstF)])
condensed.data.2013.c$genotypes.ordered.by.FloweringStageDoYFirstF<-genotypes.ordered.by.FloweringStageDoYFirstF

###################################################################################################
###################################################################################################

# maxCanopyHeight ORDERED

temp<-sort(condensed.data.2013.c$maxCanopyHeight,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013.c[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013.c[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013.c[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.maxCanopyHeight)
condensed.data.subset2.2013$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.maxCanopyHeight)
condensed.data.subset3.2013$genotypes.ordered.by.maxCanopyHeight <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.maxCanopyHeight)

# Maximum and minimum average canopy height (45.0, 467.0)
ylim <- range(condensed.data.2013.c$maxCanopyHeight)
"""
# ONE AVERAGE IS AT 466 DUE TO AN ENTRY IN CANOPYHEIGHT BEING 1155 INSTEAD OF 115, CHANGE IT IN ORIGINAL DATA
# AND THEN RUN THE ENTIRE CODE AGAIN!!!
"""
# Plotting mean max shoot count values for all genotypes - ORDERED - NOT FINISHED
p1 <- qplot(condensed.data.subset1.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset1.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="",
            ylim=c(0, 100))+ggtitle("Average Maximum Canopy Height for each Genotype (2011)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="chocolate4")
p2 <- qplot(condensed.data.subset2.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset2.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="Maximum Canopy Height (cm)", 
            ylim=c(0,100))+geom_point(colour="chocolate4")
p3 <- qplot(condensed.data.subset3.2011$genotypes.ordered.by.maxCanopyHeight, condensed.data.subset3.2011$maxCanopyHeight, xlab="Genotype", 
            ylab="",
            ylim=c(0,100))+geom_point(colour="chocolate4")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# DoYmaxCanopyHeight

###################################################################################################

# DoYFirst3Emergence
temp<-sort(condensed.data.2013.c$DoYFirst3Emergence,index.return=TRUE)$ix
condensed.subset.index1<-temp[1:35]
condensed.subset.index2<-temp[36:70]
condensed.subset.index3<-temp[71:105]

condensed.data.subset1.2013 <- condensed.data.2013.c[condensed.subset.index1,]
condensed.data.subset2.2013 <- condensed.data.2013.c[condensed.subset.index2,]
condensed.data.subset3.2013 <- condensed.data.2013.c[condensed.subset.index3,]

condensed.data.subset1.2013$genotypes.ordered.by.DoYFirst3Emergence <- droplevels(condensed.data.subset1.2013$genotypes.ordered.by.DoYFirst3Emergence)
condensed.data.subset2.2013$genotypes.ordered.by.DoYFirst3Emergence <- droplevels(condensed.data.subset2.2013$genotypes.ordered.by.DoYFirst3Emergence)
condensed.data.subset3.2013$genotypes.ordered.by.DoYFirst3Emergence <- droplevels(condensed.data.subset3.2013$genotypes.ordered.by.DoYFirst3Emergence)

# Maximum and minimum day of 3 emergence (113.0, 148.0)
ylim <- range(condensed.data.2013.c$DoYFirst3Emergence, na.rm=TRUE)

# Plotting DoYFirst3Emergence average for all genotypes - ORDERED
p1 <- qplot(condensed.data.subset1.2013$genotypes.ordered.by.DoYFirst3Emergence, condensed.data.subset1.2013$DoYFirst3Emergence, xlab="Genotype", 
            ylab="",
            ylim=c(100, 160))+ggtitle("Average Day of Year to Reach 3 Emergence for Every Genotype (2013)")+theme(plot.title=element_text(hjust=0.5))+geom_point(colour="darkorchid3")
p2 <- qplot(condensed.data.subset2.2013$genotypes.ordered.by.DoYFirst3Emergence, condensed.data.subset2.2013$DoYFirst3Emergence, xlab="Genotype", 
            ylab="Day of Year", 
            ylim=c(100,160))+geom_point(colour="darkorchid3")
p3 <- qplot(condensed.data.subset3.2013$genotypes.ordered.by.DoYFirst3Emergence, condensed.data.subset3.2013$DoYFirst3Emergence, xlab="Genotype", 
            ylab="",
            ylim=c(100,160))+geom_point(colour="darkorchid3")
grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################

# FloweringStageDoYFirstF

""" LOADS OF NAs IN THIS DATA! """

###################################################################################################
###################################################################################################
###################################################################################################

#Combining single and continuous data to use for a linear model

# SingleContinuous dataframe with a column for genotypes
SC.2013.lm.df <- data.frame(Single_2013$Genotype)

# Number of plants (302)
N.genotypes <- length(Single_2013$Genotype)
N.genotypes2 <- length(Continuous_2013$Genotype)

# Genotypes are identical across single and continuous data
identical(N.genotypes, N.genotypes2)

# Populating the dataframe with values
SC.2013.lm.df$UID <- Single_2013$UID
SC.2013.lm.df$StemDiameter <- Single_2013$meanStemDiam
SC.2013.lm.df$ClumpDiameter <- Single_2013$ClumpDiam
SC.2013.lm.df$TransectCount <- Single_2013$TransectCount
SC.2013.lm.df$TallestStemLigule <- Single_2013$TallestStemLigule
SC.2013.lm.df$TallestStemFlowerBase <- Single_2013$TallestStemFlower
SC.2013.lm.df$TallestStemTrueLeaf <- Single_2013$TallestStemTrueLeaf
SC.2013.lm.df$MaxCanopyHeight <- Continuous_2013$maxCanopyHeight
SC.2013.lm.df$DoYmaxCanopyHeight <- Continuous_2013$DoYmaxCanopyHeight
SC.2013.lm.df$DoYFirst3Emergence <- Continuous_2013$DoYFirst3Emergence
SC.2013.lm.df$FloweringStageDoYFirstA <- Continuous_2013$FloweringStageDoYFirstA
SC.2013.lm.df$FloweringStageDoYFirstF <- Continuous_2013$FloweringStageDoYFirstF

SC.2013.lm.df <- merge(SC.2013.lm.df, Harvest_2013[,c("UID", "Whole.DW")], by="UID")

# Renaming column Single_2013.Genotype to Genotype
names(SC.2013.lm.df)[names(SC.2013.lm.df) == "Single_2013.Genotype"] <- "Genotype"
names(SC.2013.lm.df)[names(SC.2013.lm.df) == "Whole.DW"] <- "Yield"

# Removing rows with NAs in it
# If we are to use a variable in a linear model, all datasets need to be the same size
# in order to compare

# 288 rows
dim(SC.2013.lm.df)

SC.2013.lm.df$Yield[which(SC.2013.lm.df$Yield==4.0)] <- NA
SC.2013.lm.df$Yield[which(SC.2013.lm.df$Yield==9.0)] <- NA
SC.2013.lm.df$Yield[which(SC.2013.lm.df$Yield==13.0)] <- NA
SC.2013.lm.df$Yield[which(SC.2013.lm.df$Yield==36.4)] <- NA

# Removing rows that contain NA values
SC.2013.lm.df <- na.omit(SC.2013.lm.df)

# 118 rows (184 rows deleted)
dim(SC.2013.lm.df)

# Correlation plot for linear model dataframe (continuous and single)
res2 <- cor(SC.2013.lm.df[,c("StemDiameter", "ClumpDiameter", "TransectCount", "TallestStemLigule", "TallestStemTrueLeaf", "TallestStemFlowerBase", "MaxCanopyHeight", "DoYmaxCanopyHeight", "DoYFirst3Emergence", "FloweringStageDoYFirstA", "FloweringStageDoYFirstF", "Yield")])
round(res2, 2)

corrplot(res2, method="square", type="upper", order="original", tl.col="black", tl.srt=45)


# Pairs plot just for continuous variables???

###################################################################################################

# Linear Model


SC.2013.lm.df<-merge(SC.2013.lm.df,Harvest_2013[,c("UID","Whole.DW")],by="UID")

# How well does the lm.aic2 model predict the yield values for 2013?
dw.predict.2013 <-predict(lm.aic, SC.2013.lm.df)

plot(dw.predict.2013, SC.2013.lm.df$Whole.DW)

