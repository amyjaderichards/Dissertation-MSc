# Plotting all 3 years of data on a single graph

character.genotypes <- factor(character.genotypes,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
condensed.data$character.genotypes <- character.genotypes

# Stem Diameter 
StemDiam.3years <- data.frame(condensed.data$character.genotypes)
StemDiam.3years$StemDiam.2011 <- condensed.data$StemDiameterMean
StemDiam.3years$StemDiam.2012 <- condensed.data.2012$StemDiameterMean
StemDiam.3years$StemDiam.2013 <- condensed.data.2013$StemDiameter
StemDiam.3years$genotype.order.2013 <- condensed.data.2013$genotypes.ordered.by.StemDiam

# Splitting the data into three subsets for easier plotting - ORDERED
temp <- sort(StemDiam.3years$StemDiam.2013,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1<- StemDiam.3years[condensed.subset.index1,]
condensed.data.subset2 <- StemDiam.3years[condensed.subset.index2,]
condensed.data.subset3 <- StemDiam.3years[condensed.subset.index3,]

condensed.data.subset1$genotype.order.2013 <- droplevels(condensed.data.subset1$genotype.order.2013)
condensed.data.subset2$genotype.order.2013 <- droplevels(condensed.data.subset2$genotype.order.2013)
condensed.data.subset3$genotype.order.2013 <- droplevels(condensed.data.subset3$genotype.order.2013)

head(condensed.data.subset1)

one <- data.frame(condensed.data.subset1)
two <- data.frame(condensed.data.subset2)
three <- data.frame(condensed.data.subset3)

ylim <- range(StemDiam.3years$StemDiam.2011) #(4.02, 7.85)
ylim <- range(StemDiam.3years$StemDiam.2012) #(2.66, 6.98)
ylim <- range(StemDiam.3years$StemDiam.2013) #(2.82, 7.75)

# Plotting mean StemDiameter ordered by 2013 genotypes
p1 <-
  qplot(
    condensed.data.subset1$genotype.order.2013,
    condensed.data.subset1$StemDiam.2011,
    xlab = "Genotype",
    ylab = "",
    ylim = c(3, 8)) + ggtitle("Average Stem Diameter for each Genotype") + geom_point(colour = "mediumvioletred") + theme_gray() + theme(plot.title = element_text(hjust=0.5)) +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$StemDiam.2012), colour="green") +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$StemDiam.2013), colour="orange")
p1 <- p1 + annotate("text", x=31, y=8, label="2011", colour="mediumvioletred", fontface=2)
p1 <- p1 + annotate("text", x=33, y=8, label="2012", colour="green", fontface=2)
p1 <- p1 + annotate("text", x=35, y=8, label="2013", colour="orange", fontface=2)

p2 <-
  qplot(
    two$genotype.order.2013, two$StemDiam.2011,
    xlab = "Genotype",  ylab = "Stem Diameter (cm)",
    ylim = c(3, 8)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$StemDiam.2012), colour="green") +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$StemDiam.2013), colour="orange")

p3 <-
  qplot(
    three$genotype.order.2013, three$StemDiam.2011,
    xlab = "Genotype",  ylab = "",
    ylim = c(3, 8)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$StemDiam.2012), colour="green") +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$StemDiam.2013), colour="orange")

grid.arrange(p1, p2, p3, nrow=3)

mean(StemDiam.3years[,"StemDiam.2011"])
mean(StemDiam.3years[,"StemDiam.2012"])
mean(StemDiam.3years[,"StemDiam.2013"])

###################################################################################################

# Clump Diameter

character.genotypes <- factor(character.genotypes,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
condensed.data.2013$character.genotypes <- character.genotypes

# Clump Diameter 
ClumpDiam.3years <- data.frame(condensed.data.2013$character.genotypes)
ClumpDiam.3years$ClumpDiam.2011 <- condensed.data$ClumpDiameterMean
ClumpDiam.3years$ClumpDiam.2012 <- condensed.data.2012$ClumpDiameter
ClumpDiam.3years$ClumpDiam.2013 <- condensed.data.2013$ClumpDiameter
ClumpDiam.3years$genotype.order.2013 <- condensed.data.2013$genotypes.ordered.by.ClumpDiam

# Splitting the data into three subsets for easier plotting - ORDERED
temp <- sort(ClumpDiam.3years$ClumpDiam.2013,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1<- ClumpDiam.3years[condensed.subset.index1,]
condensed.data.subset2 <- ClumpDiam.3years[condensed.subset.index2,]
condensed.data.subset3 <- ClumpDiam.3years[condensed.subset.index3,]

condensed.data.subset1$genotype.order.2013 <- droplevels(condensed.data.subset1$genotype.order.2013)
condensed.data.subset2$genotype.order.2013 <- droplevels(condensed.data.subset2$genotype.order.2013)
condensed.data.subset3$genotype.order.2013 <- droplevels(condensed.data.subset3$genotype.order.2013)

head(condensed.data.subset1)

one <- data.frame(condensed.data.subset1)
two <- data.frame(condensed.data.subset2)
three <- data.frame(condensed.data.subset3)

ylim <- range(ClumpDiam.3years$ClumpDiam.2011) #(2.0, 27.3)
ylim <- range(ClumpDiam.3years$ClumpDiam.2012) #(100, 420)
ylim <- range(ClumpDiam.3years$ClumpDiam.2013) #(117, 577)

# Plotting mean clump diameter per genotype sorted by 2013 genotype order
p1 <-
  qplot(
    condensed.data.subset1$genotype.order.2013,
    condensed.data.subset1$ClumpDiam.2011,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 580)) + ggtitle("Average Clump Diameter for each Genotype") + geom_point(colour = "mediumvioletred") + theme_gray() + theme(plot.title = element_text(hjust=0.5)) +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$ClumpDiam.2012), colour="green") +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$ClumpDiam.2013), colour="orange")
p1 <- p1 + annotate("text", x=30, y=580, label="2011", colour="mediumvioletred", fontface=2)
p1 <- p1 + annotate("text", x=32, y=580, label="2012", colour="green", fontface=2)
p1 <- p1 + annotate("text", x=34, y=580, label="2013", colour="orange", fontface=2)

p2 <-qplot(two$genotype.order.2013, two$ClumpDiam.2011,
    xlab = "Genotype",  ylab = "Clump Diameter (mm)",
    ylim = c(0, 580)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$ClumpDiam.2012), colour="green") +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$ClumpDiam.2013), colour="orange")

p3 <-
  qplot(
    three$genotype.order.2013, three$ClumpDiam.2011,
    xlab = "Genotype",  ylab = "",
    ylim = c(0, 580)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$ClumpDiam.2012), colour="green") +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$ClumpDiam.2013), colour="orange")

grid.arrange(p1, p2, p3, nrow=3)


###################################################################################################

# Transect Count
TransectCount.3years <- data.frame(condensed.data$character.genotypes)
TransectCount.3years$TransectCount.2011 <- condensed.data$TransectCount
TransectCount.3years$TransectCount.2012 <- condensed.data.2012$TransectCount
TransectCount.3years$TransectCount.2013 <- condensed.data.2013$TransectCoun
TransectCount.3years$genotype.order.2013 <- condensed.data.2013$genotypes.ordered.by.TransectCount

# Splitting the data into three subsets for easier plotting - ORDERED
temp <- sort(TransectCount.3years$TransectCount.2013,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1 <- TransectCount.3years[condensed.subset.index1,]
condensed.data.subset2 <- TransectCount.3years[condensed.subset.index2,]
condensed.data.subset3 <- TransectCount.3years[condensed.subset.index3,]

condensed.data.subset1$genotype.order.2013 <- droplevels(condensed.data.subset1$genotype.order.2013)
condensed.data.subset2$genotype.order.2013 <- droplevels(condensed.data.subset2$genotype.order.2013)
condensed.data.subset3$genotype.order.2013 <- droplevels(condensed.data.subset3$genotype.order.2013)

head(condensed.data.subset1)

one <- data.frame(condensed.data.subset1)
two <- data.frame(condensed.data.subset2)
three <- data.frame(condensed.data.subset3)

ylim <- range(TransectCount.3years$TransectCount.2011, na.rm=TRUE) #(1.33, 12.67)
ylim <- range(TransectCount.3years$TransectCount.2012) #(4. 28)
ylim <- range(TransectCount.3years$TransectCount.2013) #(7.5, 28.3)

# Plotting transect count ordered by 2013 genotype
p1 <-
  qplot(
    condensed.data.subset1$genotype.order.2013,
    condensed.data.subset1$TransectCount.2011,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 30)) + ggtitle("Average Transect Count for each Genotype") + geom_point(colour = "mediumvioletred") + theme_gray() + theme(plot.title = element_text(hjust=0.5)) +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$TransectCount.2012), colour="green") +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$TransectCount.2013), colour="orange")
p1 <- p1 + annotate("text", x=30, y=29, label="2011", colour="mediumvioletred", fontface=2)
p1 <- p1 + annotate("text", x=32, y=29, label="2012", colour="green", fontface=2)
p1 <- p1 + annotate("text", x=34, y=29, label="2013", colour="orange", fontface=2)

p2 <-qplot(two$genotype.order.2013, two$TransectCount.2011,
           xlab = "Genotype",  ylab = "Transect Count",
           ylim = c(0, 30)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$TransectCount.2012), colour="green") +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$TransectCount.2013), colour="orange")

p3 <-
  qplot(
    three$genotype.order.2013, three$TransectCount.2011,
    xlab = "Genotype",  ylab = "",
    ylim = c(0, 30)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$TransectCount.2012), colour="green") +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$TransectCount.2013), colour="orange")

grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################
# Tallest Stem Ligule
TallestStemLigule.3years <- data.frame(condensed.data$character.genotypes)
TallestStemLigule.3years$TallestStemLigule.2011 <- condensed.data$TallestStemLigule
TallestStemLigule.3years$TallestStemLigule.2012 <- condensed.data.2012$TallestStemLigule
TallestStemLigule.3years$TallestStemLigule.2013 <- condensed.data.2013$TallestStemLigule
TallestStemLigule.3years$genotype.order.2013 <- condensed.data.2013$genotypes.ordered.by.TallestStemLigule

# Splitting the data into three subsets for easier plotting - ORDERED
temp <- sort(TallestStemLigule.3years$TallestStemLigule.2013,index.return=TRUE)$ix
condensed.subset.index1 <- temp[1:35]
condensed.subset.index2 <- temp[36:70]
condensed.subset.index3 <- temp[71:105]

condensed.data.subset1 <- TallestStemLigule.3years[condensed.subset.index1,]
condensed.data.subset2 <- TallestStemLigule.3years[condensed.subset.index2,]
condensed.data.subset3 <- TallestStemLigule.3years[condensed.subset.index3,]

condensed.data.subset1$genotype.order.2013 <- droplevels(condensed.data.subset1$genotype.order.2013)
condensed.data.subset2$genotype.order.2013 <- droplevels(condensed.data.subset2$genotype.order.2013)
condensed.data.subset3$genotype.order.2013 <- droplevels(condensed.data.subset3$genotype.order.2013)

head(condensed.data.subset1)

one <- data.frame(condensed.data.subset1)
two <- data.frame(condensed.data.subset2)
three <- data.frame(condensed.data.subset3)

ylim <- range(TallestStemLigule.3years$TallestStemLigule.2011) #(7.75, 111)
ylim <- range(TallestStemLigule.3years$TallestStemLigule.2012) #(14.7, 172)
ylim <- range(TallestStemLigule.3years$TallestStemLigule.2013) #(28.7, 237)

# Plotting tallest stem ligule ordered by 2013 genotypes
p1 <-
  qplot(
    condensed.data.subset1$genotype.order.2013,
    condensed.data.subset1$TallestStemLigule.2011,
    xlab = "Genotype",
    ylab = "",
    ylim = c(0, 240)) + ggtitle("Average Tallest Stem Ligule for each Genotype") + geom_point(colour = "mediumvioletred") + theme_gray() + theme(plot.title = element_text(hjust=0.5)) +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$TallestStemLigule.2012), colour="green") +
  geom_point(data=condensed.data.subset1, aes(x=condensed.data.subset1$genotype.order.2013, y=condensed.data.subset1$TallestStemLigule.2013), colour="orange")
p1 <- p1 + annotate("text", x=30, y=239, label="2011", colour="mediumvioletred", fontface=2)
p1 <- p1 + annotate("text", x=32, y=239, label="2012", colour="green", fontface=2)
p1 <- p1 + annotate("text", x=34, y=239, label="2013", colour="orange", fontface=2)

p2 <-qplot(two$genotype.order.2013, two$TallestStemLigule.2011,
           xlab = "Genotype",  ylab = "Tallest Stem Ligule (cm)",
           ylim = c(0, 240)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$TallestStemLigule.2012), colour="green") +
  geom_point(data=two, aes(x=two$genotype.order.2013, y=two$TallestStemLigule.2013), colour="orange")

p3 <-
  qplot(
    three$genotype.order.2013, three$TallestStemLigule.2011,
    xlab = "Genotype",  ylab = "",
    ylim = c(0, 240)) + geom_point(colour = "mediumvioletred") + theme_gray() +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$TallestStemLigule.2012), colour="green") +
  geom_point(data=three, aes(x=three$genotype.order.2013, y=three$TallestStemLigule.2013), colour="orange")

grid.arrange(p1, p2, p3, nrow=3)

###################################################################################################
# Pair Plots for Individual Variables

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# Stem Diameter
StemDiam.3years <- data.frame(condensed.data$character.genotypes)
StemDiam.3years$StemDiam.2011 <- condensed.data$StemDiameterMean
StemDiam.3years$StemDiam.2012 <- condensed.data.2012$StemDiameterMean
StemDiam.3years$StemDiam.2013 <- condensed.data.2013$StemDiameter
pairs(StemDiam.3years[,c(2:4)], col="mediumvioletred", cex.labels = 2, lower.panel = panel.cor)

# Clump Diameter
ClumpDiam.3years <- data.frame(condensed.data$character.genotypes)
ClumpDiam.3years$ClumpDiam.2011 <- condensed.data$ClumpDiameterMean
ClumpDiam.3years$ClumpDiam.2012 <- condensed.data.2012$ClumpDiameter
ClumpDiam.3years$ClumpDiam.2013 <- condensed.data.2013$ClumpDiameter
pairs(ClumpDiam.3years[,c(2:4)], col="mediumseagreen", lower.panel = panel.cor, cex.labels=2)

# Transect Count
Transect.3years <- data.frame(Single_2011$Genotype)
Transect.3years$Transect.2011 <- Single_2011$TransectCount
Transect.3years$Transect.2012 <- Single_2012$TransectCount
Transect.3years$Transect.2013 <- Single_2013$TransectCount
Transect.3years <- na.omit(Transect.3years)
pairs(Transect.3years[,c(2:4)], col="darkorange3", lower.panel = panel.cor, cex.labels = 2)

# Ligule
Ligule.3years <- data.frame(Single_2011$Genotype)
Ligule.3years$TallestStemLigule.2011 <- Single_2011$TallestStemLigule
Ligule.3years$TallestStemLigule.2012 <- Single_2012$TallestStemLigule
Ligule.3years$TallestStemLigule.2013 <- Single_2013$TallestStemLigule
pairs(Ligule.3years[,c(2:4)], col="darkolivegreen", lower.panel=panel.cor, cex.labels=2)

# Flowerbase
Flower.3years <- data.frame(Single_2011$TallestStemFlowerbase)
Flower.3years$TallestStemFlower.2011 <- Single_2011$TallestStemFlowerbase
Flower.3years$TallestStemFlower.2012 <- Single_2012$TallestStemFlower
Flower.3years$TallestStemFlower.2013 <- Single_2013$TallestStemFlower
pairs(Flower.3years[,c(2:4)], col="blue3", cex.labels=2, lower.panel=NULL)

# Trueleaf
TrueLeaf.2years <- data.frame(Single_2012$Genotype)
TrueLeaf.2years$TallestStemTrueLeaf.2012 <- Single_2012$TallestStemTrueLeaf
TrueLeaf.2years$TallestStemTrueLeaf.2013 <- Single_2013$TallestStemTrueLeaf
pairs(TrueLeaf.2years[,c(2:3)], col="aquamarine4", cex.labels = 2, lower.panel=panel.cor)

# maxCanopyHeight
CanopyHeight.3years <- data.frame(Single_2011$Genotype)
CanopyHeight.3years$MaxCanopyHeight.2011 <- Continuous_2011$maxCanopyHeight
CanopyHeight.3years$MaxCanopyHeight.2012 <- Continuous_2012$maxCanopyHeight
CanopyHeight.3years$MaxCanopyHeight.2013 <- Continuous_2013$maxCanopyHeight
pairs(CanopyHeight.3years[,c(2:4)], col="chocolate4", cex.labels = 2, lower.panel=panel.cor)

# FloweringStageDoYFirstF
FirstF.3years <- data.frame(Single_2011$Genotype)
FirstF.3years$FloweringStageDoYFirstF.2011 <- Continuous_2011$FloweringStageDoYFirstF
FirstF.3years$FloweringStageDoYFirstF.2012 <- Continuous_2012$FloweringStageDoYFirstF
FirstF.3years$FloweringStageDoYFirstF.2013 <- Continuous_2013$FloweringStageDoYFirstF
pairs(FirstF.3years[,c(2:4)], col="goldenrod2", cex.labels=1.5, lower.panel=NULL)

# FloweringStageDoYFirstA
FirstA.3years <- data.frame(Single_2011$Genotype)
FirstA.3years$FloweringStageDoYFirstA.2011 <- Continuous_2011$FloweringStageDoYFirstA
FirstA.3years$FloweringStageDoYFirstA.2012 <- Continuous_2012$FloweringStageDoYFirstA
FirstA.3years$FloweringStageDoYFirstA.2013 <- Continuous_2013$FloweringStageDoYFirstA
pairs(FirstA.3years[,c(2:4)], col="firebrick2", cex.labels = 1.5, lower.panel = NULL)

# DoYmaxCanopyHeight
DoYCanopyHeight.3years <- data.frame(Single_2011$Genotype)
DoYCanopyHeight.3years$DoYmaxCanopyHeight.2011 <- Continuous_2011$DoYmaxCanopyHeight
DoYCanopyHeight.3years$DoYmaxCanopyHeight.2012 <- Continuous_2012$DoYmaxCanopyHeight
DoYCanopyHeight.3years$DoYmaxCanopyHeight.2013 <- Continuous_2013$DoYmaxCanopyHeight
pairs(DoYCanopyHeight.3years[,c(2:4)], col="steelblue", cex.labels=1.5, lower.panel=panel.cor)

#DoYFirst3Emergence
First3Emergence.2years <- data.frame(Single_2011$Genotype)
First3Emergence.2years$DoYFirst3Emergence.2012 <- Continuous_2012$DoYFirst3Emergence
First3Emergence.2years$DoYFirst3Emergence.2013 <- Continuous_2013$DoYFirst3Emergence
pairs(First3Emergence.2years[,c(2:3)], col="darkorchid3", cex.labels=1.5, lower.panel=NULL)
