# 2011 data

# Reading in the csv files - WINDOWS
Single_2011 <- read.csv("./Data/Single_Miscanthus_2011.csv", skip=5)
Continuous_2011 <- read.csv("./Data/Continuous_Miscanthus_2011.csv", skip=5)

# Reading in the csv files - MAC
Single_2011 <- read.csv("../Data/Single_Miscanthus_2011.csv", skip=5)
Continuous_2011 <- read.csv("../Data/Single/Miscanthus_2011.csv", skip=5)

# A list of rows that are dead plants i.e. 0 and NA values only
dead.plants <- c(6,14,38,39,68,69,83,110,116,119,120,126,144,166,172,185,191,193,201,204,238,310)

# Removing the dead plants from the dataframes
Single_2011 <- Single_2011[-c(dead.plants),]
Continuous_2011 <- Continuous_2011[-c(dead.plants),]

# A list of characters of the Mx-Numbers (Plant IDs) in the Single_2011 data i.e. " - Mx 1553#114"
Genotype <- as.character(Single_2011$Mx.Number)
Genotype2 <- as.character(Continuous_2011$Mx.Number)

# Checking whether the Mx Numbers are identical between single and continuous data
identical(Genotype, Genotype2)

# Using grep to find all indexes in the Genotype list that are the Goliath genotype
index.goliath <- grep("oliath",Genotype)

# Using grep to find all indexes in the Genotype list that are the Giganteus genotype
index.giganteus <- grep("iganteus",Genotype)

# Indexes of both Goliath and Giganteus genotypes
combined.index <- c(index.goliath,index.giganteus)

# If the Genotype is neither Giganteus or Goliath, strip the string to leave just the 3 digit ID number
# If the Genotype is Goliath change it to "Gol"
# If the Genotype is Giganteus change it go "Gig"
N<-length(Genotype)
for(i in 1:N){
    if(! i %in% combined.index) Genotype[i] <- as.numeric(strsplit(Genotype[i], "#")[[1]][2])
    if (i %in% index.goliath) Genotype[i]<-"Gol"
    if (i %in% index.giganteus) Genotype[i]<-"Gig"
}

# Order the Genotypes and assign them to the Genotype column in Single_2011
Genotype.copy <- factor(Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
Single_2011$Genotype <- Genotype.copy

# Checking if the Mx.number is identical between both dataframes
identical(Single_2011$Mx.Number,Continuous_2011$Mx.Number)
# Since the two vectors are identical, we proceed to copy Genotype.copy
# across to the Continuous_2011 data frame

Continuous_2011$Genotype <- Genotype.copy

# Order Genotype.copy 
Genotype.order <- order(Genotype.copy)

# Sort Single_2011 using Genotype.order
Single_2011_reordered <- Single_2011[Genotype.order,]
# Sort Continuous_2011 using Genotype.order
Continuous_2011_reordered <- Continuous_2011[Genotype.order,]

# Creating a new column meanStemDiam which is the average of StemDiam1, StemDiam2 and StemDiam3
Single_2011_reordered$meanStemDiam <- round(apply(Single_2011_reordered[,c("StemDiam1","StemDiam2","StemDiam3")],1,mean),1)
# Creating a new column sdStemDiam which is the standard deviation of StemDiam1, StemDiam2 and StemDiam3
Single_2011_reordered$sdStemDiam <- round(apply(Single_2011_reordered[,c("StemDiam1","StemDiam2","StemDiam3")],1,sd),1)

# Changing entry of "1 stem" in the AngleOutside column to NA
# 5 blank entries?
Single_2011_reordered$AngleOutside[which(Single_2011_reordered$AngleOutside=="1 stem")] <- NA
# Changing angle outside to numeric values
Single_2011_reordered$AngleOutside <- as.numeric(Single_2011_reordered$AngleOutside)

# Changing entries of '-9999' in transect count to NA
Single_2011_reordered$TransectCount[which(Single_2011_reordered$TransectCount=="-9999")] <- NA

# In the following, DoY means "Day of Year"

# Changing entries of "-9999" in FloweringIntensityLessThan50 column to NA
Single_2011_reordered$FloweringIntensityLessThan50[which(Single_2011_reordered$FloweringIntensityLessThan50==-9999)]<-NA
# Changing entries of "-9999" in FloweringIntensityGreaterThan50 column to NA
Single_2011_reordered$FloweringIntensityGreaterThan50[which(Single_2011_reordered$FloweringIntensityGreaterThan50==-9999)]<-NA
# Changing entries of "-9999" in FloweringIntensityGreaterThan80 column to NA
Single_2011_reordered$FloweringIntensityGreaterThan80[which(Single_2011_reordered$FloweringIntensityGreaterThan80==-9999)]<-NA
# Changing entries of "-9999" in SenescedGreaterThan80 column to NA
Single_2011_reordered$SenescedGreaterThan80[which(Single_2011_reordered$SenescedGreaterThan80==-9999)]<-NA

# Changing any entries which aren't NA in the FloweringIntensityLessThan50 column to "y"
Single_2011_reordered$FloweringIntensityLessThan50[which(!is.na(Single_2011_reordered$FloweringIntensityLessThan50))]<-"y"
# Changing any entries which aren't NA in the FloweringIntensityGreaterThan50 column to "y"
Single_2011_reordered$FloweringIntensityGreaterThan50[which(!is.na(Single_2011_reordered$FloweringIntensityGreaterThan50))]<-"y"
# Changing any entries which aren't NA in the FloweringIntensityGreaterThan80 column to "y"
Single_2011_reordered$FloweringIntensityGreaterThan80[which(!is.na(Single_2011_reordered$FloweringIntensityGreaterThan80))]<-"y"
# Changing any entries which aren't NA in the SenescedGreaterThan80 column to "y"
Single_2011_reordered$SenescedGreaterThan80[which(!is.na(Single_2011_reordered$SenescedGreaterThan80))]<-"y"

# Creating a new column meanClumpDiam which is the average of clumpDiam1 and ClumpDiam2
Single_2011_reordered$meanClumpDiam<-round(apply(Single_2011_reordered[,c("ClumpDiam1","ClumpDiam2")],1,mean),1)
# Creating a new column sdClumpDiam which is the standard deviation of ClumpDiam1 and ClumpDiam2
Single_2011_reordered$sdClumpDiam<-round(apply(Single_2011_reordered[,c("ClumpDiam1","ClumpDiam2")],1,sd),1)

Single_2011_reordered<-Single_2011_reordered[,c(1:5,21,7:9,22:23,10:13,24:25,14,15:20)]

# Changing column name "DoY 73" to "X73"
colnames(Continuous_2011_reordered)[8] <- "X73"

# Changing column names from "X87" to "ShootCountsDoY87" etc.
temp <- gsub("X","ShootCountsDoY",colnames(Continuous_2011_reordered)[8:40])
colnames(Continuous_2011_reordered)[8:40] <- temp

# Splitting columns 42-68 into two parts on the . i.e. X305.2 to "X305" "2" 
strsplit(colnames(Continuous_2011_reordered)[42:68],"[.]")

# A function that extracts the first element in a vector
extract.first.element <- function(data.vector){
  out <- data.vector[1]
}

# Changing column names from "X178.1" to "CanopyHeighDoY178" etc.
temp2 <- gsub("X","CanopyHeightDoY",unlist(lapply(strsplit(colnames(Continuous_2011_reordered)[42:68],"[.]"),extract.first.element)))
colnames(Continuous_2011_reordered)[42:68] <- temp2

# Changing column names from "X213.2" to "FloweringStageDoY213" etc.
temp3 <- gsub("X","FloweringStageDoY",unlist(lapply(strsplit(colnames(Continuous_2011_reordered)[70:85],"[.]"),extract.first.element)))
colnames(Continuous_2011_reordered)[70:85] <- temp3

# FloweringStageDoY305 contains no info and is dropped.
Continuous_2011_reordered <- Continuous_2011_reordered[,c(1:5,86,8:40,42:67,70:84)]

# Changing "dead" entries in CanopyHeightDoY136 to NA
Continuous_2011_reordered$CanopyHeightDoY136[which(Continuous_2011_reordered$CanopyHeightDoY136=="dead")]<-NA
# 
Continuous_2011_reordered$CanopyHeightDoY136<-as.numeric(levels(Continuous_2011_reordered$CanopyHeightDoY136)[Continuous_2011_reordered$CanopyHeightDoY136])
# Changing "h" entries in CanopyHeightDoY264 to NA
Continuous_2011_reordered$CanopyHeightDoY264[which(Continuous_2011_reordered$CanopyHeightDoY264=="h")]<-NA
# 
Continuous_2011_reordered$CanopyHeightDoY264<-as.numeric(levels(Continuous_2011_reordered$CanopyHeightDoY264)[Continuous_2011_reordered$CanopyHeightDoY264])

# Changing FALSE entries in FloweringStageDoY200 column to "F"
Continuous_2011_reordered$FloweringStageDoY200[which(Continuous_2011_reordered$FloweringStageDoY200==FALSE)]<-"F"
# Changing blank entries in FloweringStageDoY200 column to an empty string ""
Continuous_2011_reordered$FloweringStageDoY200[is.na(Continuous_2011_reordered$FloweringStageDoY200)]<-""
# 
Continuous_2011_reordered$FloweringStageDoY200 <- as.character(Continuous_2011_reordered$FloweringStageDoY200)

# 
for (i in 66:80){
  Continuous_2011_reordered[,i] <- factor(Continuous_2011_reordered[,i],levels=levels(Continuous_2011_reordered$FloweringStageDoY227))
}

# A list of the ShootCountsDaysofYear columns
ShootCountsDaysofYear <- as.numeric(gsub("ShootCountsDoY","",colnames(Continuous_2011_reordered)[7:39]))
# A list of CanopyHeightsDaysOfYear columns
CanopyHeightsDaysofYear<-as.numeric(gsub("CanopyHeightDoY","",colnames(Continuous_2011_reordered)[40:65]))
# A list of FloweringStageDaysofYear columns
FloweringStageDaysofYear<-as.numeric(gsub("FloweringStageDoY","",colnames(Continuous_2011_reordered)[66:80]))

# The number of plants (302)
N.plants <- dim(Continuous_2011_reordered)[1]

# Creating empty vectors of length N.plants for each variable (302)
maxShootCounts <- numeric(N.plants)
DoYmaxShootCount <- numeric(N.plants)
maxCanopyHeight <- numeric(N.plants)
DoYmaxCanopyHeight <- numeric(N.plants)
FloweringStageDoYFirstA <- numeric(N.plants)
FloweringStageDoYFirstF <- numeric(N.plants)

# Populate the maxShootCounts vector with the ordered Continuous_2011 data
maxShootCounts <- apply(Continuous_2011_reordered[,7:39],1,max)

# Populate the maxCanopyHeight vector with the ordered Continuous_2011 data
maxCanopyHeight <- apply(Continuous_2011_reordered[,40:65],1,max, na.rm=TRUE)

# Populate the remaining vectors with the ordered Continuous_2011 data 
for (i in 1:N.plants){
  index<-which.max(Continuous_2011_reordered[i,7:39]) #Columns 7:39 is maxShootCounts
  DoYmaxShootCount[i]<-ShootCountsDaysofYear[index]
  index<-which.max(Continuous_2011_reordered[i,40:65]) #Columns 40:65 is maxCanopyHeight
  DoYmaxCanopyHeight[i]<-CanopyHeightsDaysofYear[index]
  index<-min(which(Continuous_2011_reordered[i,66:80]=="A"))
  if (index==Inf) FloweringStageDoYFirstA[i]<-NA
  if (index!=Inf) FloweringStageDoYFirstA[i]<-FloweringStageDaysofYear[index]
  index<-min(which(Continuous_2011_reordered[i,66:80]=="F"))
  if (index==Inf) FloweringStageDoYFirstF[i]<-NA
  if (index!=Inf) FloweringStageDoYFirstF[i]<-FloweringStageDaysofYear[index]
}

# Copying maxShootCounts into the maxShootCount column
Continuous_2011_reordered$maxShootCount <- maxShootCounts
# Copying DoYmaxShootCount into the doYmaxShotoCount column
Continuous_2011_reordered$DoYmaxShootCount <- DoYmaxShootCount
# Copying maxCanopyHeight into the maxCanopyHeight column
Continuous_2011_reordered$maxCanopyHeight <- maxCanopyHeight
# Copying DoYmaxCanopyHeight into the DoYmaxCanopyHeight column
Continuous_2011_reordered$DoYmaxCanopyHeight <- DoYmaxCanopyHeight
# Copying FloweringStageDoYFirstA into the FloweringStageDoYFirstA column
Continuous_2011_reordered$FloweringStageDoYFirstA <- FloweringStageDoYFirstA
# Copying FloweringStageDoYFirstF into the FloweringStageDoYFirstF column
Continuous_2011_reordered$FloweringStageDoYFirstF <- FloweringStageDoYFirstF

# Ordering the columns
Continuous_2011_reordered <- Continuous_2011_reordered[,c(1:39,81:82,40:65,83:84,66:80,85:86)]

# Writing the ordered data to new csv files - WINDOWS
write.csv(Single_2011_reordered, "./Data/Single_2011_clean.csv", row.names=FALSE)
write.csv(Continuous_2011_reordered, "./Data/Continuous_2011_clean.csv", row.names=FALSE)

# Writing the ordered data to new csv files - MAC
write.csv(Single_2011_reordered, "../Data/Single_2011_clean.csv", row.names=FALSE)
write.csv(Continuous_2011_reordered, "../Data/Continuous_2011_clean.csv", row.names=FALSE)