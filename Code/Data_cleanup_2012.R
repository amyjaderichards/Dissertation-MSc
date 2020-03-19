# 2012 data

Single_2012<-read.csv("./Data/Single_Miscanthus_2012.csv", skip=5)
Continuous_2012<-read.csv("./Data/Continuous_Miscanthus_2012.csv", skip=5)

#A list of rows that are dead plants i.e. 0 and NA values only
dead.plants <- c(6,14,38,39,68,69,83,110,116,119,120,126,144,166,172,185,191,193,201,204,238,310)

#Removing the dead plants from the dataframes
Single_2012 <- Single_2012[-c(dead.plants),]
Continuous_2012 <- Continuous_2012[-c(dead.plants),]

#A list of characters of the Mx Numbers (Plant IDs) in the Single_2011 data i.e. " - Mx 1553#114"
Genotype <- as.character(Single_2012$Mx.Number)
Genotype2 <- as.character(Continuous_2012$Mx.Number)

identical(Genotype, Genotype2)

# A list of indexes of entries of Giganteus or Goliath
index.goliath<-grep("oliath",Genotype)
index.giganteus<-grep("iganteus",Genotype)
combined.index<-c(index.goliath,index.giganteus)

#If the Genotype is neither Giganteus or Goliath, strip the string to leave just the 3 digit ID number
#If the Genotype is Goliath change it to "Gol"
#If the Genotype is Giganteus change it go "Gig"
N<-length(Genotype)
for(i in 1:N){
  if(! i %in% combined.index) Genotype[i] <- as.numeric(strsplit(Genotype[i], "#")[[1]][2])
  if (i %in% index.goliath) Genotype[i]<-"Gol"
  if (i %in% index.giganteus) Genotype[i]<-"Gig"
}

Genotype.copy<-factor(Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
Single_2012$Genotype<-Genotype.copy

identical(Single_2012$Mx.Number,Continuous_2012$Mx.Number)
# Since the two vectors are identical, we proceed to copy Genotype.copy
# across to the Continuous_2012 data frame

# Creating a column Genotype in continuous_2012
Continuous_2012$Genotype<-Genotype.copy
# Ordering the genotypes
Genotype.order<-order(Genotype.copy)
# Sorting Single_2012 using Genotype.order
Single_2012_reordered<-Single_2012[Genotype.order,]
# Sorting Continuous_2012 using Genotype.order
Continuous_2012_reordered<-Continuous_2012[Genotype.order,]

# Creating a column for mean stem diameter
Single_2012_reordered$meanStemDiam <- round(apply(Single_2012_reordered[,c("StemDiam1","StemDiam2","StemDiam3")],1,mean),1)
# Creating a column for standard deviation of stem diameter
Single_2012_reordered$sdStemDiam <- round(apply(Single_2012_reordered[,c("StemDiam1","StemDiam2","StemDiam3")],1,sd),1)

# In the following, DoY means "Day of Year"

Single_2012_reordered$SenescedGreaterThan80<-factor(Single_2012_reordered$SenescedGreaterThan80,levels=levels(Single_2012$FloweringIntensityLessThan50))

# Reordering the columns - putting meanStemDiam in the column next to the stem diameter measurements etc
Single_2012_reordered<-Single_2012_reordered[,c(1:5,22,8:10,23:24,11:21)]

# A function that extracts the first element from a vector
extract.first.element<-function(data.vector){
  out<-data.vector[1]
}

#
temp<-as.character(Continuous_2012_reordered$ShootCountsDoY72)
index<-grep(" ",temp)
temp[index]<-unlist(lapply(strsplit(temp[index]," "),extract.first.element))
temp<-as.numeric(temp)
Continuous_2012_reordered$ShootCountsDoY72<-temp

#
temp<-gsub("X","EmergenceDoY",colnames(Continuous_2012_reordered)[12:28])
colnames(Continuous_2012_reordered)[12:28]<-temp

#
temp2<-gsub("X","CanopyHeightDoY",unlist(lapply(strsplit(colnames(Continuous_2012_reordered)[30:60],"[.]"),extract.first.element)))
colnames(Continuous_2012_reordered)[30:60]<-temp2

#
temp3<-gsub("X","FloweringStageDoY",unlist(lapply(strsplit(colnames(Continuous_2012_reordered)[63:83],"[.]"),extract.first.element)))
colnames(Continuous_2012_reordered)[63:83]<-temp3

#
temp4<-gsub("X","FloweringScoresDoY",unlist(lapply(strsplit(colnames(Continuous_2012_reordered)[85:89],"[.]"),extract.first.element)))
colnames(Continuous_2012_reordered)[85:89]<-temp4

#
for (i in 63:66){
  Continuous_2012_reordered[,i]<-factor(Continuous_2012_reordered[,i],levels=levels(Continuous_2012_reordered[,67]))
}

# Reordering the columns
Continuous_2012_reordered<-Continuous_2012_reordered[,c(1:5,90,8:9,12:28,30:60,63:83,85:89)]

#
ShootCountsDaysofYear<-as.numeric(gsub("ShootCountsDoY","",colnames(Continuous_2012_reordered)[7:8]))
EmergenceDaysofYear<-as.numeric(gsub("EmergenceDoY","",colnames(Continuous_2012_reordered)[9:25]))
CanopyHeightsDaysofYear<-as.numeric(gsub("CanopyHeightDoY","",colnames(Continuous_2012_reordered)[26:56]))
FloweringStageDaysofYear<-as.numeric(gsub("FloweringStageDoY","",colnames(Continuous_2012_reordered)[57:77]))
FloweringScoreDaysofYear<-as.numeric(gsub("FloweringScoresDoY","",colnames(Continuous_2012_reordered)[78:82]))

# The number of plants (302)
N.plants<-dim(Continuous_2012_reordered)[1]                                                         

# Creating empty vectors for each variable of length N.plants
maxShootCounts<-numeric(N.plants)
DoYmaxShootCount<-numeric(N.plants)
DoYFirst3Emergence<-numeric(N.plants)
maxCanopyHeight<-numeric(N.plants)
DoYmaxCanopyHeight<-numeric(N.plants)
FloweringStageDoYFirstA<-numeric(N.plants)
FloweringStageDoYFirstF<-numeric(N.plants)

#
maxShootCounts <- apply(Continuous_2012_reordered[,7:8],1,max)
maxCanopyHeight <- apply(Continuous_2012_reordered[,26:56],1,max,na.rm=TRUE)
maxCanopyHeight[which(maxCanopyHeight==-Inf)]<-NA

#
for (i in 1:N.plants){
  index<-which.max(Continuous_2012_reordered[i,7:8])
  DoYmaxShootCount[i]<-ShootCountsDaysofYear[index]
  index<-min(which(Continuous_2012_reordered[i,9:25]==3))
  if (index==Inf) DoYFirst3Emergence[i]<-NA
  if (index!=Inf) DoYFirst3Emergence[i]<-EmergenceDaysofYear[index]
  index<-which.max(Continuous_2012_reordered[i,26:56])
  if (length(index)==0) DoYmaxCanopyHeight[i]<-NA
  if (length(index)>0) DoYmaxCanopyHeight[i]<-CanopyHeightsDaysofYear[index]
  index<-min(which(Continuous_2012_reordered[i,57:77]=="a"))
  if (index==Inf) FloweringStageDoYFirstA[i]<-NA
  if (index!=Inf) FloweringStageDoYFirstA[i]<-FloweringStageDaysofYear[index]
  index<-min(which(Continuous_2012_reordered[i,57:77]=="f"))
  if (index==Inf) FloweringStageDoYFirstF[i]<-NA
  if (index!=Inf) FloweringStageDoYFirstF[i]<-FloweringStageDaysofYear[index]
}

Continuous_2012_reordered$maxShootCount <- maxShootCounts
Continuous_2012_reordered$DoYmaxShootCount <- DoYmaxShootCount
Continuous_2012_reordered$DoYFirst3Emergence <- DoYFirst3Emergence
Continuous_2012_reordered$maxCanopyHeight <- maxCanopyHeight
Continuous_2012_reordered$DoYmaxCanopyHeight <- DoYmaxCanopyHeight
Continuous_2012_reordered$FloweringStageDoYFirstA <- FloweringStageDoYFirstA
Continuous_2012_reordered$FloweringStageDoYFirstF <- FloweringStageDoYFirstF

# Reordering the columns
Continuous_2012_reordered<-Continuous_2012_reordered[,c(1:8,83:84,9:25,85,26:56,86:87,57:77,88:89,78:82)]

write.csv(Single_2012_reordered,"./Data/Single_2012_clean.csv", row.names=FALSE)
write.csv(Continuous_2012_reordered,"./Data/Continuous_2012_clean.csv",row.names=FALSE)



                                                     