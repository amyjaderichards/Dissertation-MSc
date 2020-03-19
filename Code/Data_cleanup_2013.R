# 2013 data

Single_2013<-read.csv("./Data/Single_Miscanthus_2013.csv", skip=5)
Continuous_2013<-read.csv("./Data/Continuous_Miscanthus_2013.csv", skip=5)

#A list of rows that are dead plants i.e. 0 and NA values only
dead.plants <- c(6,14,38,39,68,69,83,110,116,119,120,126,144,166,172,185,191,193,201,204,238,310)

#Removing the dead plants from the dataframes
Single_2013 <- Single_2013[-c(dead.plants),]
Continuous_2013 <- Continuous_2013[-c(dead.plants),]

Genotype <- as.character(Single_2013$Mx.Number)
index.goliath<-grep("oliath",Genotype)
index.giganteus<-grep("iganteus",Genotype)
combined.index<-c(index.goliath,index.giganteus)

N<-length(Genotype)
for(i in 1:N){
  if(! i %in% combined.index) Genotype[i] <- as.numeric(strsplit(Genotype[i], "#")[[1]][2])
  if (i %in% index.goliath) Genotype[i]<-"Gol"
  if (i %in% index.giganteus) Genotype[i]<-"Gig"
}


Genotype.copy<-factor(Genotype,levels=c("",1:120,"Gig","Gol"),ordered=TRUE)
Single_2013$Genotype<-Genotype.copy

identical(Single_2013$Mx.Number,Continuous_2013$Mx.Number)
# Since the two vectors are identical, we proceed to copy Genotype.copy
# across to the Continuous_2013 data frame

Continuous_2013$Genotype<-Genotype.copy
Genotype.order<-order(Genotype.copy)
Single_2013_reordered<-Single_2013[Genotype.order,]
Continuous_2013_reordered<-Continuous_2013[Genotype.order,]

Single_2013_reordered$meanStemDiam<-round(apply(Single_2013_reordered[,c("StemDiam1","StemDiam2","StemDiam3")],1,mean),1)
Single_2013_reordered$sdStemDiam<-round(apply(Single_2013_reordered[,c("StemDiam1","StemDiam2","StemDiam3")],1,sd),1)

# In the following, DoY means "Day of Year"

Single_2013_reordered$SenescedGreaterThan80<-factor(Single_2013_reordered$SenescedGreaterThan80,levels=levels(Single_2013$FloweringIntensityLessThan50))

Single_2013_reordered<-Single_2013_reordered[,c(1:5,21,8:11,22,12:20)]


extract.first.element<-function(data.vector){
  out<-data.vector[1]
}

temp<-gsub("X","EmergenceDoY",colnames(Continuous_2013_reordered)[7:38])
colnames(Continuous_2013_reordered)[7:38]<-temp


#
temp2<-gsub("X","CanopyHeightDoY",unlist(lapply(strsplit(colnames(Continuous_2013_reordered)[40:71],"[.]"),extract.first.element)))
colnames(Continuous_2013_reordered)[40:71]<-temp2

#
temp3<-gsub("X","FloweringStageDoY",unlist(lapply(strsplit(colnames(Continuous_2013_reordered)[73:104],"[.]"),extract.first.element)))
colnames(Continuous_2013_reordered)[73:104]<-temp3

# NAs introduced by coercion
Continuous_2013_reordered$CanopyHeightDoY162[which(Continuous_2013_reordered$CanopyHeight162=="")]<-NA
Continuous_2013_reordered$CanopyHeightDoY169[which(Continuous_2013_reordered$CanopyHeight169=="")]<-NA
Continuous_2013_reordered$CanopyHeightDoY162<-as.numeric(levels(Continuous_2013_reordered$CanopyHeightDoY162)[Continuous_2013_reordered$CanopyHeightDoY162])
Continuous_2013_reordered$CanopyHeightDoY169<-as.numeric(levels(Continuous_2013_reordered$CanopyHeightDoY169)[Continuous_2013_reordered$CanopyHeightDoY169])

#
Continuous_2013_reordered$FloweringStageDoY218[which(Continuous_2013_reordered$FloweringStageDoY218=="fa")]<-"af"
Continuous_2013_reordered$FloweringStageDoY218<-droplevels(Continuous_2013_reordered$FloweringStageDoY218)
Continuous_2013_reordered$FloweringStageDoY239[which(Continuous_2013_reordered$FloweringStageDoY239 =="fa")]<-"af"
Continuous_2013_reordered$FloweringStageDoY239[which(Continuous_2013_reordered$FloweringStageDoY239 =="ad")]<-"af"
Continuous_2013_reordered$FloweringStageDoY239<-droplevels(Continuous_2013_reordered$FloweringStageDoY239)

#
for (i in c(85:91,101:103)){
  Continuous_2013_reordered[,i]<-factor(Continuous_2013_reordered[,i],levels=levels(Continuous_2013_reordered[,92]))
}

#
EmergenceDaysofYear<-as.numeric(gsub("EmergenceDoY","",colnames(Continuous_2013_reordered)[7:38]))
CanopyHeightsDaysofYear<-as.numeric(gsub("CanopyHeightDoY","",colnames(Continuous_2013_reordered)[40:71]))
FloweringStageDaysofYear<-as.numeric(gsub("FloweringStageDoY","",colnames(Continuous_2013_reordered)[73:104]))

# Number of plants (302)
N.plants <- dim(Continuous_2013_reordered)[1]

# Creating empty vectors of length N.plants
DoYFirst3Emergence<-numeric(N.plants)
maxCanopyHeight<-numeric(N.plants)
DoYmaxCanopyHeight<-numeric(N.plants)
FloweringStageDoYFirstA<-numeric(N.plants)
FloweringStageDoYFirstF<-numeric(N.plants)

#
maxCanopyHeight<-apply(Continuous_2013_reordered[,40:71],1,max,na.rm=TRUE)
maxCanopyHeight[which(maxCanopyHeight==-Inf)]<-NA

#
for (i in 1:N.plants){
  index<-min(which(Continuous_2013_reordered[i,7:38]==3))
  if (index==Inf) DoYFirst3Emergence[i]<-NA
  if (index!=Inf) DoYFirst3Emergence[i]<-EmergenceDaysofYear[index]
  index<-which.max(Continuous_2013_reordered[i,40:71])
  if (length(index)==0) DoYmaxCanopyHeight[index]<-NA
  if (length(index)>0) DoYmaxCanopyHeight[i]<-CanopyHeightsDaysofYear[index]
  index<-min(which(Continuous_2013_reordered[i,73:104]=="a"))
  if (index==Inf) FloweringStageDoYFirstA[i]<-NA
  if (index!=Inf) FloweringStageDoYFirstA[i]<-FloweringStageDaysofYear[index]
  index<-min(which(Continuous_2013_reordered[i,73:104]=="f"))
  if (index==Inf) FloweringStageDoYFirstF[i]<-NA
  if (index!=Inf) FloweringStageDoYFirstF[i]<-FloweringStageDaysofYear[index]
}

#
Continuous_2013_reordered$DoYFirst3Emergence<-DoYFirst3Emergence
Continuous_2013_reordered$maxCanopyHeight<-maxCanopyHeight
Continuous_2013_reordered$DoYmaxCanopyHeight<-DoYmaxCanopyHeight
Continuous_2013_reordered$FloweringStageDoYFirstA<-FloweringStageDoYFirstA
Continuous_2013_reordered$FloweringStageDoYFirstF<-FloweringStageDoYFirstF

Continuous_2013_reordered<-Continuous_2013_reordered[,c(1:5,105,7:38,106,40:71,107:108,73:104,109:110)]

write.csv(Single_2013_reordered,"./Data/Single_2013_clean.csv",row.names=FALSE)
write.csv(Continuous_2013_reordered,"./Data/Continuous_2013_clean.csv",row.names=FALSE)
