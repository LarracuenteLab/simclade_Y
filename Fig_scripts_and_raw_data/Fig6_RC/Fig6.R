library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
data_more<-read.table("/Users/hilynano/Dropbox/forCH/Y_chromsome_paper/Submission_RC2/scripts_and_raw_data/Fig_scripts_and_raw_data/Fig6_RC/Fig6.Indel_Y_2021_RC2.txt",header=T)
data_more$pos <- factor(data_more$pos, levels=c("Y", "sim_A","sim_X","sim_4","mel_A", "mel_X","mel_pseudogene","het_pop"))
data_more$species <- factor(data_more$species, levels=c("mel", "sim_clade","mau","sim", "sec"))
data<-subset(data_more,data_more$pos != 'het_pop' )
data_Y<-subset(data,data$pos == 'Y' )

#Y_only
binCut <- c(-1000,-100,-10:-1,0,1:10,100,1000)
totaldataset <- data.frame()
for (i in 1:(length(levels(data_Y$species))))
{

    genome<- data.frame()
    genome<-subset(data_Y,data_Y$species == levels(data_Y$species)[i] )
    genomesum<-length(genome$indel)
    print (length(genome$indel))
    dataset <- data.frame()
    
    for (k in 1:(length(binCut)-1))
    {
      dataset <- rbind(dataset, c(binCut[k],sum(genome$indel>=binCut[k] & genome$indel<binCut[k+1])/genomesum)) 
      
      
    }
    colnames(dataset) <- c('lower_bin','density')
    dataset$lower_bin <- as.factor(dataset$lower_bin)
    dataset$species<-levels(data_Y$species)[i]
    
    totaldataset<-rbind(totaldataset,dataset)
  }
totaldataset$species <- factor(totaldataset$species, levels=c("mel", "sim_clade","mau","sim", "sec"))
cbPalette <-  c("black","#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#000000")
P<-ggplot(totaldataset, aes(x=lower_bin, y=density,fill=species)) + geom_bar(stat="identity",position="dodge")+scale_fill_manual(values=cbPalette)+ylab("Frequency(%)")+xlab("Indel Length")+theme(text = element_text(size=20))+theme_bw()
ggsave("/Users/hilynano/Desktop/indel_Yonly.pdf",P,width = 9, height = 6,device = "pdf")+theme_bw()

#no het pseudo
binCut <- c(-1000,-100,-10:-1,0,1:10,100,1000)
totaldataset <- data.frame()
for (i in 1:(length(levels(data$pos))))
{
  
  genome<- data.frame()
  genome<-subset(data,data$pos == levels(data$pos)[i] )
  genomesum<-length(genome$indel)
  print (length(genome$indel))
  dataset <- data.frame()
  
  for (k in 1:(length(binCut)-1))
  {
    dataset <- rbind(dataset, c(binCut[k],sum(genome$indel>=binCut[k] & genome$indel<binCut[k+1])/genomesum)) 
    
    
  }
  colnames(dataset) <- c('lower_bin','density')
  dataset$lower_bin <- as.factor(dataset$lower_bin)
  dataset$pos<-levels(data$pos)[i]
  totaldataset<-rbind(totaldataset,dataset)
}
totaldataset$pos <- factor(totaldataset$pos, levels=c("Y", "sim_A","sim_X","sim_4","mel_A", "mel_X","mel_pseudogene"))
cbPalette <-  c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
P<-ggplot(totaldataset, aes(x=lower_bin, y=density,fill=pos)) + geom_bar(stat="identity",position="dodge")+scale_fill_manual(values=cbPalette)+ylab("Frequency(%)")+xlab("Indel Length")+theme(text = element_text(size=20))+theme_bw()
ggsave("/Users/hilynano/Desktop/indel.pdf",P,width = 9, height = 6,device = "pdf")+theme_bw()

#all data
binCut <- c(-1000,-100,-10:-1,0,1:10,100,1000)
totaldataset <- data.frame()
for (i in 1:(length(levels(data_more$pos))))
{
  
  genome<- data.frame()
  genome<-subset(data_more,data_more$pos == levels(data_more$pos)[i] )
  genomesum<-length(genome$indel)
  print (length(genome$indel))
  dataset <- data.frame()
  
  for (k in 1:(length(binCut)-1))
  {
    dataset <- rbind(dataset, c(binCut[k],sum(genome$indel>=binCut[k] & genome$indel<binCut[k+1])/genomesum)) 
    
    
  }
  colnames(dataset) <- c('lower_bin','density')
  dataset$lower_bin <- as.factor(dataset$lower_bin)
  dataset$pos<-levels(data_more$pos)[i]
  totaldataset<-rbind(totaldataset,dataset)
}
totaldataset$pos <- factor(totaldataset$pos, levels=c("Y", "sim_A","sim_X","sim_4","mel_A", "mel_X","mel_pseudogene","het_pop"))
cbPalette <-  c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
P<-ggplot(totaldataset, aes(x=lower_bin, y=density,fill=pos)) + geom_bar(stat="identity",position="dodge")+scale_fill_manual(values=cbPalette)+ylab("Frequency(%)")+xlab("Indel Length")+theme(text = element_text(size=20))+theme_bw()
ggsave("/Users/hilynano/Desktop/indel_all.pdf",P,width = 9, height = 6,device = "pdf")+theme_bw()

data_minus1<-subset(data_more,data_more$indel > 3  | data_more$indel < -3)
binCut <- c(-1000,-100,-10:-1,0,1:10,100,1000)
totaldataset <- data.frame()
for (i in 1:(length(levels(data_minus1$pos))))
{
  
  genome<- data.frame()
  genome<-subset(data_minus1,data_minus1$pos == levels(data_minus1$pos)[i] )
  genomesum<-length(genome$indel)
  print (length(genome$indel))
  dataset <- data.frame()
  
  for (k in 1:(length(binCut)-1))
  {
    dataset <- rbind(dataset, c(binCut[k],sum(genome$indel>=binCut[k] & genome$indel<binCut[k+1])/genomesum)) 
    
    
  }
  colnames(dataset) <- c('lower_bin','density')
  dataset$lower_bin <- as.factor(dataset$lower_bin)
  dataset$pos<-levels(data_more$pos)[i]
  totaldataset<-rbind(totaldataset,dataset)
}
totaldataset$pos <- factor(totaldataset$pos, levels=c("Y", "sim_A","sim_X","sim_4","mel_A", "mel_X","mel_pseudogene","het_pop"))
cbPalette <-  c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
P<-ggplot(totaldataset, aes(x=lower_bin, y=density,fill=pos)) + geom_bar(stat="identity",position="dodge")+scale_fill_manual(values=cbPalette)+ylab("Frequency(%)")+xlab("Indel Length")+theme(text = element_text(size=20))+theme_bw()
ggsave("/Users/hilynano/Desktop/indel_1.pdf",P,width = 9, height = 6,device = "pdf")+theme_bw()