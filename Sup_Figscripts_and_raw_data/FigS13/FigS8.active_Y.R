data<-read.table("/Users/hilynano/Dropbox/Sim_clade_paper/sim_Y\ paper/plot/Transposon/changes_species.txt",na.strings = "NA",fill=T,header=T)

mel_sim<-subset(data,(data$mel_other+data$sim_other+data$mel_Y+data$sim_Y)>=100000)
mel_sim_O<-log((mel_sim$mel_other+1)/(mel_sim$sim_other+1),2)
mel_sim_Y<-log((mel_sim$mel_Y+1)/(mel_sim$sim_Y+1),2)

mel_mau<-subset(data,(data$mel_other+data$mau_other+data$mel_Y+data$mau_Y)>=100000)
mel_mau_O<-log((mel_mau$mel_other+1)/(mel_mau$mau_other+1),2)
mel_mau_Y<-log((mel_mau$mel_Y+1)/(mel_mau$mau_Y+1),2)

mel_sec<-subset(data,(data$mel_other+data$sec_other+data$mel_Y+data$sec_Y)>=100000)
mel_sec_O<-log((mel_sec$mel_other+1)/(mel_sec$sec_other+1),2)
mel_sec_Y<-log((mel_sec$mel_Y+1)/(mel_sec$sec_Y+1),2)

mau_sim<-subset(data,(data$mau_other+data$sim_other+data$mau_Y+data$sim_Y)>=100000)
mau_sim_O<-log((mau_sim$mau_other+1)/(mau_sim$sim_other+1),2)
mau_sim_Y<-log((mau_sim$mau_Y+1)/(mau_sim$sim_Y+1),2)

sec_sim<-subset(data,(data$sec_other+data$sim_other+data$sec_Y+data$sim_Y)>=100000)
sec_sim_O<-log((sec_sim$sec_other+1)/(sec_sim$sim_other+1),2)
sec_sim_Y<-log((sec_sim$sec_Y+1)/(sec_sim$sim_Y+1),2)

sec_mau<-subset(data,(data$sec_other+data$mau_other+data$sec_Y+data$mau_Y)>=100000)
sec_mau_O<-log((sec_mau$sec_other+1)/(sec_mau$mau_other+1),2)
sec_mau_Y<-log((sec_mau$sec_Y+1)/(sec_mau$mau_Y+1),2)

#xlim=c(0,3000),ylim=c(0,3000)
par(lwd=4,oma=c(3, 3, 7, 7),mar=c(1,1,1,1))
layout(matrix(1:16,4,4))
plot(mel_sim_O,mel_sim_Y,pch=20,cex=1,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,"Dmel",cex=4)
plot(mel_mau_O,mel_mau_Y,pch=20,cex=1,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,formatC(cor.test(mel_mau_O,mel_mau_Y,method = c("spearman"))$estimate,digits=3, format="f"),cex=3)
plot(mel_sim_O,mel_sim_Y,pch=20,cex=1,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,formatC(cor.test(mel_sim_O,mel_sim_Y,method = c("spearman"))$estimate,digits=3, format="f"),cex=3)
plot(mel_mau_O,mel_mau_Y,pch=20,cex=1,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,formatC(cor.test(mel_sec_O,mel_sec_Y,method = c("spearman"))$estimate,digits=3, format="f"),cex=3)

plot(mel_mau_O,mel_mau_Y,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20))
axis(3,labels=c("-20","-10","0","10","20"),at=c(-20,-10,0,10,20),line=NA, cex.axis=1.5, lwd.ticks=5,tck=-0.02, mgp=c(1,0.5,0))
plot(data$V2,data$V1,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,"Dmau",cex=4)
plot(data$V1,data$V2,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,formatC(cor.test(mau_sim_O,mau_sim_Y,method = c("spearman"))$estimate,digits=3, format="f"),cex=3)
plot(data$V1,data$V2,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,formatC(cor.test(sec_mau_O,sec_mau_Y,method = c("spearman"))$estimate,digits=3, format="f"),cex=3)

plot(mel_sim_O,mel_sim_Y,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20))
axis(3,labels=c("-20","-10","0","10","20"),at=c(-20,-10,0,10,20),line=NA, cex.axis=1.5, lwd.ticks=5,tck=-0.02, mgp=c(1,0.5,0))
plot(mau_sim_O,mau_sim_Y,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20))
plot(data$V1,data$V2,pch=20,cex=1,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,"Dsim",cex=4)
plot(data$V1,data$V2,pch=20,cex=1,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,formatC(cor.test(sec_sim_O,sec_sim_Y,method = c("spearman"))$estimate,digits=3, format="f"),cex=3)

plot(mel_sec_O,mel_sec_Y,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20))
axis(3,labels=c("-20","-10","0","10","20"),at=c(-20,-10,0,10,20),line=NA, cex.axis=1.5, lwd.ticks=5,tck=-0.02, mgp=c(1,0.5,0))
axis(4,labels=c("-20","-10","0","10","20"),at=c(-20,-10,0,10,20),line=NA, cex.axis=1.5, lwd.ticks=5,tck=-0.02, mgp=c(1,0.5,0))
plot(sec_mau_O,sec_mau_Y,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20))
axis(4,labels=c("-20","-10","0","10","20"),at=c(-20,-10,0,10,20),line=NA, cex.axis=1.5, lwd.ticks=5,tck=-0.02, mgp=c(1,0.5,0))
plot(sec_sim_O,sec_sim_Y,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=1,xlim=c(-20,20),ylim=c(-20,20))
axis(4,labels=c("-20","-10","0","10","20"),at=c(-20,-10,0,10,20),line=NA, cex.axis=1.5, lwd.ticks=5,tck=-0.02, mgp=c(1,0.5,0))
plot(data$V1,data$V2,pch=20,cex=0.5,ann=FALSE,axes=FALSE,frame.plot=TRUE,lwd=3,xlim=c(-20,20),ylim=c(-20,20),type="n")
text(0,0,"Dsech",cex=4)

jpeg("/Users/hilynano/Dropbox/Sim_clade_paper/sim_Y\ paper/plot/file2.jpg",width = 4800, height = 4800)



dev.off()

 