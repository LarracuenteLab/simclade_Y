setwd("/Users/hilynano/Dropbox/Sim_clade_paper/sim_Y\ paper/plot/Intron_number/")
exons <- read.delim(file='exon_copies.txt')
genespace <- c(rep(0.5,12),2,rep(0.5,15),2,rep(0.5,16),2,rep(0.5,2),2,rep(0.5,7),2,2,2,rep(0.5,5),2,rep(0.5,5),2,rep(0.5,5),2,rep(0.5,5))
pdf(file="canonical exons.pdf",height=8,width=12,pointsize=12)
par(mfrow=c(4,1))
par(mar=c(4,4,4,0))
barplot(t(as.matrix(exons$mel)),beside=T,space=genespace,ylim=c(0,16),yaxt='n')
text(x=40,y=14,cex=2, labels=substitute(paste(italic('D. melanogaster'))))
axis(side=2,cex.axis=2,at=c(0,4,8,12,16),line=0)
barplot(t(as.matrix(exons$sim)),beside=T,space=genespace,ylim=c(0,16),yaxt='n')
text(x=40,y=14,cex=2, labels=substitute(paste(italic('D. simulans'))))
axis(side=2,cex.axis=2,at=c(0,4,8,12,16),line=0)
barplot(t(as.matrix(exons$mau)),beside=T,space=genespace,ylim=c(0,16),yaxt='n')
text(x=40,y=14,cex=2, labels=substitute(paste(italic('D. mauritiana'))))
axis(side=2,cex.axis=2,at=c(0,4,8,12,16),line=0)
barplot(t(as.matrix(exons$sec)),beside=T,space=genespace,ylim=c(0,16),yaxt='n')
text(x=40,y=14,cex=2, labels=substitute(paste(italic('D. sechellia'))))
axis(side=2,cex.axis=2,at=c(0,4,8,12,16),line=0)
axis(side=1,cex.axis=2,at=c(10,33,58,75,84,94,103,113,123,134),tick=F,labels=c('kl-2',"kl-3","kl-5","PRY","ORY","Pp1*","CCY","Ppr-Y","ARY", "wdy"))

dev.off()




