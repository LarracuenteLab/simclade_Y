library(ggplot2)
a<-read.table("/Users/hilynano/Dropbox/Sim_clade_paper/sim_Y\ paper/coverage/mau_q0_plot.txt",header=T)
#a$location <- factor(a$location, levels=c("A", "X","Ahet","Xhet", "Y","U"))
a$location <- factor(a$location, levels=c("A", "X", "Y", "U"))
cbPalette <-  c("#E69F00", "#56B4E9", "#009E73",  "#000000")
P<-ggplot(a,aes(coverage,colour=location))+ geom_freqpoly(binwidth = 1,size=1.5)+ylab("count per 10Kb")+scale_colour_manual(values=cbPalette)+theme_bw()+xlim(c(0,250))
ggsave("/Users/hilynano/Desktop/mau_q0_plot.pdf",P,width = 4, height = 3,device = "pdf")+theme_bw()

