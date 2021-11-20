library(ggplot2)
library(ggforce)

a<-read.table("/Users/hilynano/Dropbox/Sim_clade_paper/sim_Y\ paper/plot/Transposon/rDNA_plot.txt",header=T)
a$Species <- factor(a$Species, levels=c("Dmel","Dmau","Dsim","Dsech"))
a$chr <- factor(a$chr, levels=c("A","X","U","Y"))
a$TE <- factor(a$TE, levels=c("5.8S","18S","28S","ITS1","ITS2","ETS","IGS"))
P<-ggplot(a, aes(x=chr, y=len_proportion*100))+ geom_jitter(inherit.aes = TRUE,alpha=0.5,size=0.5)+theme_light()+ facet_grid(a$TE~a$Species)+theme(strip.text = element_text(size = 12, colour = "white"))+ylab("Length(%)")+xlab("Chromosome")+coord_cartesian( ylim = c(0, 120))
ggsave("/Users/hilynano/Dropbox/Sim_clade_paper/sim_Y\ paper/plot/Transposon/rDNA.pdf",P,width = 8, height = 8,device = "pdf")
