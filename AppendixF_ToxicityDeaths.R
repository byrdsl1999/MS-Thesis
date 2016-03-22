#Input data
DaL2<-c(1,5,6,0,3)
DaP2<-c(13,9,8,12,9)
DaA2<-c(2,2,2,4,4)
labs2<-c("DI", "NaCl", "Ampicillin", "Neomycin", "Penicillin")
labs1<-c("Died as Larva", "Died as Pupa", "Died as Imago")
Da2<-data.frame(cbind(DaL2,DaP2,DaA2), row.names=labs2)

par(mar=c(2.1, 3.1, 1.6, 8.1), xpd=TRUE, mfcol=c(1,1))

prop = prop.table(as.matrix(Da2),margin=1)
barplot(t(prop), col=gray.colors(length(rownames(prop))), width=2)
legend("topright",inset=c(-0.475,0), fill=gray.colors(length(rownames(prop))), legend=labs1, bty="n")
mtext("Proportion of flies dying", 2,2)

