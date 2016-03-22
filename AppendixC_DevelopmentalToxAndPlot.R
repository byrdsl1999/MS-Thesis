Appendix C: R Script for Insect toxicity experiments and plot

library("pwr")

Treat1<-c("DI", "NaCl", "Penicillin", "Neomycin")
Lifespan1<-c(5.87, 5.73, 6, 6.2)
exp1<-data.frame(Treat1,Lifespan1)
barplot(exp1[,2], names.arg=exp1[,1])

#DI is deionized water. NaCl is salt. Ampicillin is Ampicillin. 
#Neomycin is neomycin sulfate. Penicillin is Penicillin-G hydrate.
#DaL- Died as Larva. DaP- Died as Pupa. DaA- Died as adult. 
#Dtp- Time in days from removal from cold storage to pupation. 
#DtE- Time from pupation to emergence. 
#DtA- Time from emergence to death as an adult.
ID<-1:80
Treat2<-factor(c(rep("DI",16), rep("NaCl", 16), rep("Ampicillin", 16), rep("Neomycin",16), rep("Penicillin", 16)))

DaL<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0)
DaP<-c(0,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,1,1,0,1,1,1,0,0,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,1,1)
DaA<-c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,1,0,1,0,1,1,0,0,0,0,0,0,0,1,0,0,0)
DtP<-c(3,4,3,6,5,4,4,6,4,4,4,4,3,4,NA,3,3,NA,7,3,4,5,5,NA,NA,3,3,5,3,10,NA,NA,NA,NA,NA,6,3,NA,4,3,NA,4,4,3,NA,3,3,3,5,6,5,4,3,3,3,4,4,5,4,4,3,5,3,3,NA,3,NA,5,5,3,3,3,4,3,3,4,3,NA,6,6)
DtE<-c(17,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,NA,NA,NA,19,NA,NA,NA,NA,NA,NA,NA,NA,19,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,17,NA,16,NA,NA,NA,NA,NA,16,NA,NA,NA,NA,NA,19,NA,NA,16,17,NA,17,NA,20,21,NA,NA,NA,NA,NA,NA,NA,18,NA,NA,NA)
DtA<-c(2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0, NA,NA,NA,3, NA,NA,NA,NA,NA,NA,NA,NA,2 ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,10,NA,0, NA,NA,NA,NA,NA,10,NA,NA,NA,NA,NA,3 ,NA,NA, 5, 5,NA, 4,NA, 1, 6,NA,NA,NA,NA,NA,NA,NA, 0,NA,NA,NA)

#Constructing the dataframe
exp2<-data.frame(ID,Treat2,DaL,DaP,DaA,DtP,DtE,DtA)
#Ordering the factors so that controls come first, and so that factors aren't order alphabetically.
exp2$Treat2 <- factor(exp2$Treat2,c("DI","NaCl","Ampicillin","Neomycin","Penicillin"))

#Anovas on the various measures of fly health, by treatment.
aov.DaL<-aov(DaL~Treat2, data=exp2)
summary(aov.DaL)
aov.DaP<-aov(DaP~Treat2, data=exp2)
summary(aov.DaP)
aov.DaA<-aov(DaA~Treat2, data=exp2)
summary(aov.DaA)

aov.DtP<-aov(DtP~Treat2, data=exp2)
summary(aov.DaP)
aov.DtE<-aov(DtE~Treat2, data=exp2)
summary(aov.DtE)
aov.DtA<-aov(DtA~Treat2, data=exp2)
summary(aov.DtA)

#Power tests
#This should yield our detectable effect size, assuming power of 0.8
effect<-pwr.anova.test(k=5, n=16, sig.level=.05, power=.8)
effect

#Now to determine the power in each test
#Group the treatment means by measure, to determine effect size. Effect=SDgroupmeans/SDglobalmean
DaLmean<-c(mean(DaL[Treat2=="DI"]),mean(DaL[Treat2=="NaCl"]),mean(DaL[Treat2=="Amplicillin"]),mean(DaL[Treat2=="Neomycin"]),mean(DaL[Treat2=="Penicillin"]))
DaPmean<-c(mean(DaP[Treat2=="DI"]),mean(DaP[Treat2=="NaCl"]),mean(DaP[Treat2=="Amplicillin"]),mean(DaP[Treat2=="Neomycin"]),mean(DaP[Treat2=="Penicillin"]))
DaAmean<-c(mean(DaA[Treat2=="DI"]),mean(DaA[Treat2=="NaCl"]),mean(DaA[Treat2=="Amplicillin"]),mean(DaA[Treat2=="Neomycin"]),mean(DaA[Treat2=="Penicillin"]))
DtPmean<-c(mean(DtP[Treat2=="DI"], na.rm = TRUE),mean(DtP[Treat2=="NaCl"], na.rm = TRUE),mean(DtP[Treat2=="Amplicillin"], na.rm = TRUE),mean(DtP[Treat2=="Neomycin"], na.rm = TRUE),mean(DtP[Treat2=="Penicillin"], na.rm = TRUE))
DtEmean<-c(mean(DtE[Treat2=="DI"], na.rm = TRUE),mean(DtE[Treat2=="NaCl"], na.rm = TRUE),mean(DtE[Treat2=="Amplicillin"], na.rm = TRUE),mean(DtE[Treat2=="Neomycin"], na.rm = TRUE),mean(DtE[Treat2=="Penicillin"], na.rm = TRUE))
DtAmean<-c(mean(DtA[Treat2=="DI"], na.rm = TRUE),mean(DtA[Treat2=="NaCl"], na.rm = TRUE),mean(DtA[Treat2=="Amplicillin"], na.rm = TRUE),mean(DtA[Treat2=="Neomycin"], na.rm = TRUE),mean(DtA[Treat2=="Penicillin"], na.rm = TRUE))
#Effect=SDgroupmeans/SDglobalmean
DaLef<-sd(DaLmean)/sd(DaL)
DaPef<-sd(DaPmean)/sd(DaP)
DaAef<-sd(DaAmean)/sd(DaA)
DtPef<-sd(DtPmean)/sd(DtP, na.rm = TRUE)
DtEef<-sd(DtEmean)/sd(DtE, na.rm = TRUE)
DtAef<-sd(DtAmean)/sd(DtA, na.rm = TRUE)

#Determining what power each test has.
pwr.anova.test(k=5, n=16, f=DaLef, sig.level=.05)
pwr.anova.test(k=5, n=16, f=DaPef, sig.level=.05)
pwr.anova.test(k=5, n=16, f=DaAef, sig.level=.05)
#This is a bit inaccurate. Since many flies didn't survive to emerge, we have to count those who did. 
#We assume equal groups, which we know to be false. So that's going to overestmate power.
pwr.anova.test(k=5, n=sum(!is.na(DtP))/5, f=DtPef, sig.level=.05)
pwr.anova.test(k=5, n=sum(!is.na(DtE))/5, f=DtEef, sig.level=.05)
pwr.anova.test(k=5, n=sum(!is.na(DtA))/5, f=DtAef, sig.level=.05)
#These below should tell you what difference this effect size means for each of the variables.
DaLsd<-sd(DaL)
DaLsd * effect[[3]]
DaPsd<-sd(DaP)
DaPsd * effect[[3]]
DaAsd<-sd(DaA)
DaAsd * effect[[3]]
DtPsd<-sd(DtP)
DtPsd * effect[[3]]
DtEsd<-sd(DtE)
DtEsd * effect[[3]]
DtAsd<-sd(DtA)
DtAsd * effect[[3]]

#As only DaL is significant. Posthoc only run on that.
pairwise.t.test(DaL, Treat2, p.adjust.method="bonf")


#Making the plots
par(mfcol=c(3,2), mar=c(2.5,3.5,1,.2))
boxplot(DaL~Treat2, data=exp2, title="Died as Larva")
title(main="Died as Larva")
text(1,.3, "ab", cex=1)
text(2,.3, "ab", cex=1)
text(3,.3, "a", cex=1)
text(4,.3, "b", cex=1)
text(5,.3, "ab", cex=1)
boxplot(DaP~Treat2, data=exp2, title="Died as Pupa")
title(main="Died as Pupa")
mtext("Proportion of flies dying", 2,2)
boxplot(DaA~Treat2, data=exp2, title="Died as Adult")
title(main="Died as Adult")


boxplot(DtP~Treat2, data=exp2)
title(main="Days as Larva")
boxplot(DtE~Treat2, data=exp2)
title(main="Days as Pupa")
mtext("Days alive, in specified state",2,2)
boxplot(DtA~Treat2, data=exp2)
title(main="Days as Adult")

