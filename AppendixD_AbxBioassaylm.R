InhibitionGrowthCurvesForR <- read.csv("~/Dropbox/Thesis/Thesis copy/Data/Data/InhibitionGrowthCurvesForR.csv", sep=";")
CleanData <- InhibitionGrowthCurvesForR[which(InhibitionGrowthCurvesForR$slid==0 & InhibitionGrowthCurvesForR$Observable.inhibition==1 & InhibitionGrowthCurvesForR$Interference==0),]

DoxData<-CleanData[CleanData$Treatment=="Doxycyclin",]
NeoData<-CleanData[CleanData$Treatment=="Neomycin",]
PenData<-CleanData[CleanData$Treatment=="Penicillin",]

par(mfrow=c(3,1), mar=c(3.3,3.2,.2,2))

plot(Average.Inhibition~Log.Conc, data=DoxData, xlab=NA, ylab=NA, ylim=c(6, 25))
Doxlm <- lm(Average.Inhibition~Log.Conc, data=DoxData)
abline(Doxlm)
abline(h=7.4, lty=5)
text(-1.5,20, "Doxycycline")
text(-1.5, 15, "y=5.69x+16.98")
text(.7, 11, expression("R"^2~"=0.925"))

plot(Average.Inhibition~Log.Conc, data=NeoData, xlab=NA, ylab=NA, ylim=c(6, 22))
Neolm <- lm(Average.Inhibition~Log.Conc, data=NeoData)
abline(Neolm)
abline(h=7.4, lty=5)
text(-1.7,18, "Neomycin")
text(-1.7, 15, "y=3.87x+15.70")
text(.9, 11, expression("R"^2~"=0.887"))
mtext("Inhibition diameter(mm)",2,2)

plot(Average.Inhibition~Log.Conc, data=PenData, xlab=NA, ylab=NA, ylim=c(6, 36))
Penlm <- lm(Average.Inhibition~Log.Conc, data=PenData)
abline(Penlm)
abline(h=7.4, lty=5)
text(-1,29, "Penicillin")
text(-1, 23, "y=12.43x+28.79")
text(.5, 13, expression("R"^2~"=0.866"))
mtext("Log Concentration",1,2)


