insecttoxicity2plotdata <- read.csv("~/Documents/insecttoxicity2plotdata.csv")

begindate<-"06/07/13"
enddate<-"07/05/13"
bd<-as.Date(begindate, "%m/%d/%y")
ed<-as.Date(enddate, "%m/%d/%y")
datesa<-bd:ed
datesa
cb<-rbind(datesa,insecttoxicity2plotdata[1,3:31])
insecttoxicity2plotdata[1:80,3:31]
cb
plot(insecttoxicity2plotdata[1,3:31]~dates)
for i in 1:80{
    insecttoxicity2plotdata[i,3:31]

#Create a blank plot
par(mfrow=c(1,1), mar=c(4.3,4.3,.3,.3))
plot(1, type="n", xlab="Days", ylab="Devlopmental State", xlim=c(bd, ed), ylim=c(0, 2), xaxt="n", yaxt="n")
axis(2, at=c(0,1,2),labels=c("Larvae", "Pupae", "Imago"))
axis(1, at=c(15863, 15868, 15873, 15878, 15883, 15888), labels=c(0,5,10,15,20,25))
legend("topleft", inset=.02, cex=.7, c("DI", "Salt", "Ampicillin", "Neomycin", "Penicillin"), lty=c(1,1,1,1,1), lwd=c(3,3,3,3,3), col=c("red2", "lightseagreen", "goldenrod1", "mediumvioletred", "lightskyblue"))

#Add all the lines, as well as average lines
for (i in 1:16){
    lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(insecttoxicity2plotdata[i,3:31]),.4)), col="red2", lwd=.2)}
lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(colMeans(insecttoxicity2plotdata[1:16,3:31])),.3)), col="red2", lwd=4)


for (i in 17:32){
    lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(insecttoxicity2plotdata[i,3:31]),.4)), col="lightseagreen", lwd=.2)}
lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(colMeans(insecttoxicity2plotdata[17:32,3:31])),.3)), col="lightseagreen", lwd=4)


for (i in 33:48){
    lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(insecttoxicity2plotdata[i,3:31]),.4)), col="goldenrod1", lwd=.2)}
lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(colMeans(insecttoxicity2plotdata[33:48,3:31])),.3)), col="goldenrod1", lwd=4)


for (i in 49:64){
    lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(insecttoxicity2plotdata[i,3:31]),.4)), col="mediumvioletred", lwd=.2)}
lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(colMeans(insecttoxicity2plotdata[49:64,3:31])),.3)), col="mediumvioletred", lwd=4)


for (i in 65:80){
    lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(insecttoxicity2plotdata[i,3:31]),.4)), col="lightskyblue", lwd=.2)}
lines(smooth.spline(jitter(datesa,.3), jitter(as.numeric(colMeans(insecttoxicity2plotdata[65:80,3:31])),.3)), col="lightskyblue", lwd=4)


#Add Death Points
points(x=bd+0.5+jitter(insecttoxicity2plotdata[1:18,33],.5), y=jitter(insecttoxicity2plotdata[1:18,31],.3), col="red2", cex=1.5, pch=4)
points(x=bd+0.5+jitter(insecttoxicity2plotdata[17:32,33],.5), y=jitter(insecttoxicity2plotdata[17:32,31],.3), col="lightseagreen", cex=1.5, pch=4)
points(x=bd+0.5+jitter(insecttoxicity2plotdata[33:48,33],.5), y=jitter(insecttoxicity2plotdata[33:48,31],.3), col="goldenrod1", cex=1.5, pch=4)
points(x=bd+0.5+jitter(insecttoxicity2plotdata[49:64,33],.5), y=jitter(insecttoxicity2plotdata[49:64,31],.3), col="mediumvioletred", cex=1.5, pch=4)
points(x=bd+0.5+jitter(insecttoxicity2plotdata[65:80,33],.5), y=jitter(insecttoxicity2plotdata[65:80,31],.3), col="lightskyblue", cex=1.5, pch=4)



