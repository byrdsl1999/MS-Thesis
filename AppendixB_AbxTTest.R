library("pwr")

bidaily<-c(.194299866,.184013709,.3461129637)
daily<-c(.438263662,.136178488,.549486042,.440545941)
dbdtt<-t.test(bidaily,daily, alternative="less", mu=0, paired=FALSE, var.equal=FALSE)
dbdtt
normdbdtt<-t.test(bidaily/3,daily/7, alternative="two.sided", mu=0, paired=FALSE, var.equal=FALSE)
normdbdtt

ha<-(mean(daily)-mean(bidaily))/sd(c(daily,bidaily))
n1a<-length(daily)
n2a<-length(bidaily)
pwr.2p2n.test(h=ha,n1=n1a,n2=n2a)
pwr.2p2n.test(n1=n1a,n2=n2a, sig.level=.05, power=.8)

hb<-(mean(daily/7)-mean(bidaily/3))/sd(c(daily/7,bidaily/3))
n1b<-length(daily)
n2b<-length(bidaily)
pwr.2p2n.test(hb,n1b,n2b)

