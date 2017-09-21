#Pull data frame from COW
#This forms the skeleton of the data fram
system<-read.csv(url("http://www.correlatesofwar.org/data-sets/state-system-membership/system2016/at_download/file/system2016.csv"))
#Pull monadic level data
#Polilty
polity<-read.table("http://privatewww.essex.ac.uk/~ksg/data/ksgp4use.asc", header=TRUE)
#CINC data
temp <- tempfile()
download.file("http://www.correlatesofwar.org/data-sets/national-material-capabilities/nmc-v5-1/at_download/file/NMC_5_0.zip",temp)
cinc<- read.csv(unz(temp, "NMC_5_0.csv"))
unlink(temp)
#Civil War Data
#UCDP Monadic Conflict Onset Dataset
UCDPMondadic<-read.csv(url("http://ucdp.uu.se/downloads/monadterm/ucdp-onset-conf-2014.csv"))
#Kristian Gleditsch’s Expanded Trade and GDP data
temp <- tempfile()
download.file("http://privatewww.essex.ac.uk/~ksg/data/exptradegdpv4.1.zip",temp)
Trade_GDP<- read.table(unz(temp, "pwt_v61.asc"), header=TRUE)
unlink(temp)
#Get MID Data
MIDS<-read.csv(url("http://vanity.dss.ucdavis.edu/~maoz/dyadmid20.csv"))






#create variables to merge
polity$POLITY[polity$POLITY==-66] <- NA
polity$POLITY[polity$POLITY==-77] <- NA
polity$POLITY[polity$POLITY==-88] <- NA
polity$POLITY[policy$POLITY==-88] <- NA
democ<-polity$POLITY
ccode<-polity$CCODE
year<-polity$YEAR
polity_data<-data.frame(year,ccode,democ)
summary(polity_data)

#CINC Data
cincscore<-cinc$cinc
ccode<-cinc$ccode
year<-cinc$year
cinc_data<-data.frame(year,ccode,cincscore)

year<-MIDS$YEAR
hostility<-MIDS$HIHOST
ccodeA<-MIDS$STATEA
ccodeB<-MIDS$STATEB
dyadid<-year*1000000+stateA*1000+stateB
dyadid<-format(dyadid,scientific=FALSE)
mid_data<-data.frame(dyadid,year,ccodeA,ccodeB,hostility)
summary(mid_data)

#UCDPMonadic and Trade
UCDPMondadic$ccode<-UCDPMondadic$gwno
Trade_GDP$ccode<-Trade_GDP$statenum

#Reorder variables
dataOrig<-system[c(2,3,1)]

library(plyr)
dataOrig<-join(dataOrig,cinc_data,type="left")
dataOrig<-join(dataOrig,polity_data,type="left")
dataOrig<-join(dataOrig,UCDPMondadic,type="left")
dataOrig<-join(dataOrig,Trade_GDP,type="left")
summary(dataOrig)

#Make Dyads
library(StratSel)
dyads<-makeDyadic(dataOrig,directed=FALSE, show.progress=5)
#Create dyadic id
dyadid<-dyads$sen_year*1000000+dyads$sen_ccode*1000+dyads$rec_ccode
dyadid<-format(dyadid,scientific=FALSE)
year<-dyads$sen_year
ccodeA<-dyads$sen_ccode
ccodeB<-dyads$rec_ccode
cincA<-dyads$sen_cincscore
cincB<-dyads$rec_cincscore
democA<-dyads$sen_democ
democB<-dyads$rec_democ
df<-data.frame(year,dyadid,ccodeA,ccodeB,cincA,cincB,democA,democB)

df<-join(df,mid_data,type="left")

df$hostility[ is.na(df$hostility) ] <- 0
df$mid<-df$hostility
df$mid[df$mid>0]<-1
df$war<-df$hostility
df$war[df$war<5]<-0
df$war[df$war==5]<-1

midlogit <- glm(mid ~ cincA*cincB+democA*democB, data = df, family = "binomial")
warlogit <- glm(war ~ cincA*cincB+democA*democB, data = df, family = "binomial")


#match