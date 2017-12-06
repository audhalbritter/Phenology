# Phenology data 2015

# Libraries
library(reshape2); library(reshape); library(lme4)

# function to read in head of phenology data 2015
ReadInHeadPhenology15 <- function(datasheet, site){
  # import head of data set
  dat.h <- read.csv(datasheet, sep=";", header=FALSE, nrow=3, stringsAsFactors=FALSE)
  dat.h2 <- do.call(rbind,
                    lapply(seq(3,ncol(dat.h),20),function(i){
                      x <- dat.h[ ,c(i)]
                      names(x) <- c("date", "weather", "name")
                      x <- c(x,week=dat.h[ 1,i+18])
                      x <- c(x,Site=site)
                      x
                    })
  )
}

dath1 <- ReadInHeadPhenology15("DataSheet2015Hog.csv", "Hogsete")
dath2 <- ReadInHeadPhenology15("DataSheet2015Ram.csv", "Rambaera")
dath3 <- ReadInHeadPhenology15("DataSheet2015Ves.csv", "Veskre")
dath4 <- ReadInHeadPhenology15("DataSheet2015Lav.csv", "Lavisdalen")
dath5 <- ReadInHeadPhenology15("DataSheet2015Gud.csv", "Gudmedalen")
dath6 <- ReadInHeadPhenology15("DataSheet2015Skj.csv", "Skjellingahaugen")
meta.pheno <- rbind(dath1, dath2, dath3, dath4, dath5, dath6)



# function for reading in phenology data 2015
ReadInBodyPhenology15 <- function(datasheet, site){
  # import body of data
  dat <- read.csv(datasheet, header=FALSE, sep=";", skip=4, stringsAsFactors=FALSE)
  head(dat)
  dat <- dat[dat$V2!="",] # get rid of empty lines, where no species
  dat$V2<-gsub("*", "", dat$V2,fixed = TRUE) # get rid of * and space
  dat$V2<-gsub(" ", "", dat$V2,fixed = TRUE)
  
  # loop to get turfID in all cells
  for (i in 2:nrow(dat)){
    if(nchar(dat$V1[i])==0){
      dat$V1[i] <- dat$V1[i-1]
    }
  }
  # import head of data set
  dat.h <- read.csv(datasheet, sep=";", header=FALSE, nrow=3, stringsAsFactors=FALSE)
  
  # merge data into long data table
  dat.long <- do.call(rbind,
                      lapply(seq(3,ncol(dat)-19,20),function(i){
                        x <- dat[ ,c(1:2,i:(i+19))]
                        names(x) <- c("turfID", "species", paste(rep(c("v", "b", "f", "s", "r"), 4  ), rep(1:4, each=5), sep="."))
                        x$week<-dat.h[1,i+18]
                        x  
                      })
  )
  
  # Extract turfID
  dat.long$turfID <- sapply(strsplit(dat.long$turfID, split=" - ", fixed=TRUE), function(x) (x[2]                                                                                       
  ))
  dat.long$Site <- site
  # convert to factor and numeric
  dat.long <- cbind(dat.long[,c(1:3,8,13,18,23:24)],sapply(dat.long[,c(4:7,9:12,14:17,19:22)],as.numeric))
  dat.long$turfID <- as.factor(dat.long$turfID)
  dat.long$species <- as.factor(dat.long$species)
  dat.long$week <- as.factor(dat.long$week)
  dat.long$Site <- as.factor(dat.long$Site)
  dat.long
}

dat1 <- ReadInBodyPhenology15("DataSheet2015Hog.csv", "Hogsete")
dat2 <- ReadInBodyPhenology15("DataSheet2015Ram.csv", "Rambaera")
dat3 <- ReadInBodyPhenology15("DataSheet2015Ves.csv", "Veskre")
dat4 <- ReadInBodyPhenology15("DataSheet2015Lav.csv", "Lavisdalen")
dat5 <- ReadInBodyPhenology15("DataSheet2015Gud.csv", "Gudmedalen")
dat6 <- ReadInBodyPhenology15("DataSheet2015Skj.csv", "Skjellingahaugen")
pheno15 <- rbind(dat1, dat2, dat3, dat4, dat5, dat6)
pheno15$week.nr <- as.numeric(pheno15$week) # needs to be outside function, that week.nr across sites is consistent



# calculate the sum of buds, flowers and seeds per turf and species
CalcSums <- function(dat){
  dat$nr.b <- apply(dat[,c(seq(9,21,4))],1,sum, na.rm=TRUE)
  dat$nr.b[dat$nr.b == 0] <- NA
  dat$nr.f <- apply(dat[,c(seq(10,22,4))],1,sum, na.rm=TRUE)
  dat$nr.f[dat$nr.f == 0] <- NA
  dat$nr.s <- apply(dat[,c(seq(11,23,4))],1,sum, na.rm=TRUE)
  dat$nr.s[dat$nr.s == 0] <- NA
  dat$nr.r <- apply(dat[,c(seq(12,24,4))],1,sum, na.rm=TRUE)
  dat$nr.r[dat$nr.r == 0] <- NA
  return(dat)
}
pheno15 <- CalcSums(pheno15)


# read in turfs
turfs.15 <- read.csv("turfs.csv", sep=";", header=TRUE)
head(turfs.15)

# read in traits
traits.15 <- read.csv("trait.csv", sep=";", header=TRUE)
head(traits.15)


### for testing function
ddd <- subset(ph, ph$turfID=="104 TT1 102")
ddd <- subset(ddd, ddd$species=="Pot.ere")
res<-sort(unique(ddd$week.nr))[which.min(ddd$nr.f>0)] # find first flowering
res<-sort(unique(ddd$week.nr))[which.max(ddd$nr.f)] # peak
res<-sort(unique(ddd$week.nr),decreasing=TRUE)[which.min(rev(ddd$nr.f>0))] # end
dosm <- turfs.15$sm.15.w.nr[match(ddd$turfID, turfs.15$turfID)]
res - dosm




### GET FIRST, PEAK, END AND DURATION OF BUD SET, FLOWERING, SEED SET
# ph = phenology data set; ph.var = peak, first, end or duration; ph.stage = nr.b, br.f, nr.s, nr.r
# ph <- pheno15
phenology.var <- function(ph, ph.var, control.turf){ # for function
  # visualize the data (nr.fl per week)
  par(mfrow=c(4,4))
  pheno.vars <- by(ph, ph$turfID, function(tu){ # loop by turfID
    by(tu, tu$species, function(sp){ # loop by species
      if(sum(sp$nr.f, na.rm=TRUE) > 0){ # if all NA, then zero
        if(FALSE){
          matplot(sp$week.nr,(sp[,26:29]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          
        }
        if(control.turf=="origin"){
          dosm <- turfs.15$o.wsm15.wnr[match(sp$turfID, turfs.15$turfID)]
        }
        else if(control.turf=="destination"){
          dosm <- turfs.15$d.wsm15.wnr[match(sp$turfID, turfs.15$turfID)]
        }
        if(ph.var=="peak"){
          res<-sort(unique(sp$week.nr))[which.max(sp$nr.f)] # peak flowering (use unique for not to get them 4 times)
          res2 <- res - mean(dosm)
        }else if(ph.var=="first"){
          res<-sort(unique(sp$week.nr))[which.min(sp$nr.f>0)] # first flowering
          res2 <- res - mean(dosm)
        }else if(ph.var=="end"){
          res<-sort(unique(sp$week.nr),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] # end of flowering (reverse from first flowering)
          res2 <- res - mean(dosm)
        }else {stop("UNRECOGNISED PH.VAR")}
        
      }
      else return(NA)
    })     
  })
  pheno.vars2<-pheno.vars[sapply(pheno.vars,length)>0] # gets rid of turfs with no flowers
  pheno.vars2<-t(sapply(pheno.vars2, I))
  new.dat <- as.data.frame(pheno.vars2)
  ### MIGHT WANT TO CHANGE THIS; NOT ELEGANT
  new.dat[,144:156] <- turfs.15[match(rownames(new.dat), turfs.15$turfID), c(1:8,11,12,18:20)] # add turf info
  return(new.dat)
}

pheno.variables <- phenology.var(pheno15, "first", "origin")
head(pheno.variables)

# duration is special case: don't calc in days since sm, does not make sense!
#}else if(ph.var=="duration"){
#  end<-sort(unique(sp$week.nr),decreasing=TRUE)[which.min(rev(sp$nr.f>0))]
#  first<-sort(unique(sp$week.nr))[which.min(sp$nr.f>0)]
#  res<-end - (first-1) # duration of flowering



# Reshape phenology data: long species list
# input phenology.data, output from replace week
reshape.pheno1 <- function(dd){
  ### Wide to long data set (species in column)
  pheno.dat <- dd[,c(148,1:143)] # select turfID and species
  pheno.long <- melt(pheno.dat, id=c("turfID"))
  pheno.long <- na.omit(pheno.long) # remove all species with NA
  ### NOT ELEGANT
  pheno.long[,4:16] <- turfs.15[match(pheno.long$turfID, turfs.15$turfID), c(1:8,11,12,18:20)]
  pheno.long[,17:19] <- traits.15[match(pheno.long$variable, traits.15$species), c(2:4)] # add siteID, TTtreat, T_level,... to data set
  return(pheno.long)  
}
pheno.long <- reshape.pheno1(pheno.variables)






# for drawing graphs
### DESTINATION

reshape.pheno2 <- function(dd, control.turf){
  if(control.turf=="origin"){
    pheno.mean <- dcast(dd, siteID + variable  ~ TTtreat, mean)
    pheno.sd <- dcast(dd, siteID + variable  ~ TTtreat, sd)
  }
  else if(control.turf=="destination"){
    pheno.mean <- dcast(dd, destSiteID + variable  ~ TTtreat, mean)
    pheno.sd <- dcast(dd, destSiteID + variable  ~ TTtreat, sd)
  }
  pheno.sd <- pheno.sd[,3:7]
  colnames(pheno.sd) <- c("tt1.sd", "tt2.sd", "tt3.sd", "tt4.sd", "ttc.sd")
  pheno.treat <- cbind(pheno.mean,pheno.sd)
  pheno.treat$warm <- pheno.treat$TT2 - pheno.treat$TTC # check which species are earlier/later
  pheno.treat$wet <- pheno.treat$TT3 - pheno.treat$TTC
  pheno.treat$warmwet <- pheno.treat$TT4 - pheno.treat$TTC
  pheno.treat[,3:7][ is.na(pheno.treat[,3:7]) ] <- NA # replace NA with 0 if you want to see sp without partner!
  pheno.treat[,8:15][ is.na(pheno.treat[,8:15]) ] <- NA
  pheno.treat[,16:18] <- traits.15[match(pheno.treat$variable, traits.15$species), c(2:4)]
  return(pheno.treat)
}

pheno.treat <- reshape.pheno2(pheno.long, "origin")

### not needed anymore if function works
pheno.mean <- dcast(pheno.long, destSiteID + variable  ~ TTtreat, mean)
pheno.sd <- dcast(pheno.long, destSiteID + variable  ~ TTtreat, sd)
pheno.sd <- pheno.sd[,3:7]
colnames(pheno.sd) <- c("tt1.sd", "tt2.sd", "tt3.sd", "tt4.sd", "ttc.sd")
pheno.treat <- cbind(pheno.mean,pheno.sd)
pheno.treat$warm <- pheno.treat$TT2 - pheno.treat$TTC # check which species are earlier/later
pheno.treat$wet <- pheno.treat$TT3 - pheno.treat$TTC
pheno.treat$warmwet <- pheno.treat$TT4 - pheno.treat$TTC
pheno.treat[,3:7][ is.na(pheno.treat[,3:7]) ] <- NA # replace NA with 0 if you want to see sp without partner!
pheno.treat[,8:15][ is.na(pheno.treat[,8:15]) ] <- NA
pheno.treat[,16:18] <- traits.15[match(pheno.treat$variable, traits.15$species), c(2:4)]

### ORIGIN
pheno.mean <- dcast(pheno.long, siteID + variable  ~ TTtreat, mean)
pheno.sd <- dcast(pheno.long, siteID + variable  ~ TTtreat, sd)
pheno.sd <- pheno.sd[,3:7]
colnames(pheno.sd) <- c("tt1.sd", "tt2.sd", "tt3.sd", "tt4.sd", "ttc.sd")
pheno.treat <- cbind(pheno.mean,pheno.sd)
pheno.treat$warm <- pheno.treat$TT2 - pheno.treat$TTC # check which species are earlier/later
pheno.treat$wet <- pheno.treat$TT3 - pheno.treat$TTC
pheno.treat$warmwet <- pheno.treat$TT4 - pheno.treat$TTC
pheno.treat[,3:7][ is.na(pheno.treat[,3:7]) ] <- NA # replace NA with 0 if you want to see sp without partner!
pheno.treat[,8:15][ is.na(pheno.treat[,8:15]) ] <- NA
pheno.treat[,16:18] <- traits.15[match(pheno.treat$variable, traits.15$species), c(2:4)]





####################
### PLOT GRAPHS
####################

# plot graphs comparing treatment and control
# input = pheno.treat
# choose destination/origin
plot.treat.control <- function(input, control.turf){
  xylim <- range(input$TTC, na.rm=TRUE) ### MIGHT NEED TO CHANGE RANGE OF ALL DATA
  par(mfrow=c(1,3), mar=c(1,1,1.5,0.5), mgp=c(3,0.5,0), oma=c(4,4,1.2,1.2), cex=2)
  plot(input$TTC, input$TT2, main="warmer", xlim=xylim, ylim=xylim, axes=FALSE, pch=16, ylab="", xlab="", col=as.numeric(input$functionalGroup))
  box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
  bars(input$TTC, input$TT2, input$ttc.sd, c="grey")
  bars.y(input$TTC, input$TT2, input$tt2.sd, c="grey")
  abline(1,1, lty=2)
  plot(input$TTC, input$TT3, main="wetter", xlim=xylim, ylim=xylim, axes=FALSE, pch=16, ylab="", xlab="", col=as.numeric(input$functionalGroup))
  box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
  bars(input$TTC, input$TT3, input$ttc.sd, c="grey")
  bars.y(input$TTC, input$TT3, input$tt3.sd, c="grey")
  abline(1,1, lty=2)
  plot(input$TTC, input$TT4, main="warmer & wetter", xlim=xylim, ylim=xylim, axes=FALSE, pch=16, ylab="", xlab="", col=as.numeric(input$functionalGroup))
  box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
  bars(input$TTC, input$TT4, input$ttc.sd, c="grey")
  bars.y(input$TTC, input$TT4, input$tt4.sd, c="grey")
  abline(1,1, lty=2)
  if(control.turf=="destination"){
    mtext("Destination (control)", 1, outer=TRUE, line=2.5, cex=2)
  }
  else if(control.turf=="origin"){
    mtext("Origin (control)", 1, outer=TRUE, line=2.5, cex=2)
  }
  mtext("Transplant (treatment)", 2, outer=TRUE, line=2.5, cex=2)
}
plot.treat.control(pheno.treat, "origin")


input <- pheno.treat
xylim <- range(input$TTC, na.rm=TRUE)
xylim <- range(-5,15)
# graph
pdf ("Fig_Phenology_2015_Fl_peak_dest.pdf", width=13, height=6.5, pointsize=5, onefile=TRUE, paper="special")
par(mfrow=c(1,3), mar=c(1,1,1.5,0.5), mgp=c(3,0.5,0), oma=c(4,4,1.2,1.2), cex=3)
plot(input$TTC, input$TT2, main="warmer", xlim=xylim, ylim=xylim, axes=FALSE, pch=16, ylab="", xlab="", col=as.numeric(input$functionalGroup))
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
bars(input$TTC, input$TT2, input$ttc.sd, c="grey")
bars.y(input$TTC, input$TT2, input$tt2.sd, c="grey")
abline(1,1, lty=2)
legend(2,15, legend=c("Forb", "Graminoid"), col=c(1,2), pch=16, cex=0.7, bty = "n")
plot(input$TTC, input$TT3, main="wetter", xlim=xylim, ylim=xylim, axes=FALSE, pch=16, ylab="", xlab="", col=as.numeric(input$functionalGroup))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
bars(input$TTC, input$TT3, input$ttc.sd, c="grey")
bars.y(input$TTC, input$TT3, input$tt3.sd, c="grey")
abline(1,1, lty=2)
plot(input$TTC, input$TT4, main="warmer & wetter", xlim=xylim, ylim=xylim, axes=FALSE, pch=16, ylab="", xlab="", col=as.numeric(input$functionalGroup))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
bars(input$TTC, input$TT4, input$ttc.sd, c="grey")
bars.y(input$TTC, input$TT4, input$tt4.sd, c="grey")
abline(1,1, lty=2)
mtext("Control (destination)", 1, outer=TRUE, line=2.5, cex=3)
mtext("Transplant", 2, outer=TRUE, line=2.5, cex=3)
dev.off()

pdf ("Fig_Phenology_2015_Empty.pdf", width=13, height=6.5, pointsize=5, onefile=TRUE, paper="special")
par(mfrow=c(1,3), mar=c(1,1,1.5,0.5), mgp=c(3,0.5,0), oma=c(4,4,1.2,1.2), cex=3)
plot(input$TTC, input$TT2, main="warmer", xlim=xylim, ylim=xylim, type="n", xlab="", ylab="", axes=FALSE)
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
abline(1,1, lty=2)
plot(input$TTC, input$TT3, main="wetter", xlim=xylim, ylim=xylim, type="n", xlab="", ylab="", axes=FALSE)
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
abline(1,1, lty=2)
plot(input$TTC, input$TT4, main="warmer & wetter", xlim=xylim, ylim=xylim, type="n", xlab="", ylab="", axes=FALSE)
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
abline(1,1, lty=2)
mtext("Control (destination)", 1, outer=TRUE, line=2.5, cex=3)
mtext("Transplant", 2, outer=TRUE, line=2.5, cex=3)
dev.off()

# calculate some stuff
par(mfrow=c(2,3))
by(pheno.long, pheno.long$destSiteID, function(site){
  #plot(value ~ TTtreat, site, main=unique(site$destSiteID))
  length(site$turfID) # data points
})
# nr of species: 76
unique(pheno.long$variable)






# Testing Models
head(pheno.long)
aggregate(value ~ TTtreat, data = pheno.long, FUN = mean)
hist(log(pheno.long$value))

fit5 <- lmer(log(value+1) ~ destT_level*destP_level + (1|destBlockID) + (1|variable), pheno.long)
fit4 <- lmer(log(value+1) ~ destT_level + destP_level + (1|destBlockID) + (1|variable), pheno.long)
fit3 <- lmer(log(value+1) ~ destT_level + (1|destBlockID) + (1|variable), pheno.long)
fit2 <- lmer(log(value+1) ~ destP_level + (1|destBlockID) + (1|variable), pheno.long)
fit1 <- lmer(log(value+1) ~ 1 + (1|destBlockID) + (1|variable), pheno.long)
modsel(list(fit5, fit4, fit3, fit2, fit1), 1000)
fix.check(fit4)

fit5 <- lmer(value ~ Temperature_level*Precipitation_level + (1|blockID) + (1|variable), pheno.long)
fit4 <- lmer(value ~ Temperature_level+Precipitation_level + (1|blockID) + (1|variable), pheno.long)
fit3 <- lmer(value ~ Temperature_level + (1|blockID) + (1|variable), pheno.long)
fit2 <- lmer(value ~ Precipitation_level + (1|blockID) + (1|variable), pheno.long)
fit1 <- lmer(value ~ 1 + (1|blockID) + (1|variable), pheno.long)
modsel(list(fit5, fit4, fit3, fit2, fit1), 1000)






modsel <- function(mods,x){  
  phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df")
    dd$QAIC[j] = QAICc(mods[[j]],phi)
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}; dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}


QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(ranef(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}


### CHECK MODELS
fix.check <- function(mod){    #function to produce model-checking plots for the fixed effects of an lmer model
  par(mfrow = c(2,2))
  plot(fitted(mod),resid(mod))  #should have no pattern
  abline(h=0)
  print(anova(lm(fitted(mod)~resid(mod))))  #should be non-significant
  qqnorm(resid(mod), ylab="Residuals")		#should be approximately straight line
  qqline(resid(mod))
  plot(density(resid(mod)))					#should be roughly normally distributed
  rug(resid(mod))}

