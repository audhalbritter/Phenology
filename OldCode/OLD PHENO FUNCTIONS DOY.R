#### OLD DOY CODE
#### GET FIRST, PEAK, END AND DURATION OF BUD SET, FLOWERING, SEED SET IN DOY ####
# ph = phenology data set; ph.var = peak, first, end or duration;
# ph.stage = nr.b, br.f, nr.s, nr.r
# ph <- pheno15
GetPhenologyVar <- function(ph, ph.var, ph.stage){ # for function
  # visualize the data (nr.fl per week)
  par(mfrow=c(4,4))
  # remove rows were doy is NA
  pheno.vars <- by(ph, ph$turfID, function(tu){ # loop by turfID
    by(tu, tu$species, function(sp){ # loop by species
      
      ### BUD
      if(ph.stage=="nr.b"){
        if(sum(sp$nr.b, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$doy,(sp[,28:31]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$doy))[which.max(sp$nr.f)] # peak flowering (use unique for not to get them 4 times)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$doy))[which.min(sp$nr.f>0)] # first flowering
          }else if(ph.var=="end"){
            res<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] # end of flowering (reverse from first flowering)
          }else if(ph.var=="duration"){
            first<-sort(unique(sp$doy))[which.min(sp$nr.f>0)]
            end<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))]
            res <- end - (first-1)
          }else {stop("UNRECOGNISED PH.VAR")}
          
        }
        else return(NA)
      }
      
      ### FLOWERING
      if(ph.stage=="nr.f"){
        if(sum(sp$nr.f, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$doy,(sp[,28:31]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$doy))[which.max(sp$nr.f)] # peak flowering (use unique for not to get them 4 times)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$doy))[which.min(sp$nr.f>0)] # first flowering
          }else if(ph.var=="end"){
            res<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] # end of flowering (reverse from first flowering)
          }else if(ph.var=="duration"){
            res <- sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] - (sort(unique(sp$doy))[which.min(sp$nr.f>0)] -1)
          }else {stop("UNRECOGNISED PH.VAR")}
          
        }
        else return(NA)
      }
      
      ### SEED
      else if(ph.stage=="nr.s"){
        if(sum(sp$nr.s, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$doy,(sp[,28:31]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$doy))[which.max(sp$nr.f)] # peak flowering (use unique for not to get them 4 times)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$doy))[which.min(sp$nr.f>0)] # first flowering
          }else if(ph.var=="end"){
            res<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] # end of flowering (reverse from first flowering)
          }else if(ph.var=="duration"){
            first<-sort(unique(sp$doy))[which.min(sp$nr.f>0)]
            end<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))]
            res <- end - (first-1)
          }else {stop("UNRECOGNISED PH.VAR")}
          
        }
        else return(NA)
      }
      
      ### RIPE SEED
      else if(ph.stage=="nr.r"){
        if(sum(sp$nr.r, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$doy,(sp[,28:31]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$doy))[which.max(sp$nr.f)] # peak flowering (use unique for not to get them 4 times)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$doy))[which.min(sp$nr.f>0)] # first flowering
          }else if(ph.var=="end"){
            res<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] # end of flowering (reverse from first flowering)
          }else if(ph.var=="duration"){
            first<-sort(unique(sp$doy))[which.min(sp$nr.f>0)]
            end<-sort(unique(sp$doy),decreasing=TRUE)[which.min(rev(sp$nr.f>0))]
            res <- end - (first-1)
          }else {stop("UNRECOGNISED PH.VAR")}
          
        }
        else return(NA)
      }
      
    })     
  })
  pheno.vars2<-pheno.vars[sapply(pheno.vars,length)>0] # gets rid of turfs with no flowers
  pheno.vars2<-t(sapply(pheno.vars2, I))
  new.dat <- as.data.frame(pheno.vars2)
  new.dat[,144:159] <- turfs.15[match(rownames(new.dat), turfs.15$turfID), c("siteID", "TTtreat", "Year", "blockID", "turfID", "Temperature_level", "Precipitation_level", "destBlockID", "destSiteID", "newTT", "o.wsm15.wnr2", "d.wsm15.wnr2", "destT_level", "destP_level", "d.dosm", "o.dosm")] # add turf info
  #new.dat[,144:157] <- turfs.15[match(rownames(new.dat), turfs.15$turfID), c(1:8,11,12,16,19,21:22)]
  return(new.dat)
}



#### REPLACE DOY WITH DAYS AFTER SNOWMELT OR DEGREEDAYS ####
# choose origin or destinaton control.turf
# dat = pheno.variables
# var = snowmelt or degreedays
# YEAR = 2015 by default
ReplaceDayOfYear <- function(control.turf, dat, var, YEAR = 2015){
  ### ORIGIN
  if(control.turf=="origin"){
    # Days after snowmelt
    if(var == "snowmelt"){
      res <- sapply(dat[,1:143], function(x) {
        x - turfs.15$o.dosm[match(dat$turfID, turfs.15$turfID)]
      })
    }
    # Degree days after swnomelt
    else if(var == "degreedays"){
      climate <- daily.temp[daily.temp$year == 2015,]
      res <- sapply(dat[,1:143], function(x) {
        t1 <- climate$CumTemperature[match(x, climate$doy)] # CumTemp until FLOWERING
        dosm <- turfs.15$o.dosm[match(dat$turfID, turfs.15$turfID)]
        t2 <- climate$CumTemperature[match(dosm, climate$doy)] # CumTemp until SNOWMELT
        res <- t1 - t2 # Degree days
      })
    }
    else {stop("UNRECOGNISED VAR")}
    
  }
  ### DESTINATION
  else if(control.turf=="destination"){
    # Days after snowmelt
    if(var == "snowmelt"){
      res <- sapply(dat[,1:143], function(x) {
        x - turfs.15$d.dosm[match(dat$turfID, turfs.15$turfID)]
      })
    }
    # Degree days after swnomelt
    else if(var == "degreedays"){
      climate <- daily.temp[daily.temp$year == YEAR,]
      res <- sapply(dat[,1:143], function(x) {
        t1 <- climate$CumTemperature[match(x, climate$doy)] # CumTemp until FLOWERING
        dosm <- turfs.15$d.dosm[match(dat$turfID, turfs.15$turfID)]
        t2 <- climate$CumTemperature[match(dosm, climate$doy)] # CumTemp until SNOWMELT
        res <- t1 - t2 # Degree days
      })
    }
    else {stop("UNRECOGNISED VAR")}
    
  }
  res2 <- cbind(res, dat[144:ncol(dat)])
  return(res2)
}


#### RESPAHE PHENOLOGY DATA: WIDE TO LONG ####
# used for analysis
# dd pheno.variables
ReshapePhenoWideToLong <- function(dd){
  ### Wide to long data set (species in column)
  pheno.data <- dd[,c(148,1:143)] # select turfID and species
  pheno.long <- melt(pheno.data, id=c("turfID"))
  pheno.long <- na.omit(pheno.long) # remove all species with NA
  #colnames(pheno.long) <- c("turfID", "species", "ph.event")
  pheno.long[,4:21] <- turfs.15[match(pheno.long$turfID, turfs.15$turfID), c("siteID", "TTtreat", "Year", "blockID", "turfID", "Temperature_level", "Precipitation_level", "destBlockID", "destSiteID", "newTT", "o.wsm15.wnr2", "d.wsm15.wnr2", "destT_level", "destP_level", "Temp_value", "Prec_value", "destTemp_value", "destPrec_value")]
  pheno.long[,22:26] <- traits.15[match(pheno.long$variable, traits.15$species), c("family", "functionalGroup", "lifeSpan", "flowering.time", "occurrence.2")]
  return(pheno.long)  
}


#### GET DATA ####
# dd = pheno15
# ph.stage = nr.b, br.f, nr.s, nr.r
# pheno.var = peak, first, end or duration
# c.turf = destination or origin
# unit = doy, sm or temp
GetData <- function(dd, pheno.stage, pheno.var, c.turf, unit){
  # first/peak... of flower/bud/seed
  pheno.variables <- GetPhenologyVar(dd, pheno.stage, pheno.var)
  
  # get right unit: doy, sm or temp
  if(unit == "doy"){
    pheno.variables2 <- pheno.variables
  }
  else if(unit == "snowmelt"){
    pheno.variables2 <- ReplaceDayOfYear(c.turf, pheno.variables, "snowmelt")
  }
  else if(unit == "degreedays"){
    pheno.variables2 <- ReplaceDayOfYear(c.turf, pheno.variables, "degreedays")
  }
  
  # reshape wide to long
  pheno.long <- ReshapePhenoWideToLong(pheno.variables2)
  pheno.long$var <- pheno.var
  pheno.long$stage <- pheno.stage
  pheno.long$unit <- unit
  return(pheno.long)
}



#### RESPAHE PHENOLOGY DATA: GRAPHS TCC ####
# dd: pheno.long
# control.turf: origin or destination
# only uses TTC
ReshapePhenoGraphsTTC <- function(dd, control.turf){
  # Long to wide data set again (TTtreat next to each other)
  if(control.turf=="destination"){
    pheno.mean <- dcast(dd, destSiteID + variable  ~ TTtreat, mean)
    pheno.sd <- dcast(dd, destSiteID + variable  ~ TTtreat, sd)
  }
  else if(control.turf=="origin"){
    pheno.mean <- dcast(dd, siteID + variable  ~ TTtreat, mean)
    pheno.sd <- dcast(dd, siteID + variable  ~ TTtreat, sd)
  }
  pheno.sd <- pheno.sd[,3:7]
  colnames(pheno.sd) <- c("tt1.sd", "tt2.sd", "tt3.sd", "tt4.sd", "ttc.sd")
  pheno.treat <- cbind(pheno.mean,pheno.sd)
  pheno.treat$warm <- pheno.treat$TT2 - pheno.treat$TTC # check which species are earlier/later
  pheno.treat$wet <- pheno.treat$TT3 - pheno.treat$TTC
  pheno.treat$warmwet <- pheno.treat$TT4 - pheno.treat$TTC
  pheno.treat[,3:7][ is.na(pheno.treat[,3:7]) ] <- NA # replace NA with 0 if you want to see sp without partner!
  pheno.treat[,8:15][ is.na(pheno.treat[,8:15]) ] <- NA
  pheno.treat[,16:19] <- traits.15[match(pheno.treat$variable, traits.15$species), c("family", "functionalGroup", "lifeSpan", "flowering.time")]
  if(control.turf=="destination"){
    pheno.treat[,20:21] <- turfs.15[match(pheno.treat$destSiteID, turfs.15$destSiteID), c(19:20)]
  }
  else if(control.turf=="origin"){
    pheno.treat[,20:21] <- turfs.15[match(pheno.treat$siteID, turfs.15$siteID), c(6:7)]
  }
  pheno.treat$control <- pheno.treat$TTC
  pheno.treat$control.sd <- pheno.treat$ttc.sd
  return(pheno.treat)  
}

#### RESPAHE PHENOLOGY DATA: GRAPHS newTT ####
# dd: pheno.long
# control.turf: origin or destination
# uses TTC and TT1
ReshapePhenoGraphsnewTT <- function(dd, control.turf){
  # Long to wide data set again (TTtreat next to each other)
  if(control.turf=="destination"){
    pheno.mean <- dcast(dd, destSiteID + variable  ~ newTT, mean)
    pheno.sd <- dcast(dd, destSiteID + variable  ~ newTT, sd)
  }
  else if(control.turf=="origin"){
    pheno.mean <- dcast(dd, siteID + variable  ~ newTT, mean)
    pheno.sd <- dcast(dd, siteID + variable  ~ newTT, sd)
  }
  pheno.sd <- pheno.sd[,3:6]
  colnames(pheno.sd) <- c("control.sd", "tt2.sd", "tt3.sd", "tt4.sd")
  pheno.treat <- cbind(pheno.mean,pheno.sd)
  pheno.treat$warm <- pheno.treat$TT2 - pheno.treat$control # check which species are earlier/later
  pheno.treat$wet <- pheno.treat$TT3 - pheno.treat$control
  pheno.treat$warmwet <- pheno.treat$TT4 - pheno.treat$control
  pheno.treat[,3:6][ is.na(pheno.treat[,3:6]) ] <- NA # replace NA with 0 if you want to see sp without partner!
  pheno.treat[,7:13][ is.na(pheno.treat[,7:13]) ] <- NA
  pheno.treat[,14:17] <- traits.15[match(pheno.treat$variable, traits.15$species), c("family", "functionalGroup", "lifeSpan", "flowering.time")]
  if(control.turf=="destination"){
    pheno.treat[,18:19] <- turfs.15[match(pheno.treat$destSiteID, turfs.15$destSiteID), c(21:22)]
  }
  else if(control.turf=="origin"){
    pheno.treat[,18:19] <- turfs.15[match(pheno.treat$siteID, turfs.15$siteID), c(6:7)]
  }
  return(pheno.treat)  
}



MakeFigure <- function(dd, c.turf, TT, filename, toptitle){
  # Reshape phenology data: for graphs
  if(TT == "TTC"){
    pheno.treat <- ReshapePhenoGraphsTTC(dd, c.turf)
  }
  if(TT == "newTT"){
    pheno.treat <- ReshapePhenoGraphsnewTT(dd, c.turf)
  }
  pheno.treat
  # Plot graphs
  plot.treat.control(pheno.treat, c.turf, filename, toptitle)
}




#### PLOT PHENOLOGY GRAPHS COMPARING TREATMENT AND CONTROL ####
# dd = pheno.treat
# control.turf = destination or origin
# filename = name of the pdf FFl_DOY; FFl_SM, FFl_DD
plot.treat.control <- function(dd, control.turf, filename, toptitle){
  input <- dd
  xxlim <- range(input$control, na.rm=TRUE)
  yylim <- range(cbind(input$"TT2", input$"TT3", input$"TT4"), na.rm=TRUE)
  pdf (paste(filename, ".pdf", sep=""), width=13, height=6.5, pointsize=7)
  par(mfrow=c(1,3), mar=c(1,1,1,0.5), mgp=c(1,1,0), oma=c(3,3,3,0.5), cex=3)
  if(control.turf=="destination"){
    plot(input$control, input$TT2, main="warmer", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
    box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
    bars(input$control, input$TT2, input$control.sd, c="grey")
    bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
    abline(1,1, lty=2)
    legend(min(xxlim)*1.03,max(yylim)*0.97, legend=c("wet", "intermediate", "dry"), col=c("darkblue", "blue", "lightblue"), pch=16, cex=0.8, bty = "n")
    plot(input$control, input$TT3, main="wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
    box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
    bars(input$control, input$TT3, input$control.sd, c="grey")
    bars.y(input$control, input$TT3, input$tt3.sd, c="grey")
    abline(1,1, lty=2)
    legend(min(xxlim)*1.03,max(yylim)*0.97, legend=c("alpine", "intermediate"), col=c("darkblue"), pch=c(17,16), cex=0.8, bty = "n")
    plot(input$control, input$TT4, main="warmer & wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
    box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
    bars(input$control, input$TT4, input$control.sd, c="grey")
    bars.y(input$control, input$TT4, input$tt4.sd, c="grey")
    abline(1,1, lty=2)
    mtext("Destination (control)", 1, outer=TRUE, line=1.3, cex=3)
    mtext("Transplant", 2, outer=TRUE, line=1.3, cex=3)
    mtext(toptitle, 3, outer=TRUE, line=1.3, cex=3)
    
  }
  else if(control.turf=="origin"){
    plot(input$control, input$TT2, main="warmer", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$Temperature_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$Precipitation_level<=2, "lightblue", ifelse(input$Precipitation_level>=4, "darkblue", "blue")))
    box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
    bars(input$control, input$TT2, input$control.sd, c="grey")
    bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
    abline(1,1, lty=2)
    legend(1.2,14, legend=c("wet", "intermediate", "dry"), col=c("darkblue", "blue", "lightblue"), pch=16, cex=0.8, bty = "n")
    plot(input$control, input$TT3, main="wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$Temperature_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$Precipitation_level<=2, "lightblue", ifelse(input$Precipitation_level>=4, "darkblue", "blue")))
    box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
    bars(input$control, input$TT3, input$control.sd, c="grey")
    bars.y(input$control, input$TT3, input$tt3.sd, c="grey")
    abline(1,1, lty=2)
    legend(1.2,14, legend=c("alpine", "intermediate"), col=c("darkblue"), pch=c(17,16), cex=0.8, bty = "n")
    plot(input$control, input$TT4, main="warmer & wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$Temperature_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$Precipitation_level<=2, "lightblue", ifelse(input$Precipitation_level>=4, "darkblue", "blue")))
    box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
    bars(input$control, input$TT4, input$control.sd, c="grey")
    bars.y(input$control, input$TT4, input$tt4.sd, c="grey")
    abline(1,1, lty=2)
    mtext("Origin (control)", 1, outer=TRUE, line=1.3, cex=3)
    mtext("Transplant", 2, outer=TRUE, line=1.3, cex=3)
    mtext(toptitle, 3, outer=TRUE, line=1.3, cex=3)
  }
  dev.off()
}





#### GET DATA AND RBIND ####
# pheno15 contains NA in doy, because did not visit every site every week. Needs to be removed or end and duraiton are calculated wrong. But needs to be there when calculated in nr of weeks

# FLOWERING
pheno.long1 <- GetData(pheno15[!is.na(pheno15$doy),], "first", "nr.f", "destination", "doy")
pheno.long2 <- GetData(pheno15[!is.na(pheno15$doy),], "peak", "nr.f", "destination", "doy")
pheno.long3 <- GetData(pheno15[!is.na(pheno15$doy),], "end", "nr.f", "destination", "doy")
pheno.long4 <- GetData(pheno15[!is.na(pheno15$doy),], "duration", "nr.f", "destination", "days")
pheno.long5 <- GetData(pheno15[!is.na(pheno15$doy),], "first", "nr.f", "destination", "snowmelt")
pheno.long6 <- GetData(pheno15[!is.na(pheno15$doy),], "peak", "nr.f", "destination", "snowmelt")
pheno.long7 <- GetData(pheno15[!is.na(pheno15$doy),], "end", "nr.f", "destination", "snowmelt")
pheno.long8 <- GetData(pheno15[!is.na(pheno15$doy),], "first", "nr.s", "destination", "doy")
pheno.all <- rbind(pheno.long1, pheno.long2, pheno.long3, pheno.long4, pheno.long5, pheno.long6, pheno.long7, pheno.long8)
head(pheno.long.f)
#rm(pheno.long1, pheno.long2, pheno.long3, pheno.long4, pheno.long5, pheno.long6, pheno.long7, pheno.long8)


pheno.variables2 <- GetPhenologyVar(pheno15, "nr.f", "peak")
ReshapePhenoWideToLong(pheno.variables2)

pheno.long1 <- GetData(pheno15[!is.na(pheno15$doy),], "duration", "nr.s", "destination", "doy")
head(pheno.long1)
# Make Figures
MakeFigure(pheno.long1, "destination", "newTT", "FFl_Dest", "First flowering in day of year")
head(pheno.treat)


pheno.variables <- GetPhenologyVar(pheno15[!is.na(pheno15$doy),], "duration", "nr.f")
pheno.variables2 <- ReplaceDayOfYear("destination", pheno.variables, "snowmelt")
pheno.long <- ReshapePhenoWideToLong(pheno.variables2)

# test first, end,...
ddd <- pheno15 %>%
  filter(turfID== "101 TTC", species== "Ant.odo")


res<-sort(unique(ddd$doy),decreasing=TRUE)[which.min(rev(ddd$nr.f>0))]


pheno.long %>%
  filter(turfID== "101 TTC", variable== "Ach.mil")


daily.temp %>%
  filter(year == 2015, site == "Hogsete") %>%
  filter(doy == 108)

turfs.15 %>%
  filter(turfID== "101 TTC")

doy fl: 244
doy sm: 108
CumT fl: 192
CumT sm:56
first: 136


### Test difference between TT1 and TTC
pheno.treat2 <- reshape.pheno2(pheno.long, "destination") # need to take function 2
par(mfrow=c(1,1))
plot(pheno.treat2$TTC, pheno.treat2$TT1, col=as.numeric(pheno.treat2$destSiteID), pch=16)
abline(1,1)
# Test difference between TTC and TT1 with subset of the data
mod01 <- lm(value ~ TTtreat, pheno.long[pheno.long$TTtreat=="TTC" | pheno.long$TTtreat=="TT1",])
summary(mod01)
library(multcomp)
summary(glht(mod01, mcp(TTtreat="Tukey")))

# Mixed Model
mod01 <- lmer(value ~ TTtreat + (1|variable) + (1|siteID), data = pheno.long[pheno.long$TTtreat=="TTC" | pheno.long$TTtreat=="TT1",])
mod02 <- lmer(value ~ 1 + (1|variable) + (1|siteID), data = pheno.long[pheno.long$TTtreat=="TTC" | pheno.long$TTtreat=="TT1",])
modsel(list(mod01, mod02), 1000)
# there is no difference between TT1 and TTC



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
  plot(value ~ newTT, site, main=unique(site$destSiteID))
  length(site$turfID) # data points
})
# nr of species: 76
unique(pheno.long$variable)



pheno.treat[,19:20] <- traits.15[match(pheno.treat$variable, traits.15$species), c(10,20)]
input <- pheno.treat
xxlim <- range(input$control, na.rm=TRUE)
yylim <- range(cbind(input$"TT2", input$"TT3", input$"TT4"), na.rm=TRUE)
plot(input$control, input$TT4, main="varmere", xlim=xxlim, ylim=yylim, axes=FALSE, pch=as.numeric(pheno.treat$occurrence.2), ylab="", xlab="", col="darkblue")
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
bars(input$control, input$TT2, input$control.sd, c="grey")
bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
abline(1,1, lty=2)



