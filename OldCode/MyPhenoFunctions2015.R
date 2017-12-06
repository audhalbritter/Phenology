### My Phenology Functions 2015



# Read in head of phenology data 2015
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



# Read in phenology data 2015
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



# Calculate the sum of buds, flowers and seeds per turf and species
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



### GET FIRST, PEAK, END AND DURATION OF BUD SET, FLOWERING, SEED SET
# ph = phenology data set; ph.var = peak, first, end or duration; ph.stage = nr.b, br.f, nr.s, nr.r
# ph <- pheno15
phenology.var <- function(ph, ph.var, control.turf, ph.stage){ # for function
  # visualize the data (nr.fl per week)
  par(mfrow=c(4,4))
  pheno.vars <- by(ph, ph$turfID, function(tu){ # loop by turfID
    by(tu, tu$species, function(sp){ # loop by species
      
      ### BUD
      if(ph.stage=="nr.b"){
        if(sum(sp$nr.b, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$week.nr,(sp[,26:29]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(control.turf=="origin"){
            dosm <- turfs.15$o.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          else if(control.turf=="destination"){
            dosm <- turfs.15$d.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$week.nr2))[which.max(sp$nr.b)] # peak flowering (use unique for not to get them 4 times)
            res2 <- res - mean(dosm)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$week.nr2))[which.min(sp$nr.b>0)] # first flowering
            res2 <- res - mean(dosm)
          }else if(ph.var=="end"){
            res<-sort(unique(sp$week.nr2),decreasing=TRUE)[which.min(rev(sp$nr.b>0))] # end of flowering (reverse from first flowering)
            res2 <- res - mean(dosm)
          }else {stop("UNRECOGNISED PH.VAR")}
          
        }
        else return(NA)
      }
      
      ### FLOWERING
      if(ph.stage=="nr.f"){
        if(sum(sp$nr.f, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$week.nr,(sp[,26:29]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(control.turf=="origin"){
            dosm <- turfs.15$o.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          else if(control.turf=="destination"){
            dosm <- turfs.15$d.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$week.nr2))[which.max(sp$nr.f)] # peak flowering (use unique for not to get them 4 times)
            res2 <- res - mean(dosm)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$week.nr2))[which.min(sp$nr.f>0)] # first flowering
            res2 <- res - mean(dosm)
          }else if(ph.var=="end"){
            res<-sort(unique(sp$week.nr2),decreasing=TRUE)[which.min(rev(sp$nr.f>0))] # end of flowering (reverse from first flowering)
            res2 <- res - mean(dosm)
          }else {stop("UNRECOGNISED PH.VAR")}
        
        }
        else return(NA)
      }
      
      ### SEED
      else if(ph.stage=="nr.s"){
        if(sum(sp$nr.s, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$week.nr,(sp[,26:29]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(control.turf=="origin"){
            dosm <- turfs.15$o.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          else if(control.turf=="destination"){
            dosm <- turfs.15$d.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$week.nr2))[which.max(sp$nr.s)] # peak flowering (use unique for not to get them 4 times)
            res2 <- res - mean(dosm)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$week.nr2))[which.min(sp$nr.s>0)] # first flowering
            res2 <- res - mean(dosm)
          }else if(ph.var=="end"){
            res<-sort(unique(sp$week.nr2),decreasing=TRUE)[which.min(rev(sp$nr.s>0))] # end of flowering (reverse from first flowering)
            res2 <- res - mean(dosm)
          }else {stop("UNRECOGNISED PH.VAR")}
        }
        else return(NA)
      }
      
      ### RIPE SEED
      else if(ph.stage=="nr.r"){
        if(sum(sp$nr.r, na.rm=TRUE) > 0){ # if all NA, then zero
          if(FALSE){
            matplot(sp$week.nr,(sp[,26:29]), type="l", main=unique(paste(sp$turfID, sp$species, sep="-")), col=c("green", "red", "blue", "black"), lty=1)
          }
          if(control.turf=="origin"){
            dosm <- turfs.15$o.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          else if(control.turf=="destination"){
            dosm <- turfs.15$d.wsm15.wnr2[match(sp$turfID, turfs.15$turfID)]
          }
          if(ph.var=="peak"){
            res<-sort(unique(sp$week.nr2))[which.max(sp$nr.r)] # peak flowering (use unique for not to get them 4 times)
            res2 <- res - mean(dosm)
          }else if(ph.var=="first"){
            res<-sort(unique(sp$week.nr2))[which.min(sp$nr.r>0)] # first flowering
            res2 <- res - mean(dosm)
          }else if(ph.var=="end"){
            res<-sort(unique(sp$week.nr2),decreasing=TRUE)[which.min(rev(sp$nr.r>0))] # end of flowering (reverse from first flowering)
            res2 <- res - mean(dosm)
          }else {stop("UNRECOGNISED PH.VAR")}
        }
        else return(NA)
      }
      
    })     
  })
  pheno.vars2<-pheno.vars[sapply(pheno.vars,length)>0] # gets rid of turfs with no flowers
  pheno.vars2<-t(sapply(pheno.vars2, I))
  new.dat <- as.data.frame(pheno.vars2)
  new.dat[,144:157] <- turfs.15[match(rownames(new.dat), turfs.15$turfID), c("siteID", "TTtreat", "Year", "blockID", "turfID", "Temperature_level", "Precipitation_level", "destBlockID", "destSiteID", "newTT", "o.wsm15.wnr2", "d.wsm15.wnr2", "destT_level", "destP_level")] # add turf info
  #new.dat[,144:157] <- turfs.15[match(rownames(new.dat), turfs.15$turfID), c(1:8,11,12,16,19,21:22)]
  return(new.dat)
}

# duration is special case: don't calc in days since sm, does not make sense!
#}else if(ph.var=="duration"){
#  end<-sort(unique(sp$week.nr),decreasing=TRUE)[which.min(rev(sp$nr.f>0))]
#  first<-sort(unique(sp$week.nr))[which.min(sp$nr.f>0)]
#  res<-end - (first-1) # duration of flowering




########################################################
### for testing function
ddd <- subset(ph, ph$turfID=="104 TT1 102")
ddd <- subset(ddd, ddd$species=="Pot.ere")
res<-sort(unique(ddd$week.nr))[which.min(ddd$nr.f>0)] # find first flowering
res<-sort(unique(ddd$week.nr))[which.max(ddd$nr.f)] # peak
res<-sort(unique(ddd$week.nr),decreasing=TRUE)[which.min(rev(ddd$nr.f>0))] # end
dosm <- turfs.15$sm.15.w.nr[match(ddd$turfID, turfs.15$turfID)]
res - dosm
########################################################




# Reshape phenology data: long species list
# input phenology.data
reshape.pheno1 <- function(dd){
  ### Wide to long data set (species in column)
  pheno.data <- dd[,c(148,1:143)] # select turfID and species
  pheno.long <- melt(pheno.data, id=c("turfID"))
  pheno.long <- na.omit(pheno.long) # remove all species with NA
  #colnames(pheno.long) <- c("turfID", "species", "ph.event")
  pheno.long[,4:21] <- turfs.15[match(pheno.long$turfID, turfs.15$turfID), c("siteID", "TTtreat", "Year", "blockID", "turfID", "Temperature_level", "Precipitation_level", "destBlockID", "destSiteID", "newTT", "o.wsm15.wnr2", "d.wsm15.wnr2", "destT_level", "destP_level", "Temp_value", "Prec_value", "destTemp_value", "destPrec_value")]
  pheno.long[,22:25] <- traits.15[match(pheno.long$variable, traits.15$species), c("family", "functionalGroup", "lifeSpan", "flowering.time")]
  return(pheno.long)  
}


# Reshape phenology data: for graphs
# input pheno.long
# control: only uses TTC
reshape.pheno2 <- function(dd, control.turf){
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

# control: uses mean of TTC and TT1
reshape.pheno2.2 <- function(dd, control.turf){
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


reshape.pheno2.GDD <- function(dd, control.turf){
  # Long to wide data set again (TTtreat next to each other)
  #dd2 <- dd[,c("variable", "destSiteID", "newTT", "GDD")]
  if(control.turf=="destination"){
    dd2 <- dd[,c("variable", "destSiteID", "newTT", "GDD")]
    pheno.mean <- dcast(dd2, destSiteID + variable  ~ newTT, mean)
    pheno.sd <- dcast(dd2, destSiteID + variable  ~ newTT, sd)
  }
  else if(control.turf=="origin"){
    dd2 <- dd[,c("variable", "siteID", "newTT", "GDD")]
    pheno.mean <- dcast(dd2, siteID + variable  ~ newTT, mean)
    pheno.sd <- dcast(dd2, siteID + variable  ~ newTT, sd)
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






### PLOT ERROR BARS
# x and y are the location of the points, z is the standard error, c the colour
bars <- function(x,y,z,c){for (k in 1:length(y)) for (i in c(-1, 1)) arrows(x[k], y[k], x[k], y[k]+i*z[k], angle=90, length=0, col=c)}
bars.y <- function(x,y,z,c){for (l in 1:length(x)) for (j in c(-1, 1)) arrows(x[l], y[l], x[l]+j*z[l], y[l], angle=90, length=0, col=c)}

# Plot graphs comparing treatment and control
# data = pheno.treat
# choose destination/origin and bud/flower/seed
dd<-pheno.treat
plot.treat.control <- function(dd, control.turf, filename){
  input <- dd
  xxlim <- range(input$control, na.rm=TRUE)
  yylim <- range(cbind(input$"TT2", input$"TT3", input$"TT4"), na.rm=TRUE)
  pdf (paste(filename, ".pdf", sep=""), width=13, height=6.5, pointsize=7)
  par(mfrow=c(1,3), mar=c(1,1,1.5,0.5), mgp=c(1,1,0), oma=c(3,3,1,0.5), cex=3)
    if(control.turf=="destination"){
      plot(input$control, input$TT2, main="warmer", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
      box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
      bars(input$control, input$TT2, input$control.sd, c="grey")
      bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
      abline(1,1, lty=2)
      legend(1.2,14, legend=c("wet", "intermediate", "dry"), col=c("darkblue", "blue", "lightblue"), pch=16, cex=0.8, bty = "n")
      plot(input$control, input$TT3, main="wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
      box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
      bars(input$control, input$TT3, input$control.sd, c="grey")
      bars.y(input$control, input$TT3, input$tt3.sd, c="grey")
      abline(1,1, lty=2)
      legend(1.2,14, legend=c("alpine", "intermediate"), col=c("darkblue"), pch=c(17,16), cex=0.8, bty = "n")
      plot(input$control, input$TT4, main="warmer & wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
      box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
      bars(input$control, input$TT4, input$control.sd, c="grey")
      bars.y(input$control, input$TT4, input$tt4.sd, c="grey")
      abline(1,1, lty=2)
      mtext("Destination (control)", 1, outer=TRUE, line=1.3, cex=3)
      mtext("Transplant", 2, outer=TRUE, line=1.3, cex=3)
      
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
    }
  dev.off()
}


disp <- function(mod,data){
  rdev <- sum(residuals(mod)^2)
  mdf <- length(fixef(mod))
  rdf <- nrow(data)-mdf
  rdev/rdf}

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
