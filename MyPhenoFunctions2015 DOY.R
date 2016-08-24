######################################################
############  MY PHENOLOGY FUNCTIONS 2015 ############
######################################################



#### READ IN HEAD OF PHENOLOGY DATA 2015 ####
ReadInHeadPhenology15 <- function(datasheet, site){
  # import head of data set
  dat.h <- read.csv(datasheet, sep=";", header=FALSE, nrow=3, stringsAsFactors=FALSE)
  dat.h2 <- do.call(rbind,
                    lapply(seq(3,ncol(dat.h),20),function(i){
                      x <- dat.h[ ,c(i)]
                      names(x) <- c("date", "weather", "name")
                      x <- c(x,week=dat.h[ 1,i+18])
                      x <- c(x,Site=site)
                      x <- c(x,doy=yday(dmy(dat.h[ 1,i])))
                      x
                    })
  )
  return(dat.h2)
}


#### READ IN PHENOLOGY DATA 2015 ####
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
  long.table <- lapply(seq(3,ncol(dat)-19,20),function(i){
    x <- dat[ ,c(1:2,i:(i+19))]
    names(x) <- c("turfID", "species", paste(rep(c("v", "b", "f", "s", "r"), 4  ), rep(1:4, each=5), sep="."))
    x$week<-dat.h[1,i+18]
    x$doy <- yday(dmy(dat.h[1,i]))
    x  
  })
  dat.long <- do.call(rbind,c(long.table,stingsAsFactors=FALSE))
  
  # Extract turfID
  dat.long$turfID <- sapply(strsplit(dat.long$turfID, split=" - ", fixed=TRUE), function(x) (x[2]                                                                                       
  ))
  dat.long$Site <- site
  # convert to factor and numeric
  #sapply(dat.long[,c(4:7,9:12,14:17,19:22)],function(x)print(grep("\\D", x = x, value = TRUE))) # Check error messages
  dat.long <- cbind(dat.long[,c(1:3,8,13,18,23:25)],sapply(dat.long[,c(4:7,9:12,14:17,19:22)],as.numeric))
  #dat.long$turfID <- as.factor(dat.long$turfID)
  dat.long$species <- as.factor(dat.long$species)
  #dat.long$week <- as.factor(dat.long$week)
  #dat.long$Site <- as.factor(dat.long$Site)
  dat.long
  return(dat.long)
}


#### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEEDS PER TURFID AND SPECIES ####
CalcSums <- function(dat){
  dat$nr.b <- apply(dat[,c("b.1", "b.2", "b.3", "b.4")],1,sum, na.rm=TRUE)
  dat$nr.b[dat$nr.b == 0] <- NA
  dat$nr.f <- apply(dat[,c("f.1", "f.2", "f.3", "f.4")],1,sum, na.rm=TRUE)
  dat$nr.f[dat$nr.f == 0] <- NA
  dat$nr.s <- apply(dat[,c("s.1", "s.2", "s.3", "s.4")],1,sum, na.rm=TRUE)
  dat$nr.s[dat$nr.s == 0] <- NA
  dat$nr.r <- apply(dat[,c("r.1", "r.2", "r.3", "r.4")],1,sum, na.rm=TRUE)
  dat$nr.r[dat$nr.r == 0] <- NA
  return(dat)
}


# make plots
pheno15 %>%
  filter(turfID == "21 TT4 123") %>%
  filter(nr.f != is.na(nr.f)) %>%
  group_by(species, doy) %>%
  ggplot(aes(y = nr.f, x = doy, color = species)) +
    geom_line() +
    facet_wrap(~species)





#### PLOTTING ####


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#### FIGURES ####

#### FUNCITON TO RESHAPE FOR FIGURES ####
# Make separate columns for each treatment
# used for graphs
# data: pheno.long
# control.truf: destination or origin
# controls: C = TTC or C1 = TTC and TT1
ReshapeToMakeFigure <- function(dd, control.turf, controls){
  if(control.turf == "destination"){
    if(controls == "C"){
      dd2 <- dd %>%
        select(destSiteID, species, pheno.stage, pheno.var, pheno.unit, TTtreat, value, turfID) %>% 
        group_by(destSiteID, species, pheno.stage, pheno.var, pheno.unit, TTtreat) %>%
        summarise(mean = mean(value), sd = sd(value)) %>%
        gather(key = var, value = value, -destSiteID, -species, -pheno.stage, -pheno.var, -pheno.unit, -TTtreat) %>% 
        unite(new.var, TTtreat, var) %>% 
        spread(key = new.var, value = value)
      names(dd2) <- c("destSiteID", "species", "pheno.stage", "pheno.var", "pheno.unit", "TT1", "TT1_sd", "TT2", "TT2_sd", "TT3", "TT3_sd", "TT4", "TT4_sd", "TTC", "TTC_sd")
    }
    else if(controls == "C1"){
      dd2 <- dd %>%
        select(destSiteID, species, pheno.stage, pheno.var, pheno.unit, TTtreat, value, turfID, newTT) %>% #select colums
        group_by(destSiteID, species, pheno.stage, pheno.var, pheno.unit, newTT) %>% # group
        summarise(mean = mean(value), sd = sd(value)) %>% # calculate mean and sd for each site and sp
        gather(key = var, value = value, -destSiteID, -species, -pheno.stage, -pheno.var, -pheno.unit, -newTT) %>% # merge mean and sd to one columd
        unite(new.var, newTT, var) %>% # merge treatment and mean/sd
        spread(key = new.var, value = value) # spread mean and sd for each treatment
      names(dd2) <- c("destSiteID", "species", "pheno.stage", "pheno.var", "pheno.unit", "control", "control_sd", "TT2", "TT2_sd", "TT3", "TT3_sd", "TT4", "TT4_sd")  
      
      dd3 <- dd2 %>% 
        gather(key = newTT, value = value, -destSiteID,  -species, -pheno.stage, -pheno.var, -pheno.unit, -control, -control_sd, -TT2_sd, -TT3_sd, -TT4_sd) # devide control and treamtents
      
      dd4 <- dd3 %>% 
        gather(key = newTT_sd, value = sdev, -destSiteID,  -species, -pheno.stage, -pheno.var, -pheno.unit, -control, -control_sd, -newTT, -value) # same for sd
      
      dd4[,(ncol(dd4)+1):(ncol(dd4)+2)] <- turfs.15[match(dd4$destSiteID, turfs.15$destSiteID), c("destT_level", "destP_level")] 
      dd4[,(ncol(dd4)+1):(ncol(dd4)+3)] <- traits.15[match(dd4$species, traits.15$species), c("functionalGroup", "flowering.time", "occurrence.2")]
    }

  }
  else if(control.turf == "origin"){
    if(controls == "C"){
      dd2 <- dd %>%
        select(siteID, species, pheno.stage, pheno.var, pheno.unit, TTtreat, value, turfID) %>% 
        group_by(siteID, species, pheno.stage, pheno.var, pheno.unit, TTtreat) %>%
        summarise(mean = mean(value), sd = sd(value)) %>%
        gather(key = var, value = value, -siteID, -species, -pheno.stage, -pheno.var, -pheno.unit, -TTtreat) %>% 
        unite(new.var, TTtreat, var) %>% 
        spread(key = new.var, value = value)
      names(dd2) <- c("siteID", "species", "pheno.stage", "pheno.var", "pheno.unit", "TT1", "TT1_sd", "TT2", "TT2_sd", "TT3", "TT3_sd", "TT4", "TT4_sd", "TTC", "TTC_sd")
    }
    else if(controls == "C1"){
      dd2 <- dd %>%
        select(siteID, species, pheno.stage, pheno.var, pheno.unit, value, turfID, newTT) %>% 
        group_by(siteID, species, pheno.stage, pheno.var, pheno.unit, newTT) %>%
        summarise(mean = mean(value), sd = sd(value)) %>%
        gather(key = var, value = value, -siteID, -species, -pheno.stage, -pheno.var, -pheno.unit, -newTT) %>% 
        unite(new.var, newTT, var) %>% 
        spread(key = new.var, value = value)
      names(dd2) <- c("siteID", "species", "pheno.stage", "pheno.var", "pheno.unit", "control", "control_sd", "TT2", "TT2_sd", "TT3", "TT3_sd", "TT4", "TT4_sd")
      
      dd3 <- dd2 %>% 
        gather(key = newTT, value = value, -siteID,  -species, -pheno.stage, -pheno.var, -pheno.unit, -control, -control_sd, -TT2_sd, -TT3_sd, -TT4_sd) # devide control and treamtents
      
      dd4 <- dd3 %>% 
        gather(key = newTT_sd, value = sdev, -siteID,  -species, -pheno.stage, -pheno.var, -pheno.unit, -control, -control_sd, -newTT, -value) # same for sd
      
      dd4[,(ncol(dd4)+1):(ncol(dd4)+2)] <- turfs.15[match(dd4$siteID, turfs.15$siteID), c("Temperature_level", "Precipitation_level")] 
      dd4[,(ncol(dd4)+1):(ncol(dd4)+3)] <- traits.15[match(dd4$species, traits.15$species), c("functionalGroup", "flowering.time", "occurrence.2")]
    }
  }
  return(dd4)
}


#### MAKE PLOT ####
# data : pheno.treat
# control.truf: destination or origin
# p.var: peak, end, first, duration
# p.stage: flower, bud, seed
MakePlot <- function(dd, control.turf, p.var, p.stage, p.unit){
  if(control.turf == "destination"){
    warm <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = TT2, color = factor(destP_level), shape = factor(destT_level)), size = 4) +
      #geom_errorbar(aes(x = control, ymin=control-TT2_sd, ymax=control+TT2_sd), width=0.1, color = "gray") +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("lightblue","blue", "darkblue")) +
      scale_shape_manual(values = c(17,16)) +
      theme(legend.position="none") +
      ggtitle("warm") + ylab("Transplant") + xlab("") +
      geom_text(aes(x = control, y = TT2, label=species),hjust=0, vjust=0)
    warm + ylim(min(cbind(warm$TT2, warm$TT3, warm$TT4), na.rm = TRUE), max(cbind(warm$TT2, warm$TT3, warm$TT4), na.rm = TRUE))
    
    wet <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = TT3, color = factor(destP_level), shape = factor(destT_level)), size = 4) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("lightblue","blue", "darkblue")) +
      theme(legend.position="none") +
      ggtitle("wet") + ylab("") + xlab("Control (destination)") +
      geom_text(aes(x = control, y = TT3, label=species),hjust=0, vjust=0)
    wet + ylim(min(cbind(wet$TT2, wet$TT3, wet$TT4), na.rm = TRUE), max(cbind(wet$TT2, wet$TT3, wet$TT4), na.rm = TRUE))
    
    ww <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = TT4, color = factor(destP_level), shape = factor(destT_level)), size = 4) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(name = "Precipitation", labels = c("dry", "intermediate", "wet"), values = c("lightblue","blue", "darkblue")) +
      scale_shape_manual(name = "Temperature", labels = c("alpine", "subalpine"), values = c(17,16)) +
      theme(legend.position=c(0.2,0.8)) +
      ggtitle("warm & wet") + ylab("") + xlab("") +
      geom_text(aes(x = control, y = TT4, label=species),hjust=0, vjust=0, show.legend = FALSE)
    ww + ylim(min(cbind(ww$TT2, ww$TT3, ww$TT4), na.rm = TRUE), max(cbind(ww$TT2, ww$TT3, ww$TT4), na.rm = TRUE))
  }
  else if(control.turf == "origin"){
    warm <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = TT2, color = factor(Precipitation_level), shape = factor(Temperature_level)), size = 4) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("white","lightblue","blue", "darkblue")) +
      scale_shape_manual(values = c(17,16)) +
      theme(legend.position="none") +
      ggtitle("warm") + ylab("Transplant") + xlab("") +
      geom_text(aes(x = control, y = TT2, label=species),hjust=0, vjust=0)
    
    wet <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = TT3, color = factor(Precipitation_level), shape = factor(Temperature_level)), size = 4) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("white","lightblue","blue", "darkblue")) +
      scale_shape_manual(values = c(17,16)) +
      theme(legend.position="none") +
      ggtitle("wet") + ylab("") + xlab("Control (origin)") +
      geom_text(aes(x = control, y = TT3, label=species),hjust=0, vjust=0)
    
    ww <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = TT4, color = factor(Precipitation_level), shape = factor(Temperature_level)), size = 4) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(name = "Precipitation", labels = c("very dry", "dry", "intermediate", "wet"), values = c("white","lightblue","blue", "darkblue")) +
      scale_shape_manual(name = "Temperature", labels = c("alpine", "subalpine"), values = c(17,16)) +
      theme(legend.position=c(0.8,0.2)) +
      ggtitle("warm & wet") + ylab("") + xlab("") +
      geom_text(aes(x = control, y = TT4, label=species),hjust=0, vjust=0, show.legend = FALSE)
  }
  
  multiplot(warm, wet, ww, cols=3)
}



#### PLOT ERROR BARS ####
# x and y are the location of the points, z is the standard error, c the colour
bars <- function(x,y,z,c){for (k in 1:length(y)) for (i in c(-1, 1)) arrows(x[k], y[k], x[k], y[k]+i*z[k], angle=90, length=0, col=c)}
bars.y <- function(x,y,z,c){for (l in 1:length(x)) for (j in c(-1, 1)) arrows(x[l], y[l], x[l]+j*z[l], y[l], angle=90, length=0, col=c)}




#### FUNCTIONS TO ANALYSE DATA ####

#### CALCULATE OVERDISPERSION ####
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


#### MODEL SELECTION FUNCITON ####
modsel <- function(mods,x, phi = 1){  
  #phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df") # calculate nr of parameters
    dd$QAIC[j] = QAICc(mods[[j]],phi) # calculate QAIC
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}
  dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}

#### CALCULATE QAICc ####
QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(resid(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}


#### CHECK MODELS ####
fix.check <- function(mod){    #function to produce model-checking plots for the fixed effects of an lmer model
  par(mfrow = c(2,2))
  plot(fitted(mod),resid(mod))  #should have no pattern
  abline(h=0)
  print(anova(lm(fitted(mod)~resid(mod))))	#should be non-significant
  qqnorm(resid(mod), ylab="Residuals")		#should be approximately straight line
  qqline(resid(mod))
  plot(density(resid(mod)))					#should be roughly normally distributed
  rug(resid(mod))}

