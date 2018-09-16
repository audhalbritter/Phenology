#### Cover and Flowering

library(reshape2)

phenology.variables <- phenology.var(phenology, "peak", year=2014)
# get rid of invaders
good.sp.data <- get.rid.of.invaders(phenology.variables)
head(good.sp.data)

### reshape 1 to long data set, remove NAs and add cover data
pheno.dat <- good.sp.data[,c(5,3,12:211)] # select turfID, Year and species
pheno.cover <- melt(pheno.dat, id=c("turfID", "Year"))
pheno.cover <- na.omit(pheno.cover) # remove all species with NA
colnames(pheno.cover) <- c("turfID", "Year", "species","nr.flower")
pheno.cover[,5:13] <- phenology.meta[match(pheno.cover$turfID, phenology.meta$turfID),c(1:2,4,6:11)] # add siteID, TTtreat, T_level,... to data set
cover <- read.table("turfCommunity.csv", sep=";", header=TRUE)
cover.13 <- cover[cover$year=="2013",] # choose cover from 2013
pheno.cover[,14] <- cover.13[match(pheno.cover$turfID, cover.13$turfID),4] # add cover
colnames(pheno.cover)[14] <- "cover"
# standardize nr. flowers
pheno.cover$st.flower <- (pheno.cover$nr.flower - min(pheno.cover$nr.flower))/range(pheno.cover$nr.flower)



ttc <- subset(pheno.cover, pheno.cover$TTtreat=="TTC")
tt2 <- subset(pheno.cover, pheno.cover$TTtreat=="TT2")
tt3 <- subset(pheno.cover, pheno.cover$TTtreat=="TT3")
tt4 <- subset(pheno.cover, pheno.cover$TTtreat=="TT4")
par(mfrow=c(2,2), mar=c(1,1,0.5,0.5), mgp=c(3,0.5,0), oma=c(3,3,0,0), cex=1.5)
plot((ttc$cover), ttc$st.flower, xlim=c(0,30), ylim=c(0,0.5), axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=FALSE); axis(2, labels=TRUE)
plot(tt2$cover, tt2$st.flower, xlim=c(0,30), ylim=c(0,0.5), axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=FALSE); axis(2, labels=FALSE)
plot(tt3$cover, tt3$st.flower, xlim=c(0,30), ylim=c(0,0.5), axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
plot(tt4$cover, tt4$st.flower, xlim=c(0,30), ylim=c(0,0.5), axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
mtext("% cover", side=1, outer=TRUE, line=1.5, cex=2)
mtext("probportion flowering", side=2, outer=TRUE, line=1.5, cex=2)

summary(lm(st.flower ~ cover, pheno.cover))

