#### DESTINATION ####

dd<-pheno.treat
dd <- ddd2
input <- dd
xxlim <- range(input$control, na.rm=TRUE)
yylim <- range(cbind(input$"TT2", input$"TT3", input$"TT4"), na.rm=TRUE)
par(mfrow=c(1,1), mar=c(1,1,1.5,0.5), mgp=c(1,1,0), oma=c(3,3,1,0.5), cex=1.5)

plot(input$control, input$TT2, main="warmer", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
bars(input$control, input$TT2, input$control.sd, c="grey")
bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.3,7), jitter(input$TT2+0.3,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.3,7), jitter(input$TT2+0.3,0), labels=input$destSiteID, cex= 0.5)
legend(1.2,14, legend=c("wet", "intermediate", "dry"), col=c("darkblue", "blue", "lightblue"), pch=16, cex=0.8, bty = "n")

plot(input$control, input$TT3, main="wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
bars(input$control, input$TT3, input$control.sd, c="grey")
bars.y(input$control, input$TT3, input$tt3.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.3,7), jitter(input$TT3+0.3,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.3,7), jitter(input$TT3+0.3,0), labels=input$destSiteID, cex= 0.5)
legend(1.2,14, legend=c("alpine", "intermediate"), col=c("darkblue"), pch=c(17,16), cex=0.8, bty = "n")

plot(input$control, input$TT4, main="warmer & wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$destT_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
bars(input$control, input$TT4, input$control.sd, c="grey")
bars.y(input$control, input$TT4, input$tt4.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.3,7), jitter(input$TT4+0.3,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.3,7), jitter(input$TT4+0.3,0), labels=input$destSiteID, cex= 0.5)
mtext("Destination (control)", 1, outer=TRUE, line=1.3, cex=3)
mtext("Transplant", 2, outer=TRUE, line=1.3, cex=3)



# Plot: functional groups/flowering time
plot(input$control, input$TT2, main="warmer", xlim=xxlim, ylim=yylim, axes=FALSE, ylab="", xlab="", pch=as.numeric(input$flowering.time)+15, col=ifelse(input$destP_level<=2, "lightblue", ifelse(input$destP_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
bars(input$control, input$TT2, input$control.sd, c="grey")
bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.3,7), jitter(input$TT2+0.3,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.3,7), jitter(input$TT2+0.3,0), labels=input$destSiteID, cex= 0.5)
legend(1.2,14, legend=c("forbs", "graminoids"), col=c("black", "red"), pch=16, cex=0.8, bty = "n")


#### ORIGIN ####
dd<-pheno.treat
input <- dd
xxlim <- range(input$control, na.rm=TRUE)
yylim <- range(cbind(input$"TT2", input$"TT3", input$"TT4"), na.rm=TRUE)
par(mfrow=c(1,1), mar=c(1,1,1.5,0.5), mgp=c(1,1,0), oma=c(3,3,1,0.5), cex=1.5)

plot(input$control, input$TT2, main="warmer", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$Temperature_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$Precipitation_level<=2, "lightblue", ifelse(input$Precipitation_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=TRUE)
bars(input$control, input$TT2, input$control.sd, c="grey")
bars.y(input$control, input$TT2, input$tt2.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.3,7), jitter(input$TT2+0.3,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.3,7), jitter(input$TT2+0.3,0), labels=input$siteID, cex= 0.5)
legend(1.2,14, legend=c("wet", "intermediate", "dry"), col=c("darkblue", "blue", "lightblue"), pch=16, cex=0.8, bty = "n")

plot(input$control, input$TT3, main="wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$Temperature_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$Precipitation_level<=2, "lightblue", ifelse(input$Precipitation_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
bars(input$control, input$TT3, input$control.sd, c="grey")
bars.y(input$control, input$TT3, input$tt3.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.4,7), jitter(input$TT3+0.4,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.4,7), jitter(input$TT3+0.4,0), labels=input$siteID, cex= 0.5)
legend(1.2,14, legend=c("alpine", "intermediate"), col=c("darkblue"), pch=c(17,16), cex=0.8, bty = "n")

plot(input$control, input$TT4, main="warmer & wetter", xlim=xxlim, ylim=yylim, axes=FALSE, pch=ifelse(input$Temperature_level<=1, 17, 16), ylab="", xlab="", col=ifelse(input$Precipitation_level<=2, "lightblue", ifelse(input$Precipitation_level>=4, "darkblue", "blue")))
box(); axis(1, labels=TRUE); axis(2, labels=FALSE)
bars(input$control, input$TT4, input$control.sd, c="grey")
bars.y(input$control, input$TT4, input$tt4.sd, c="grey")
abline(1,1, lty=2)
text(jitter(input$control+0.3,7), jitter(input$TT4+0.3,0), labels=input$variable, cex= 0.5)
text(jitter(input$control+0.3,7), jitter(input$TT4+0.3,0), labels=input$siteID, cex= 0.5)
mtext("Destination (control)", 1, outer=TRUE, line=1.3, cex=3)
mtext("Transplant", 2, outer=TRUE, line=1.3, cex=3)