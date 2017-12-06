# Overdispersion

library(lme4)


disp <- function(mod,data){
  rdev <- sum(residuals(mod)^2)
  mdf <- length(fixef(mod))
  rdf <- nrow(data)-mdf
  rdev/rdf}


head(pheno.long)
mod1 <- glmer(value ~ destT_level * destP_level + (1|destSiteID) + (1|variable), data=pheno.long, family=poisson)


disp(mod1, pheno.long)
