#### TEST DIFFERENCE IN SMT IN TREATMENTS ####

# Create MetaData
SMData <- MetaData %>%
  filter(!is.na(SMDiff.w), Treatment != "Control")


fit <- lmer(SMDiff.w ~ Treatment + (1|siteID/blockID), SMData)
summary(fit)
plot(fit)
hist(SMData$SMDiff.w)

ggplot(SMData, aes(y = SMDiff.w, x = siteID, color = Treatment, shape = Shape)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter() + 
  facet_wrap(~ Treatment)
