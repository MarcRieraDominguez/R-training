# Model selection with information criteria and multimodel inference
# Developed by Marc Riera
# Contact: m.riera@creaf.uab.cat
#=========================================================================

#### Note that the aim of this script is to show the use of various functions, and not an endorsement of a particular way to do research.
#### In particular, the comparison of multiple information criteria is meant to be pedagogical, not a recommendation that in real life, one should try multiple approaches, and select one afterwards.
#### Moreover, we avoid fitting an offset, as shown in the documentation of the "Owls" dataset, as the goal is to showcase the behavior of specific functions.
#### Note that model selection is not applied in these examples to the random part of the model, nor to the zero-inflation
#### To focus on implementation, theory is kept to a minimum in this script, check the theoretical materials for further details


# Load libraries ----------------------------------------------------------

library(AICcmodavg)
library(data.table)
library(domir)
library(ggnewscale)
library(glmmTMB)
library(lmtest)
library(microbenchmark)
library(multcomp)
library(MuMIn)
library(performance)
library(PerformanceAnalytics)
library(tidyverse)



# Load files ----------------------------------------------------------

# Load dataset, included inside the glmmTMB package
data.ori <- glmmTMB::Owls
summary(data.ori)
str(data.ori)


# Prepare dataset --------------------------------------------------------

# Standardize continuous explanatory variable
data.mod <-
  data.ori %>% 
  mutate(logBroodSize.std = (logBroodSize - mean(logBroodSize))/sd(logBroodSize))
str(data.mod)
summary(data.mod)



# Fit a full model ----------------------------------------------------

# Mixed-effects negative binomial with zero-inflation
modfull <- glmmTMB(SiblingNegotiation ~ FoodTreatment + SexParent + logBroodSize.std + (1|Nest), family = nbinom1(), zi = ~1, data=data.mod)
summary(modfull)

# Absolute measures of model quality
# Visual assessment of residuals
DHARMa::simulateResiduals(modfull) %>% plot()
performance::check_model(modfull)
plot(resid(modfull) ~ fitted(modfull))
# Likelihood ratio test, only testing the fixed effects
lmtest::lrtest(modfull, update(modfull, . ~ 1 + (1|Nest)))
# R-squared: marginal and conditional
MuMIn::r.squaredGLMM(modfull)



# Hypotheses --------------------------------------------------------------

# Compare hypotheses, including a null model. We keep some aspects of the model constant across models, such as logBroodSize, the ranfom effect, and zero-inflation
model.table <-
  list("only.parent" = "SexParent",
       "only.food" = "FoodTreatment",
       "both" = "SexParent + FoodTreatment",
       "null.model" = "1") %>% 
  lapply(function(x) glmmTMB::glmmTMB(formula(paste0("SiblingNegotiation ~ ", x, "+ logBroodSize.std + (1|Nest)")), family = nbinom1(), zi = ~1, data=data.mod)) %>%
  MuMIn::model.sel(rank = "AICc",
                   extra = c(
                     r2m = function(x) MuMIn::r.squaredGLMM(x)[1,1] %>% round(digits = 3),
                     r2c = function(x) MuMIn::r.squaredGLMM(x)[1,2] %>% round(digits = 3),
                     lrt = function(x) lmtest::lrtest(x, update(x, . ~ 1 + (1|Nest)))$'Pr(>Chisq)'[2] %>% round(digits = 3),
                     vif.max = function(x) performance::check_collinearity(x)$VIF %>% max()))

model.table
# A focus without the variables
View(model.table[, -c(1:5)])

# Sum of weights
MuMIn::sw(model.table)


# Evidence ratio can be calculated as ratio of weights: the "only.food" is almost three times as likely to be the actual "best" model according to the Kullback-Leibler distance
0.73/0.27

# The AICcmodavg package has even a  dedicated function
aictable <- AICcmodavg::aictab(cand.set = list(glmmTMB(SiblingNegotiation ~ SexParent + logBroodSize.std + (1|Nest), family = nbinom1(), zi = ~1, data=data.mod),
                                               glmmTMB(SiblingNegotiation ~ FoodTreatment + logBroodSize.std + (1|Nest), family = nbinom1(), zi = ~1, data=data.mod),
                                               glmmTMB(SiblingNegotiation ~ FoodTreatment + SexParent + logBroodSize.std + (1|Nest), family = nbinom1(), zi = ~1, data=data.mod),
                                               glmmTMB(SiblingNegotiation ~ 1 + (1|Nest), family = nbinom1(), zi = ~1, data=data.mod)
                                               ),
                               modnames = c("only.parent", "only.food", "both", "null.model"))

# Interestingly, both packages disagree in how many parameters the null model is estimating
aictable

AICcmodavg::evidence(aictable)
0.73/0.27


# All possible combinations -----------------------------------------------

# Setting for the dredge() function
options(na.action = na.fail)

# Check all variables combinations = all models = all hypotheses. If you do this, tell your reader, note that many scientists do not like it
d.result <-
  list("BIC" = "BIC", "AICc" = "AICc") %>% 
  lapply(function (y) MuMIn::dredge(
    global.model = modfull,
    rank = y,
    extra = c(
      r2m = function(x) MuMIn::r.squaredGLMM(x)[1,1] %>% round(digits = 3),
      r2c = function(x) MuMIn::r.squaredGLMM(x)[1,2] %>% round(digits = 3),
      lrt = function(x) lmtest::lrtest(x, update(x, . ~ 1 + (1|Nest)))$'Pr(>Chisq)'[2] %>% round(digits = 3),
      vif.max = function(x) performance::check_collinearity(x)$VIF %>% max())
    )
  )

d.result
d.result$BIC %>% View()
d.result$AICc %>% View()
class(d.result$BIC)
summary(d.result$BIC)
attr(d.result$BIC, "model.calls")

# The weights add up to one
sum(d.result$BIC$weight)
sum(d.result$AICc$weight)


# Assess how different variables are related
PerformanceAnalytics::chart.Correlation(data.frame(d.result$BIC) %>% dplyr::select(logLik, r2m, r2c, df, y, weight, delta))
PerformanceAnalytics::chart.Correlation(data.frame(d.result$AICc) %>% dplyr::select(logLik, r2m, r2c, df, y, weight, delta))

# Note that we can subset with subset(), which by default is recalc.weights = FALSE
subset(d.result$BIC, delta <= 6)
subset(d.result$BIC, delta <= 6, recalc.weights = TRUE)
subset(d.result$BIC, delta <= 6, recalc.weights = FALSE)

# A function to get the "best" model
best <- MuMIn::get.models(d.result$BIC, delta == 0)
best
summary(best)
# It returns a list, so it should be fine taking the first element
best[[1]]
summary(best[[1]])


# Model average
d.average <-
  lapply(d.result, function(x)
    subset(x, delta <= 6) %>% 
      # fit = TRUE stores the fitted models, which is useful latter for prediction
      MuMIn::model.avg(revised.var = TRUE, fit = TRUE))

d.average
summary(d.average$BIC)

# By default it shows the conditional average (no shrinking)
coef(d.average$AICc)
coef(d.average$AICc, full = FALSE)
coef(d.average$AICc, full = TRUE)
confint(d.average$AICc, full = FALSE)
confint(d.average$AICc, full = TRUE)

# Get coefficients in a table to plot them and compare
mod.avg.coef <- 
  pmap(list("criterion" = list("BIC" = d.average$BIC, "BIC" = d.average$BIC, "AICc" = d.average$AICc, "AICc" = d.average$AICc),
            "full.averaging" = list("full" = TRUE, "subset" = FALSE, "full" = TRUE, "subset" = FALSE)),
       function(criterion, full.averaging) data.frame(var = names(coef(criterion, full = full.averaging)),
                                                    coef = coef(criterion, full = full.averaging),
                                                    confint.low = confint(criterion, full = full.averaging)[, 1],
                                                    confint.up = confint(criterion, full = full.averaging)[, 2],
                                                    full.averaging = full.averaging)) %>% 
  bind_rows(.id = "criterion") %>% 
  mutate(res.fill = if_else((confint.low / confint.up) < 0, true = "95% CI overlaps 0", false = "95% CI does not overlap 0"))

mod.avg.coef

# Plot coefficients obtained through various methods
ggplot(data = mod.avg.coef, aes(x = var, y = coef, group = full.averaging)) +
  geom_point(aes(shape = criterion, col = res.fill), position = position_dodge2(width = 0.7), size = 3) +
  scale_colour_viridis_d("", option = "viridis") +
  geom_hline(yintercept = 0) +
  ggnewscale::new_scale_colour()+
  geom_linerange(aes(ymin = confint.low, ymax = confint.up, col = full.averaging), position = position_dodge2(width = 0.7)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Variable",
       y = "Model-averaged coefficient",
       colour = "Full averaging",
       shape = "Information Criterion") +
  scale_x_discrete(labels = scales::label_wrap(35)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") +
  guides(col = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))



# Prediction -------------------------------------------------------------

# calculate predictions (indicate fit = TRUE in the mod.avg call). A data frame can be specified in the newdata argument
predict(object = d.average$AICc, se.fit = TRUE, full = TRUE)



# Relative importance -----------------------------------------------------

# Sum of Akaike weights
sw(subset(d.result$BIC, delta <= 6) )
sw(subset(d.result$AIC, delta <= 6) )


# Dominance analysis: https://github.com/jluchman/domir
# https://stats.stackexchange.com/questions/622256/dominance-analysis-with-a-random-effects-beta-regression-nested-random-effects
# https://github.com/jluchman/domir/discussions/10
dom <- domir::domir(SiblingNegotiation ~ FoodTreatment + SexParent + logBroodSize.std,
                    function(x) {
                      glmmTMB(formula = update(x, . ~ . + (1|Nest)), family = nbinom1(), zi = ~1, data=data.mod)%>% MuMIn::r.squaredGLMM() %>% .[1, 1]})
dom



# Extra: parallelize dredge -----------------------------------------------

# Note that glmmTMB can also be parallelized: https://stackoverflow.com/questions/74222940/parallelize-both-model-fitting-and-dredging-glmmtmb-dredge

## Create large dataset
N <- 3e5
x1 <- rnorm(N, 1, 2)
x2 <- rnorm(N, 4, 2)
x3 <- rnorm(N, 10, 2)
y <- 0.3 + 0.4 * x1 - 0.2 * x2 + 0.9 * x3 + rnorm(N, 0, 0.25)

df <- data.frame(y,
                 x1,
                 x2,
                 x3)


# create cluster "clust" and export data and libraries (following documentation of pdredge)
clust  <-  parallel::makeCluster((parallel::detectCores() - 1))
parallel::clusterEvalQ(clust, library(MuMIn))
parallel::clusterExport(clust, "df")

# Compare running time: parallelization is faster for large datasets
microbenchmark::microbenchmark(
  
  # No parallel
  MuMIn::dredge(global.model = lm(y ~ x1 + x2 + x3, data = df), rank = "AICc"),
  
  # Parallel dredge with "clust"
  MuMIn::dredge(global.model = lm(y ~ x1 + x2 + x3, data = df), rank = "AICc", cluster = clust),
  
  times = 10,
  unit = "seconds")

