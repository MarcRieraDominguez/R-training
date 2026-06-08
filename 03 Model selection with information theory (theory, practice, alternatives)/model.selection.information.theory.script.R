# Model Selection for Scientists: Theory, Practice, and Alternatives
# Developed by Marc Riera
# Contact: marcr93@gmail.com
#=========================================================================



# Load libraries ----------------------------------------------------------

library(AICcmodavg)
library(data.table)
library(DHARMa)
library(domir)
library(ggnewscale)
library(glmnet)
library(lmtest)
library(microbenchmark)
library(multcomp)
library(MuMIn)
library(performance)
library(PerformanceAnalytics)
library(see)
library(tidyverse)



# Before we start: pipe operator -----------------------------------------------------------

# The pipe operator (%>%, shortcut: Control + Shit + M) comes from the tidyverse package ecosystem, and allows to transfer the object in the left-hand side as the first argument of the function on the right-hand-side
test.pipe <- 1:5
summary(test.pipe)
test.pipe %>% summary() %>% str()
str(summary(test.pipe))

# You can use a dot "." to indicate in which position the transferred object should go
test.pipe %>% paste0(" <- text at the end") # with we do not write the dot, the piped object defaults to the first argument
test.pipe %>% paste0(., " <- text at the end") # we can be explicit using the dot
test.pipe %>% paste0("text at the beginning -> ", .) # The dot allows to use the piped object at the end of the function

test.pipe %>% paste0("surrounded by text -> ", ., " <- surrounded by text")

# R has a "native" pipe that does not depend on having installed packages (|>). In this script uses the tidyverse pipe
test.pipe |> summary()
test.pipe |> summary() |> str()




# Lists in R --------------------------------------------------------------

# Lists can store any type of object: numbers, text, matrices. It can be convenient to name the different elements of the list
test.list <- list("first" = 1, "second" = 1:5, "third" = 20, "fourth" = c("hello!", "good afternoon"))

# The lapply() function applys a user-defined function to each of the elements of the list
test.list %>% lapply(function(x) x ^ 3) # this one yields an error: text cannot be exponentiated!
test.list[1:3] %>% lapply(function(x) x ^ 3) # we can subset lists: keep the elements in the positions 1, 2 and 3 -> now it works fine
test.list["second"] %>% lapply(function(x) x ^ 3) # we can subset by name
test.list$second %>% lapply(function(x) x ^ 3) # We can subset with the dollar ($) operator

# You can use pipes within lapply(), and use the dot "." to control to which position the object is piped
test.list %>% lapply(function(x) n_distinct(x))
test.list %>% lapply(function(x) n_distinct(x) %>% paste0("The number of distinct elements is: ", ., ". You get the idea :)"))



# Load and prepare dataset ----------------------------------------------------------

# Load file and select key columns
data.ori <- data.table::fread(file = "dataset.habitat.range.transmitting.science.2026.txt", sep = "\t", colClasses = NULL, data.table = F, fill = T) %>% # Use pipe operator to select the relevant columns
  dplyr::select(species = E.M.name, habitat.range = hr1, intro.pathway = pathway.3cat, mrt, native.climatic.nb, height, life.form, dispersal)

# Check structure
summary(data.ori)
str(data.ori)

# Transform variables (standardization: mean = 0, variance = 1), declare factors (for categorical = qualitative variables)
data.mod <-
  data.ori %>% 
  mutate(across(.cols = c(mrt, height, native.climatic.nb), .fns = function(x) (x - mean(x))/sd(x), .names = "{.col}.std"),
         across(.cols = c(intro.pathway, life.form, dispersal), .fns = factor))

str(data.mod)
summary(data.mod)



# Fit a full model ----------------------------------------------------

# A full or saturated model contains all variables of interest: it embodies the hypothesis that "everything matters"
modfull <- lm(log(habitat.range) ~ intro.pathway + mrt.std + native.climatic.nb.std + height.std + life.form + dispersal, data = data.mod)

summary(modfull) # inspect it



# Some aspects of model quality, performance and validation ---------------

# Absolute measures of model quality, in this case and adjusted R-squared
summary(modfull)$adj.r.squared
performance::r2(modfull) # the performance package is convenient to get a variety of metrics... but it does little work on its own!! Rather, it will call other packages that do the actual calculations

# Models may be validated by inspecting the residuals. The residuals of the model are not wonderful, but I think that good enough to proceed
plot(resid(modfull) ~ fitted(modfull))
# The performance package is also useful
performance::check_model(modfull)
performance::check_residuals(modfull) %>% plot()
performance::check_normality(modfull) %>% plot()
# Recently, the DHARMa package has become quite useful, based on simulated residuals
DHARMa::simulateResiduals(modfull) %>% plot()

# An assessment of overfitting is also important. Variance Inflation Factors (VIFs) should remain below 5, and some will argue that the threshold should be: VIFs < 3
performance::check_collinearity(modfull) %>% plot()

# A likelihood ratio test will tells us whether the model is better than an intercept-only model (just the mean = ~ 1): does the model of itnerest differ significantly in log-likelihood from the null model?
lmtest::lrtest(modfull, update(modfull, . ~ 1))



# Compare multiple hypotheses --------------------------------------------------------------


# We can use the model.sel() function from the MuMIn package to compare hypotheses
model.sel(
  
  # List of models
  list("only.human" = lm(log(habitat.range) ~ intro.pathway + mrt.std, data = data.mod),
       "only.biological" = lm(log(habitat.range) ~ native.climatic.nb.std + height.std + life.form + dispersal, data = data.mod),
       "both.human.biological" = lm(log(habitat.range) ~ intro.pathway + mrt.std + native.climatic.nb.std + height.std + life.form + dispersal, data = data.mod)),
  
  # Which information criterion to do the ranking with
  rank = "AICc",
  
  # We might request additional information, supplied as user-defined functions
  extra = c(
    "r2" = function(x) summary(x)$adj.r.squared %>% round(digits = 3), # R2 = model quality, rounded
    "lrt" = function(x) lmtest::lrtest(x)$'Pr(>Chisq)'[2] %>% round(digits = 3), # lrt = likelihood ratio test, rounded! (no one needs a p-value with 15 decimal values)
    "vif.max" = function(x) performance::check_collinearity(x)$VIF %>% max())) # vif.max = the largest variance inflation factor among the explanatory variables

# This can be re-written so the focus is on the parts that change. In other words, since all hypotheses are tested with linear models (lm()) with the same dataset, we can iterate the same modelling procedure with three different sets of explanatory variables, rather than copy-pasting and introducing errors

model.table.with.a.list <-
  list("only.human" = "intro.pathway + mrt.std",
       "only.biological" = "native.climatic.nb.std + height.std + life.form + dispersal",
       "both.human.biological" = "intro.pathway + mrt.std + native.climatic.nb.std + height.std + life.form + dispersal") %>% 
  # Notice the convenience of lapply: 
  lapply(function(x) lm(formula(paste0("log(habitat.range) ~ ", x)), data = data.mod)) %>% 
  MuMIn::model.sel(rank = "AICc",
                   extra = c(
                     "r2" = function(x) summary(x)$adj.r.squared %>% round(digits = 3),
                     "lrt" = function(x) lmtest::lrtest(x)$'Pr(>Chisq)'[2] %>% round(digits = 3),
                     "vif.max" = function(x) performance::check_collinearity(x)$VIF %>% max()))

model.table.with.a.list
# View(model.table.with.a.list)

# You can update the table, let's add the null model! First, we duplicate the model selection table, and then append the null model
model.table.with.a.list.null <- model.table.with.a.list
# use model.sel() in the left hand side
model.sel(model.table.with.a.list.null) <- update(modfull, . ~ 1) # keep the response in the full model and use just the intercept to model
model.table.with.a.list.null

# Evidence ratio can be calculated as ratio of weights
model.table.with.a.list.null
# Only biological vs Only human
0.744/0.256


# Model averaging allows for inference from multiple models (= multiple competing hypotheses)
avg <-
  model.avg(model.table.with.a.list.null,
            delta < 6, # Keep those within 6 units of the best model
            revised.var = TRUE, # Revised formula for standard errors
            fit = FALSE) # Switching this to TRUE can be necessary if we want to use the model-averaged coefficients for prediction
avg
summary(avg)

# By default it shows the conditional average (no shrinking)
coef(avg)
coef(avg, full = FALSE)
coef(avg, full = TRUE) # Full averaging = shrinking towards 0 for those variables not present in all models
confint(avg, full = FALSE)
confint(avg, full = TRUE)


# Get the two tyes of coefficients
avg.coef <- list("full" = TRUE, "conditional" = FALSE) %>% 
  lapply(function(averaging.type) data.frame(var = names(coef(avg, full = averaging.type)),
                                             coef = coef(avg, full = averaging.type),
                                             confint.low = confint(avg, full = averaging.type)[, 1],
                                             confint.up = confint(avg, full = averaging.type)[, 2]))


# Assemble the coefficients in a data frame for plotting
avg.coef.df <-
  avg.coef %>% 
  bind_rows(.id = "averaging.type") %>% 
  mutate(res.fill = if_else((confint.low / confint.up) < 0, true = "95% CI overlaps 0", false = "95% CI does not overlap 0"))

avg.coef.df

# Comare coefficients visually
ggplot(data = avg.coef.df, aes(x = var, y = coef, group = averaging.type)) +
  geom_point(aes(shape = averaging.type, col = res.fill), position = position_dodge2(width = 0.7), size = 2) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = confint.low, ymax = confint.up), position = position_dodge2(width = 0.7)) +
  labs(x = "Variable",
       y = "Model-averaged coefficient",
       shape = "Tye of averaging",
       col = "Coefficient") +
  scale_x_discrete(labels = scales::label_wrap(35)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") 


# Sum of weights for the explanatory variables
MuMIn::sw(model.table.with.a.list.null)



# Fit all possible combinations -----------------------------------------------

# Setting for the dredge() function
options(na.action = na.fail)

# Check all variables combinations = all models = all hypotheses. If you do this, tell your reader, note that many scientists do not like it
# We will use two different information criteria for comparison
d.result <-
  list("AICc" = "AICc", "BIC" = "BIC") %>% 
  lapply(function (y) MuMIn::dredge(
    global.model = modfull,
    rank = y,
    extra = c(
      r2 = function(x) summary(x)$adj.r.squared %>% round(digits = 3),
      lrt = function(x) lmtest::lrtest(x)$'Pr(>Chisq)'[2] %>% round(digits = 3),
      vif.max = function(x) performance::check_collinearity(x)$VIF %>% max())
  )
  )

d.result
# d.result$BIC %>% View()
# d.result$AICc %>% View()
class(d.result$BIC)
summary(d.result$BIC)
attr(d.result$BIC, "model.calls")

# The weights add up to one
sum(d.result$BIC$weight)
sum(d.result$AICc$weight)


# Assess how different variables are related
PerformanceAnalytics::chart.Correlation(data.frame(d.result$AICc) %>% dplyr::select(logLik, r2, df, "AICc" = y, weight, delta))
PerformanceAnalytics::chart.Correlation(data.frame(d.result$BIC) %>% dplyr::select(logLik, r2, df, "BIC" = y, weight, delta))

# Note that we can subset with subset(), which by default is recalc.weights = FALSE
subset(d.result$BIC, delta <= 6)
subset(d.result$BIC, delta <= 6, recalc.weights = TRUE)
subset(d.result$BIC, delta <= 6, recalc.weights = FALSE)

# You can select a given model from the table, using a filtering criterion
best <- MuMIn::get.models(d.result$BIC, delta == 0)
best
summary(best)
# It returns a list, so it should be fine taking the first element
best[[1]]
summary(best[[1]])


# Model average
d.average <-
  lapply(d.result, function(x)
    subset(x, delta <= 6) %>% # Keep only the models within 6 units of the "best model"
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
            "averaging.type" = list("full" = TRUE, "conditional" = FALSE, "full" = TRUE, "conditional" = FALSE)),
       function(criterion, averaging.type) data.frame(var = names(coef(criterion, full = averaging.type)),
                                                      coef = coef(criterion, full = averaging.type),
                                                      confint.low = confint(criterion, full = averaging.type)[, 1],
                                                      confint.up = confint(criterion, full = averaging.type)[, 2],
                                                      averaging.type = if_else(averaging.type == TRUE, true = "full", false = "conditional"))) %>% 
  bind_rows(.id = "criterion") %>% 
  mutate(res.fill = if_else((confint.low / confint.up) < 0, true = "95% CI overlaps 0", false = "95% CI does not overlap 0"))

mod.avg.coef

# Plot coefficients obtained through various methods
ggplot(data = mod.avg.coef, aes(x = var, y = coef, group = averaging.type)) +
  geom_point(aes(shape = criterion, col = res.fill), position = position_dodge2(width = 0.7), size = 2) +
  scale_colour_viridis_d("", option = "viridis") +
  geom_hline(yintercept = 0) +
  ggnewscale::new_scale_colour()+
  geom_linerange(aes(ymin = confint.low, ymax = confint.up, col = averaging.type), position = position_dodge2(width = 0.7)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Variable",
       y = "Model-averaged coefficient",
       colour = "Type of averaging",
       shape = "Information Criterion") +
  scale_x_discrete(labels = scales::label_wrap(35)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") +
  guides(col = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))



# Prediction -------------------------------------------------------------

# calculate predictions (indicate fit = TRUE in the mod.avg call)
predict(object = d.average$AICc,
        newdata = data.frame("intro.pathway" = factor(c("Both", "Int", "Unint"), levels = c("Both", "Int", "Unint")),
                             "dispersal" = factor(c("Anemochorous"), levels = c("Anemochorous", "Endozoochorous", "Epizoochorous", "Unspecific")),
                             "life.form" = factor(c("Annual herbaceous"), levels = c("Annual herbaceous", "Perennial herbaceous", "Shrub/Tree")),
                             "mrt.std" = 0,
                             "native.climatic.nb.std" = 0,
                             "height.std" = 0),
        se.fit = TRUE,
        full = TRUE)



# Relative importance: sum of weights and an alternative (dominance analysis) -----------------------------------------------------

# Sum of Akaike weights
sum.weights <- sw(subset(d.result$AICc, delta <= 6) )
sum.weights

# Dominance analysis: assess all combinations of explanatory variables in terms of their model quality -> this allows to estimate the contribution of each explanatory variable to model quality
dom <- domir::domir(log(habitat.range) ~ intro.pathway + mrt.std + native.climatic.nb.std + height.std + life.form + dispersal,
                    function(x) {
                      summary(lm(x, data = data.mod))$adj.r.squared})
dom
str(dom)

dom$General_Dominance # absolute contribution to model quality = they sum up to the R2
dom$Standardized # relative contribution to model quality = they sum up to 1

# Compare sum of weights and dominance values
sw.dom <- 
  left_join(data.frame(vars = names(sum.weights),
                       sum.of.weights = as.numeric(sum.weights)),
            data.frame(vars = names(dom$Standardized),
                       general.dominance = as.numeric(dom$Standardized)),
            by = "vars")
sw.dom

# Check a correlation matrix
PerformanceAnalytics::chart.Correlation(sw.dom %>% dplyr::select(where(is.numeric)))


# Shrinkage methods -------------------------------------------------------

# Store the response and explanatory variables

# The explanatory variables should be stored as a data.matrix. Note that we have categorical factors with more than two categories, so we might use model.matrix() to turn them into dummy variables, and remove the intercept: the coefficients of the categorical values will now represent the effect in reference to a baseline, which is taken alphabetically (Intro.pathway = Both, Life.form = Annual herbaceous, Dispersal = anemochorous)
xvar.ridge <- model.matrix(~ intro.pathway + mrt.std + native.climatic.nb.std + height.std + life.form + dispersal, data = data.mod)[, -1]
xvar.ridge
yvar.ridge <- log(data.mod$habitat.range)
lambda.ridge <- 10^seq(2, -2, by = -.1) # Sequence of lambda values to search

# The model will try all user-supplied values of lambda
ridge <- glmnet::glmnet(xvar.ridge,
                        yvar.ridge,
                        alpha = 0, # 0 = ridge penalty, 1 = lasso penalty (which may set some coefficients to 0)
                        lambda =  lambda.ridge) # Range of lambda values to explore

ridge
summary(ridge)

# Function taken from: https://www.r-bloggers.com/2024/01/understanding-lasso-and-ridge-regression-4/
lbs.fun <- function(fit, offset_x=1, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])+ offset_x
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}

plot(ridge) # with increasing penalty, coefficients shrink towards 0, although not exactly 0
lbs.fun(ridge)

# Find lambda that minimizes Mean-Squared Error (MSE)
ridge.cv <- cv.glmnet(xvar.ridge, yvar.ridge, alpha = 0, lambda = lambda.ridge)
ridge.cv
plot(ridge.cv) # Largest lambda within 1 standard error of the lambda that minimizes error (dotted line, left) and lambda that minimizes error (dotted line, right)
ridge.cv$lambda.min; -log(ridge.cv$lambda.min)
ridge.cv$lambda.1se; -log(ridge.cv$lambda.1se)

ridge.min.lambda <- glmnet::glmnet(xvar.ridge,
                        yvar.ridge,
                        alpha = 0, # 0 = ridge penalty, 1 = lasso penalty
                        lambda =  ridge.cv$lambda.min) # Minimum value of lambda

coef(ridge.min.lambda) # No standard errors by default. I am not sure about how one would obtain them, as this method biases coefficients away from 0 and the usual formulas do not apply



# Simulation --------------------------------------------------------------

### Note that this is a relatively well-behaved and simple dataset (no autocorrelation etc) and that we are ignoring issues such as collinearity, etc.

# Ensure reproducibility with set.seed()
set.seed(123)

sim.N.iter <- 200 # Number of iterations (= number of simulations)
sim.N <- 100 # Number of observations in the simulated datasets

sim.df <- vector("list", sim.N.iter) # Create list to store the simulation results
names(sim.df) <- 1:sim.N.iter # Name the list

# Simulate a number of datasets: all produce the response variable with the same formula, but with some randomness!!
i <- 1
for(i in 1:sim.N.iter){
  
  sim.x_1 <- rnorm(sim.N, 0, 1) # generate random values from a normal (= gaussian) distribution that has mean = 0, standard deviation = 1
  sim.x_2 <- rnorm(sim.N, 0, 1)  
  sim.x_3 <- rnorm(sim.N, 0, 1)  
  sim.x_4 <- rnorm(sim.N, 0, 1)
  sim.x_5 <- rnorm(sim.N, 0, 1)
  
  # Note that variable x_5 is not involved in this equation -> x_5 is spurious = has no actual relationship to the response
  sim.y <- 0.3 + 0.9 * sim.x_1 - 0.4 * sim.x_2 + 0.2 * sim.x_3 + 0.1 * sim.x_4 + rnorm(sim.N, 0, 1)
  
  # Store a data frame with the variables
  sim.df[[i]] <- data.frame(sim.y,
                            sim.x_1,
                            sim.x_2,
                            sim.x_3,
                            sim.x_4,
                            sim.x_5)
} # end loop

# Try all possible variable combinations: 2^5 = 32 combinations
sim.d <- lapply(sim.df, function(x)
  lm(sim.y ~ sim.x_1 + sim.x_2 + sim.x_3 + sim.x_4 + sim.x_5, data = x) %>%
    MuMIn::dredge(rank = "AICc"))

# The "best" subset (in this case, defined as 6 units from the the lowest AICc) contains a variable number of models
sim.d %>% lapply(function(x) x %>% subset(delta <= 6) %>% nrow()) %>% unlist() %>% table()

# Average models within the "best" subset
sim.avg <- lapply(sim.d, function(x)
  x %>%
    subset(delta <= 6) %>% 
    model.avg())

# Gather coefficients from two types of averaging: full and conditional
sim.avg.coef <-
  lapply(list("full" = TRUE, "conditional" = FALSE),
         function(t) {
           lapply(sim.avg, function(x)
             data.frame(
               var = names(coef(x)),
               coef = coef(x, full = t),
               confint.low = confint(x, full = t)[, 1],
               confint.up = confint(x, full = t)[, 2],
               averaging.type = ifelse(t == TRUE, "full", "conditional")
             )) %>%
             bind_rows(.id = "simulation.number")
         }) %>% 
  bind_rows(.id = "averaging.type") %>% 
  mutate(overlap.0 = if_else((confint.low / confint.up) < 0, true = TRUE, false = FALSE))

# Plot the results
ggplot(data = sim.avg.coef) + geom_boxplot(aes(x = var, y = coef, col = averaging.type))

# Let's examine in what percentage of cases the variables would be considered "significant" from the perspective of the confidence interval overlapping with 0
sim.avg.coef %>% 
  group_by(averaging.type, var) %>%
  summarise(percentage.significant = sum(!overlap.0)/n())


# Calculate the sum of weights across all possible models
sim.sw <-
  lapply(sim.d, function(dredge.result)
    dredge.result %>%
      subset(delta <= 6) %>% # within 6 units of the best
      sw()) %>% 
  lapply(function(x)
    data.frame(vars = names(x),
               sum.of.weights = as.numeric(x))) %>%
  bind_rows(.id = "Sim.number")

# Plot the result: we expect weight(x_1) > weight(x_2) > weight(x_3) > weight(x_4) > weight(x_5). In fact, we would expect that weight(x_5) = 0
plot.sw <- ggplot(data = sim.sw) + geom_boxplot(aes(x = vars, y = sum.of.weights))
plot.sw

# Test an alternative to calculate relative importance
# Create a list to store the result of dominance analysis
sim.dom <- vector("list", sim.N.iter)

# Run dominance analysis inside a loop
j <- 1
for(j in 1:sim.N.iter){
  dat <- sim.df[[j]]
  sim.dom[[j]] <- domir::domir(
    sim.y ~ sim.x_1 + sim.x_2 + sim.x_3 + sim.x_4 + sim.x_5,
    function(y) {
      summary(lm(y, data = dat))$adj.r.squared
    } # end function to calculate model quality
  ) # end dominance analysis
} # end loop

# Gather the dominance results in a data frame
sim.dom.df <-
  sim.dom %>% 
  lapply(function(x) x[["Standardized"]]) %>% # Standardized dominance: values add up to 1
  lapply(function(x) data.frame(vars = names(x),
                                standardized.dominance = as.numeric(x))) %>% 
  bind_rows(.id = "Sim.number")

# Assess whether we got negative values of standardized dominance
sim.dom.negative <- sim.dom.df %>% filter(standardized.dominance < 0)
nrow(sim.dom.negative) # a thousand instances
table(sim.dom.negative$vars) # a negative value was given mostly to the spurious variable, but also to the fourth most important, and more rarely, to the third most important

# Plot the results of the dominance analysis, we expect x_1 > x_2 > x_3 > x_4 > x_5
plot.dom <- ggplot(data = sim.dom.df) + geom_boxplot(aes(x = vars, y = standardized.dominance))
plot.dom

plot.sw + plot.dom # Note that the sum of weights of all variables does not add up to 1, while the sum of standardized dominance does sum up to 1
cowplot::plot_grid(plot.sw, plot.dom, ncol = 2)


# Extra: parallelize dredge -----------------------------------------------

## Create large dataset
p.N <- 3e5
p.x1 <- rnorm(p.N, 1, 2) # random normal distribution
p.x2 <- rnorm(p.N, 4, 2)
p.x3 <- rnorm(p.N, 10, 2)
p.y <- 0.3 + 0.4 * p.x1 - 0.2 * p.x2 + 0.9 * p.x3 + rnorm(p.N, 0, 0.25)

p.df <- data.frame(p.y,
                   p.x1,
                   p.x2,
                   p.x3)


# create cluster "clust" and export data and libraries (following documentation of pdredge)
clust  <-  parallel::makeCluster((parallel::detectCores() - 1))
parallel::clusterEvalQ(clust, library(MuMIn))
parallel::clusterExport(clust, "p.df")

# Compare running time
microbenchmark::microbenchmark(
  
  # No parallel
  MuMIn::dredge(global.model = lm(p.y ~ p.x1 + p.x2 + p.x3, data = p.df), rank = "AICc"),
  
  # Parallel dredge with "clust"
  MuMIn::dredge(global.model = lm(p.y ~ p.x1 + p.x2 + p.x3, data = p.df), rank = "AICc", cluster = clust),
  
  times = 10,
  
  unit = "seconds")



# EXTRA: alternatives packages --------------------------------------------


# Other packages produce such tables, but they sometimes are less flexible. For instance, AICcmodavg::aictab() does not handle any type of model. I would recommend using MuMIn in general as it is very flexibles, but just for you to know, there are alternatives
aictable <- AICcmodavg::aictab(cand.set = list(lm(log(habitat.range) ~ intro.pathway + mrt.std, data = data.mod),
                                               lm(log(habitat.range) ~ native.climatic.nb.std + height.std + life.form + dispersal, data = data.mod),
                                               lm(log(habitat.range) ~ intro.pathway + mrt.std + native.climatic.nb.std + height.std + life.form + dispersal, data = data.mod)), modnames = c("only.human", "only.biological", "both.human.biological"))
aictable
AICcmodavg::evidence(aictable)
