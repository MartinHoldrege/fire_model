
library(DALEX)

# generate data -----------------------------------------------------------


n <- 1000
x1 <- runif(n, -3, 3) 
x2 <- -3*x1 + x1^2 + rnorm(n, sd = 0.5) #strangely correlated predictor variables


y <- 10*x1 + 5*x2 - 5*x1^2 + 10*x1*x2+ rnorm(n, sd = 3)
plot(x1, x2)
plot(x1, y)
plot(x2, y)
df <- data.frame(y, x1, x2)


# fit model ---------------------------------------------------------------


mod <- lm(y~x1 + x2 + I(x1^2) + x1:x2, data = df)
summary(mod)


# model diagnostics plots -------------------------------------------------


exp<- DALEX::explain(model = mod,data = df[-1],y = y)


part <- model_profile(explainer = exp,
                        type       = "partial",
                        variables  =c('x1', 'x2'),
                        N = NULL)

acc <- variable_profile(explainer = exp,
                        type       = "accumulated",
                        variables  =c('x1', 'x2'),
                        N = NULL)

acc$agr_profiles$`_label_` = "accumulated local"
part$agr_profiles$`_label_` = "partial dependence"
# showing both partial dependence and accumulated local profiles
# not the al profile for x2 is perhaps different then expected.
# (presumably because of the interaction)
plot(part, acc)



# compare to the pdp plot from the partial package
# (should be the same as type = 'partial' from DALEX)
# library(patchwork)
# g1 <- pdp::partial(mod, pred.var = 'x1', plot = TRUE, plot.engine  = 'ggplot')
# g2 <- pdp::partial(mod, pred.var = 'x2', plot = TRUE, plot.engine  = 'ggplot')
# g1 + g2
