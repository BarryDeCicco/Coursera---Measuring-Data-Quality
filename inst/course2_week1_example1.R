# course2_week1_example1

load("data/ess_belgium.rdata")

ess_belgium$int50[ess_belgium$int_rr <= 50] <- 1 # define two subgroups (could be anything!)
ess_belgium$int50[ess_belgium$int_rr > 50] <- 0

summary(ess_belgium$int50)

ess.red <- ess_belgium[ess_belgium$trstplt != 88 & ess_belgium$trstprt != 88 & ess_belgium$trstprl != 88,] # remove missing data!

dim(ess.red) # 1,674 cases for analysis

library(lavaan)

trst.model <- '  trstlead =~ trstplt + trstprt + trstprl '

# Fit Unconstrained Model (both groups have have their own loadings):

fit <- cfa(trst.model,
           data = ess.red,
           group = "int50")

summary(fit)


# Fit Constrained Model (both groups to have equal loadings):

fit.loads <- cfa(trst.model,
                 data = ess.red,
                 group.equal = c("loadings"),
                 group = "int50")

summary(fit.loads)
