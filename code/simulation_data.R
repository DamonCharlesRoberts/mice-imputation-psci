# Title: Simulation data for RF MICE

# Notes:
    #* Description: R script to generate simulated data to examine the RF MICE procedure and AMELIA
    #* Updated: 2022-05-23
    #* Updated by: dcr

# Setup
    #* Set seed 
set.seed(717)
    #* Modularly load functions to create simulated data
box::use(
    fabricatr = fabricatr[...],
    DeclareDesign = DeclareDesign[...]
)

# Simple DGP dataset
    #* Creating variables involved in a simple DGP
simple_dgp_mvn = fabricate(N = 1000,
    Var1 = rnorm(N, mean = 10),
    Var2 = draw_binary(N = N, prob = 0.5),
    Var3 = draw_ordered(x = rnorm(N, mean = 3.5), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var4 = rnorm(N, mean = 0),
    Var5 = draw_binary(N = N, prob = 0.25),
    Var6 = draw_ordered(x = rnorm(N, mean = 1.75), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var7 = rnorm(N, mean = -10),
    Var8 = draw_binary(N = N, prob = 0.75),
    Var9 = draw_ordered(x = rnorm(N, mean = 5.25), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    X = draw_binary(N = N, prob = ifelse(Var2 == 1, 0.4, 0.6)),
    Z = draw_binary(N = N, prob = ifelse(Var5 == 1, 0.4, 0.6)),
    Y = draw_ordered(x = rnorm(N, mean = 0.9 * X + 0.3 * Z), breaks = c(1, 2, 3, 4, 5, 6, 7))
)
simple_dgp_nmvn = fabricate(N = 1000,
    Var1 = rnorm(N, mean = 10),
    Var2 = draw_binary(N = N, prob = 0.5),
    Var3 = draw_ordered(x = rnorm(N, mean = 3.5), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var4 = rnorm(N, mean = 0),
    Var5 = draw_binary(N = N, prob = 0.25),
    Var6 = draw_ordered(x = rnorm(N, mean = 1.75), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var7 = rnorm(N, mean = -10),
    Var8 = draw_binary(N = N, prob = 0.75),
    Var9 = draw_ordered(x = rnorm(N, mean = 5.25), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    X = draw_binary(N = N, prob = ifelse(Var2 == 1, 0.4, 0.6)),
    Z = draw_binary(N = N, prob = ifelse(Var5 == 1, 0.4, 0.6)),
    Y = draw_ordered(x = 0.9 * X + 0.3 * Z + rnorm(N), breaks = c(1,2,3,4,5,6,7))
)
# Moderated DGP dataset
    #* Creating variables involved in a moderated DGP
moderated_dgp_mvn = fabricate(N = 1000,
    Var1 = rnorm(N, mean = 10),
    Var2 = draw_binary(N = N, prob = 0.5),
    Var3 = draw_ordered(x = rnorm(N, mean = 3.5), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var4 = rnorm(N, mean = 0),
    Var5 = draw_binary(N = N, prob = 0.25),
    Var6 = draw_ordered(x = rnorm(N, mean = 1.75), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var7 = rnorm(N, mean = -10),
    Var8 = draw_binary(N = N, prob = 0.75),
    Var9 = draw_ordered(x = rnorm(N, mean = 5.25), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    X = draw_binary(N = N, prob = (0.5 * Var2)),
    Z = draw_binary(N = N, prob = (0.5 * Var5)),
    Y = draw_ordered(x = rnorm(N, mean = 0.3*X + 0.2*Z + 0.8*X*Z), breaks = c(1,2,3,4,5,6,7))
)
moderated_dgp_nmvn = fabricate(N = 1000,
    Var1 = rnorm(N, mean = 10),
    Var2 = draw_binary(N = N, prob = 0.5),
    Var3 = draw_ordered(x = rnorm(N, mean = 3.5), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var4 = rnorm(N, mean = 0),
    Var5 = draw_binary(N = N, prob = 0.25),
    Var6 = draw_ordered(x = rnorm(N, mean = 1.75), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    Var7 = rnorm(N, mean = -10),
    Var8 = draw_binary(N = N, prob = 0.75),
    Var9 = draw_ordered(x = rnorm(N, mean = 5.25), breaks = c(1, 2, 3, 4, 5, 6, 7)),
    X = draw_binary(N = N, prob = (0.5 * Var2)),
    Z = draw_binary(N = N, prob = (0.5 * Var5)),
    Y = draw_ordered(x = 0.3*X + 0.2*Z + 0.8*X*Z + rnorm(N), breaks = c(1,2,3,4,5,6,7))
)