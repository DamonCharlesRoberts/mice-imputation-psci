# Title: Introducing MAR processes in simulated datasets

# Notes:
    #* Description: R script that generates missingness in simulated data
    #* Updated: 2022-05-23
    #* Updated by: dcr

# Setup 
    #* Set seed
set.seed(717)
    #* Modularly load functions
box::use(
    mice = mice[ampute],
    dplyr = dplyr[select, filter]
)

# Introducing MAR
    #* Weights
ID = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var1 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var2 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var3 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var4 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var5 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var6 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var7 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var8 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
Var9 = c(0, 0, 0.072, 0, 0.072, 0, 0, 0, 0, 0, 0.072, 0.072, 0.072)
X = c(0, 0, 0.076, 0, 0.076, 0, 0, 0, 0, 0, 0.076, 0.076, 0.076)
Y = c(0, 0, 0.076, 0, 0.076, 0, 0, 0, 0, 0, 0.076, 0.076, 0.076)
Z = c(0, 0, 0.076, 0, 0.076, 0, 0, 0, 0, 0, 0.076, 0.076, 0.076)
weights = cbind(ID, Var1, Var2, Var3, Var4, Var5, Var6, Var7, Var8, Var9, X, Y, Z)
    #* MAR in simple_dgp simulated dataset
        #** MVN
for(i in seq(from = 0.1, to = 0.9, by = 0.1)){
    x_1 = simple_dgp_mvn |>
        select(X, Y, Z)
    x_2 = simple_dgp_mvn |>
        select(-c(X, Y, Z))
    x_1 = ampute(x_1, mech = 'MAR', prop = i, freq = c(0.33, 0.33, 0.33))
    x_2 = ampute(x_2, mech = 'MAR', prop = i, freq = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
    x = cbind(x_2$amp, x_1$amp)
    #x = produce_NA(simple_dgp_mvn, p_miss = i, mecha = 'MAR', p_obs = 0.2)
    #x = ampute(simple_dgp_mvn, mech = 'MAR', prop = i,freq = c(0, 0, 0.045, 0, 0, 0.045, 0, 0, 0, 0, 0.3, 0.3, 0.3), weights = weights)
    assign(paste0('simple_mvn_mar_', i), x)
}
        #** NMVN
for(i in seq(from = 0.1, to = 0.9, by = 0.1)){
    x_1 = simple_dgp_nmvn |>
        select(X, Y, Z)
    x_2 = simple_dgp_nmvn |>
        select(-c(X, Y, Z))
    x_1 = ampute(x_1, mech = 'MAR', prop = i, freq = c(0.33, 0.33, 0.33))
    x_2 = ampute(x_2, mech = 'MAR', prop = i, freq = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
    x = cbind(x_2$amp, x_1$amp)  
    #x = produce_NA(simple_dgp_nmvn, p_miss = i, mecha = 'MAR', p_obs = 0.2)
    #x = x['Incomp']
    #x = pd$DataFrame(np_array(x))
    #x = ampute(simple_dgp_nmvn, mech = 'MAR', prop = i,freq = c(0, 0, 0.045, 0, 0, 0.045, 0, 0, 0, 0, 0.3, 0.3, 0.3), weights = weights)
    assign(paste0('simple_nmvn_mar_', i), x)
}
    #* MAR in moderated_dgp simulated dataset
        #** MVN
for(i in seq(from = 0.1, to = 0.9, by = 0.1)){
    x_1 = moderated_dgp_mvn |>
        select(X, Y, Z)
    x_2 = moderated_dgp_mvn |>
        select(-c(X, Y, Z))
    x_1 = ampute(x_1, mech = 'MAR', prop = i, freq = c(0.33, 0.33, 0.33))
    x_2 = ampute(x_2, mech = 'MAR', prop = i, freq = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
    x = cbind(x_2$amp, x_1$amp)
    #x = produce_NA(moderated_dgp_mvn, p_miss = i, mecha = 'MAR', p_obs = 0.2)  
    #x = ampute(moderated_dgp_mvn, mech = 'MAR', prop = i, freq = c(0, 0, 0.045, 0, 0, 0.045, 0, 0, 0, 0, 0.3, 0.3, 0.3), weights = weights)
    assign(paste0('moderated_mvn_mar_', i), x) 
}
        #** NMVN
for(i in seq(from = 0.1, to = 0.9, by = 0.1)){
    x_1 = moderated_dgp_nmvn |>
        select(X, Y, Z)
    x_2 = moderated_dgp_nmvn |>
        select(-c(X, Y, Z))
    x_1 = ampute(x_1, mech = 'MAR', prop = i, freq = c(0.33, 0.33, 0.33))
    x_2 = ampute(x_2, mech = 'MAR', prop = i, freq = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
    x = cbind(x_2$amp, x_1$amp)
    #x = produce_NA(moderated_dgp_nmvn, p_miss = i, mecha = 'MAR', p_obs = 0.2)  
    #x = ampute(moderated_dgp_nmvn, mech = 'MAR', prop = i, freq = c(0, 0, 0.045, 0, 0, 0.045, 0, 0, 0, 0, 0.3, 0.3, 0.3), weights = weights)
    assign(paste0('moderated_nmvn_mar_', i), x) 
}