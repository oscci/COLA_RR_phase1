library("SimCorrMix")

#the following means and SD will have the biggest impact on the simulation  as they will set how close the "humps" of the bimodal distribution are from each other. 
mix_pis <- list(c(0.2, 0.8), c(0.3, 0.7),c(0.2,0.8)) #Mixing proportions for each mixture (three pairs of probabilities)
mix_mus <- list(c(-10, 1), c(0, 10),c(-5,5)) # MEans of the three mixture distributions (three pairs, each pair is the mean for each sub distribution in the mixture)
mix_sigmas <- list(c(sqrt(2), sqrt(3)), c(sqrt(1), sqrt(2)),c(sqrt(2), sqrt(3))) # SDs of the three mixture distributions (three pairs, each pair is the SD for each sub distribution in the mixture)
mix_skews <- list(c(0, 0), c(0, 0), c(0, 0))#Skew for the distributions (left as no skew so will look gaussian)
mix_skurts <- list(c(0, 0), c(0, 0), c(0, 0))#Kurtosis for the distributions (left as no kurtosis so will look gaussian)
mix_fifths <- list(c(0, 0), c(0, 0), c(0, 0))# Ignore this as it relates to the method used
mix_sixths <- list(c(0, 0), c(0, 0), c(0, 0))# Ignore this as it relates to the method used

#calculate the moments of the distributions.
N1stcum <- calc_mixmoments(mix_pis[[1]], mix_mus[[1]], mix_sigmas[[1]], mix_skews[[1]], mix_skurts[[1]], mix_fifths[[1]], mix_sixths[[1]]) 
N2stcum <- calc_mixmoments(mix_pis[[2]], mix_mus[[2]], mix_sigmas[[2]], mix_skews[[2]], mix_skurts[[2]], mix_fifths[[2]], mix_sixths[[2]])
N3stcum <- calc_mixmoments(mix_pis[[3]], mix_mus[[3]], mix_sigmas[[3]], mix_skews[[3]], mix_skurts[[3]], mix_fifths[[3]], mix_sixths[[3]])

# get means and vars from the moments
means <- c(N1stcum[1], N2stcum[1], N3stcum[1])
vars <- c(N1stcum[2]^2, N2stcum[2]^2, N3stcum[2]^2)

# set the correlations between the components of each mixture distribution and the correlation between them. Currently all 0.7 which is not sensisble as they will vary
Rey <- matrix(0.7, 6, 6)
diag(Rey) <- 1
rownames(Rey) <- colnames(Rey) <- c("M1_1", "M1_2", "M2_1", "M2_2",
                                    "M3_1", "M3_2")

# set correlation between components of the same mixture variable to 0
Rey["M1_1", "M1_2"] <- Rey["M1_2", "M1_1"] <- 0
Rey["M2_1", "M2_2"] <- Rey["M2_2", "M2_1"] <- 0
Rey["M3_1", "M3_2"] <- Rey["M3_2", "M3_1"] <- 0
  
seed <- 184
Sim1 <- corrvar(n = 400, k_cat = 0, k_cont = 0, k_mix = 3, k_pois = 0,k_nb = 0, method = "Polynomial", means = means, vars = vars, mix_pis = mix_pis, mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews, mix_skurts = mix_skurts, mix_fifths = mix_fifths, mix_sixths = mix_sixths, rho = Rey, seed = seed, use.nearPD=FALSE)

plot(Sim1$Y_mix[,1:2]) #plot first two mixture variables against each other
plot(Sim1$Y_mix[,2:3])
plot(Sim1$Y_mix[,c(1,3)])