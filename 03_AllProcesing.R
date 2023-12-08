############################## ALL SUMMER PROCESSING ##########################################
############################# OBSERVED MPD #####################################
obs.mpd.a.multivariate_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = dist_tdf_summer)
obs.mpd.multivariate_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = dist_tdf_summer)
obs.mpd.a.phylo_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = dist_pdf_summer)
obs.mpd.phylo_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = dist_pdf_summer)
###################################### OBSERVED MNTD  #####################################
obs.mntd.a.multivariate_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = dist_tdf_summer)
obs.mntd.multivariate_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = dist_tdf_summer)
obs.mntd.a.phylo_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = dist_pdf_summer)
obs.mntd.phylo_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = dist_pdf_summer)
##################################### NULL1 MPD #####################################
null1.mpd.a.multivariate_summer <- ses.mpd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.multivariate_summer <- ses.mpd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mpd.a.phylo_summer <- ses.mpd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.phylo_summer <- ses.mpd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
##################################### NULL1 MNTD #####################################
null1.mntd.a.multivariate_summer <- ses.mntd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.multivariate_summer <- ses.mntd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.phylo_summer <- ses.mntd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.phylo_summer <- ses.mntd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
###################################### NULL2 MPD #####################################
null2.mpd.a.multivariate_summer <- ses.mpd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.multivariate_summer <- ses.mpd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.phylo_summer <- ses.mpd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.phylo_summer <- ses.mpd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
###################################### NULL2 MNTD #####################################
null2.mntd.a.multivariate_summer <- ses.mntd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.multivariate_summer <- ses.mntd(samp = cdf_summer, dis = dist_tdf_summer, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.phylo <- ses.mntd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.phylo <- ses.mntd(samp = cdf_summer, dis = dist_pdf_summer, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
##################################### INDIVIDUAL TRAITS ##############################################
# Residual wing
jnk1 <- as.data.frame(tdf_summer[,1], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[1]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.wing_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.wing_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.wing_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.wing_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.wing_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.wing_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.wing_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.wing_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.wing_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.wing_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.wing_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.wing_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Residual beak
jnk1 <- as.data.frame(tdf_summer[,2], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[2]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.beak_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.beak_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.beak_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.beak_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.beak_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.beak_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.beak_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.beak_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.beak_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.beak_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.beak_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.beak_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Residual tarsus
jnk1 <- as.data.frame(tdf_summer[,3], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[3]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.tarsus_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.tarsus_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.tarsus_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.tarsus_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.tarsus_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.tarsus_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.tarsus_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.tarsus_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.tarsus_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.tarsus_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.tarsus_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.tarsus_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Mass
jnk1 <- as.data.frame(tdf_summer[,4], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[4]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.mass_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.mass_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.mass_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.mass_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.mass_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.mass_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.mass_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.mass_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.mass_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.mass_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.mass_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.mass_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Primary Substrate
jnk1 <- as.data.frame(tdf_summer[,5], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[5]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.pysub_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.pysub_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.pysub_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.pysub_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.pysub_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.pysub_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.pysub_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.pysub_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.pysub_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.pysub_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.pysub_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.pysub_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Foraging Mode
jnk1 <- as.data.frame(tdf_summer[,6], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[6]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.formode_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.formode_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.formode_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.formode_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.formode_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.formode_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.formode_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.formode_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.formode_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.formode_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.formode_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.formode_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Diet
jnk1 <- as.data.frame(tdf_summer[,7], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[7]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.diet_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.diet_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.diet_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.diet_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.diet_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.diet_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.diet_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.diet_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.diet_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.diet_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.diet_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.diet_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# habitat
jnk1 <- as.data.frame(tdf_summer[,7], drop = FALSE)
colnames(jnk1) <- names(tdf_summer)[7]
rownames(jnk1) <- rownames(tdf_summer)
obs.mpd.a.habitat_summer <- obs_mpd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.habitat_summer <- obs_mpd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.habitat_summer <- obs_mntd.a(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.habitat_summer <- obs_mntd(my.sample = cdf_summer, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.habitat_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.habitat_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.habitat_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.habitat_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.habitat_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.habitat_summer <- ses.mpd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.habitat_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.habitat_summer <- ses.mntd(samp = cdf_summer, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
############################################## COMMUNITY WEIGHTED MEANS############################
tdf_summer$Habitat <- as.factor(tdf_summer$Habitat)
tdf_summer$Diet <- as.factor(tdf_summer$Diet)
tdf_summer$ForMode <- as.factor(tdf_summer$ForMode)
tdf_summer$PySub <- as.factor(tdf_summer$PySub)
tdf_summer <- tdf_summer[order(rownames(tdf_summer)),]
cdf_summer <- as.matrix(cdf_summer[,order(names(cdf_summer))])
cwm_summer <- functcomp(x = tdf_summer, a = cdf_summer, CWM.type =  "all")
































############################## ALL WINTER PROCESSING ##########################################
############################# OBSERVED MPD #####################################
obs.mpd.a.multivariate_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = dist_tdf_winter)
obs.mpd.multivariate_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = dist_tdf_winter)
obs.mpd.a.phylo_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = dist_pdf_winter)
obs.mpd.phylo_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = dist_pdf_winter)
###################################### OBSERVED MNTD  #####################################
obs.mntd.a.multivariate_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = dist_tdf_winter)
obs.mntd.multivariate_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = dist_tdf_winter)
obs.mntd.a.phylo_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = dist_pdf_winter)
obs.mntd.phylo_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = dist_pdf_winter)
##################################### NULL1 MPD #####################################
null1.mpd.a.multivariate_winter <- ses.mpd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.multivariate_winter <- ses.mpd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mpd.a.phylo_winter <- ses.mpd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.phylo_winter <- ses.mpd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
##################################### NULL1 MNTD #####################################
null1.mntd.a.multivariate_winter <- ses.mntd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.multivariate_winter <- ses.mntd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.phylo_winter <- ses.mntd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.phylo_winter <- ses.mntd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
###################################### NULL2 MPD #####################################
null2.mpd.a.multivariate_winter <- ses.mpd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.multivariate_winter <- ses.mpd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.phylo_winter <- ses.mpd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.phylo_winter <- ses.mpd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
###################################### NULL2 MNTD #####################################
null2.mntd.a.multivariate_winter <- ses.mntd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.multivariate_winter <- ses.mntd(samp = cdf_winter, dis = dist_tdf_winter, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.phylo <- ses.mntd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.phylo <- ses.mntd(samp = cdf_winter, dis = dist_pdf_winter, null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
##################################### INDIVIDUAL TRAITS ##############################################
# Residual wing
jnk1 <- as.data.frame(tdf_winter[,1], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[1]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.wing_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.wing_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.wing_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.wing_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.wing_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.wing_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.wing_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.wing_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.wing_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.wing_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.wing_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.wing_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Residual beak
jnk1 <- as.data.frame(tdf_winter[,2], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[2]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.beak_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.beak_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.beak_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.beak_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.beak_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.beak_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.beak_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.beak_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.beak_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.beak_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.beak_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.beak_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Residual tarsus
jnk1 <- as.data.frame(tdf_winter[,3], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[3]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.tarsus_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.tarsus_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.tarsus_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.tarsus_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.tarsus_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.tarsus_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.tarsus_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.tarsus_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.tarsus_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.tarsus_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.tarsus_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.tarsus_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Mass
jnk1 <- as.data.frame(tdf_winter[,4], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[4]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.mass_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.mass_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.mass_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.mass_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.mass_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.mass_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.mass_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.mass_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.mass_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.mass_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.mass_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.mass_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Primary Substrate
jnk1 <- as.data.frame(tdf_winter[,5], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[5]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.pysub_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.pysub_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.pysub_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.pysub_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.pysub_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.pysub_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.pysub_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.pysub_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.pysub_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.pysub_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.pysub_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.pysub_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Foraging Mode
jnk1 <- as.data.frame(tdf_winter[,6], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[6]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.formode_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.formode_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.formode_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.formode_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.formode_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.formode_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.formode_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.formode_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.formode_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.formode_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.formode_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.formode_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# Diet
jnk1 <- as.data.frame(tdf_winter[,7], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[7]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.diet_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.diet_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.diet_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.diet_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.diet_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.diet_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.diet_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.diet_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.diet_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.diet_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.diet_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.diet_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
# habitat
jnk1 <- as.data.frame(tdf_winter[,7], drop = FALSE)
colnames(jnk1) <- names(tdf_winter)[7]
rownames(jnk1) <- rownames(tdf_winter)
obs.mpd.a.habitat_winter <- obs_mpd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mpd.habitat_winter <- obs_mpd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.a.habitat_winter <- obs_mntd.a(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
obs.mntd.habitat_winter <- obs_mntd(my.sample = cdf_winter, dist.mat = as.matrix(gowdis(jnk1, ord = "podani")))
null1.mpd.a.habitat_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mpd.habitat_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null1.mntd.a.habitat_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null1.mntd.habitat_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "frequency", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mpd.a.habitat_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mpd.habitat_winter <- ses.mpd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
null2.mntd.a.habitat_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = TRUE, runs = 999, iterations = 1000)
null2.mntd.habitat_winter <- ses.mntd(samp = cdf_winter, dis = as.matrix(gowdis(jnk1, ord = "podani")), null.model = "independentswap", abundance.weighted = FALSE, runs = 999, iterations = 1000)
############################################## COMMUNITY WEIGHTED MEANS############################
tdf_winter$Habitat <- as.factor(tdf_winter$Habitat)
tdf_winter$Diet <- as.factor(tdf_winter$Diet)
tdf_winter$ForMode <- as.factor(tdf_winter$ForMode)
tdf_winter$PySub <- as.factor(tdf_winter$PySub)
tdf_winter <- tdf_winter[order(rownames(tdf_winter)),]
cdf_winter <- as.matrix(cdf_winter[,order(names(cdf_winter))])
cwm_winter <- functcomp(x = tdf_winter, a = cdf_winter, CWM.type =  "all")
