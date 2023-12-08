################## ABUNDANCE BASED STATS ############################
obs_summer_stats_abund <- cbind(obs.mpd.a.multivariate_summer, obs.mntd.a.multivariate_summer, 
                          obs.mpd.a.phylo_summer, obs.mntd.a.phylo_summer)
colnames(obs_summer_stats_abund) <- c("MPD_Functional", "MNTD_Functional", "MPD_Phylo", "MNTD_Phylo")
obs_summer_stats_abund <- as.data.frame.matrix(obs_summer_stats_abund)
obs_summer_stats_abund$Season <- "Summer"
obs_summer_stats_abund <- obs_summer_stats_abund[-1,]
obs_summer_stats_abund$Elevation <- as.numeric(rownames(obs_summer_stats_abund))

obs_winter_stats_abund <- cbind(obs.mpd.a.multivariate_winter, obs.mntd.a.multivariate_winter, 
                          obs.mpd.a.phylo_winter, obs.mntd.a.phylo_winter)
colnames(obs_winter_stats_abund) <- c("MPD_Functional", "MNTD_Functional", "MPD_Phylo", "MNTD_Phylo")
obs_winter_stats_abund <- as.data.frame.matrix(obs_winter_stats_abund)
obs_winter_stats_abund$Season = "Winter"
obs_winter_stats_abund$Elevation <- as.numeric(rownames(obs_winter_stats_abund))


obs_stats_abund <- rbind(obs_summer_stats_abund, obs_winter_stats_abund)
obs_stats_abund$Elevation <- as.numeric(obs_stats_abund$Elevation)
obs_stats_abund$Season <- as.factor(obs_stats_abund$Season)


####################################### INCIDENCE BASED STATS
obs_summer_stats_inci <- cbind(obs.mpd.multivariate_summer, obs.mntd.multivariate_summer, 
                                obs.mpd.phylo_summer, obs.mntd.phylo_summer)
colnames(obs_summer_stats_inci) <- c("MPD_Functional", "MNTD_Functional", "MPD_Phylo", "MNTD_Phylo")
obs_summer_stats_inci <- as.data.frame.matrix(obs_summer_stats_inci)
obs_summer_stats_inci$Season <- "Summer"
obs_summer_stats_inci <- obs_summer_stats_inci[-1,]
obs_summer_stats_inci$Elevation <- as.numeric(rownames(obs_summer_stats_inci))

obs_winter_stats_inci <- cbind(obs.mpd.multivariate_winter, obs.mntd.multivariate_winter, 
                                obs.mpd.phylo_winter, obs.mntd.phylo_winter)
colnames(obs_winter_stats_inci) <- c("MPD_Functional", "MNTD_Functional", "MPD_Phylo", "MNTD_Phylo")
obs_winter_stats_inci <- as.data.frame.matrix(obs_winter_stats_inci)
obs_winter_stats_inci$Season = "Winter"
obs_winter_stats_inci$Elevation <- as.numeric(rownames(obs_winter_stats_inci))


obs_stats_inci <- rbind(obs_summer_stats_inci, obs_winter_stats_inci)
obs_stats_inci$Elevation <- as.numeric(obs_stats_inci$Elevation)
obs_stats_inci$Season <- as.factor(obs_stats_inci$Season)


tiff("./../04_Figures/CurrentNew/FigureS2.tiff", width = 16, height = 8, res = 300, unit = "cm")
par(mfrow = c(2,4), oma = c(0.1, 0.1, 0.1, 0.1), mar = c(2.5, 2.7, 1, 0.1), mgp = c(1.3, 0.15, 0), tck = -0.01, las = 1,
    font.main = 2, cex.main = 1, font.axis = 1, cex.axis = 0.8, cex.lab = 1, font.lab = 2)



x1 <- obs_summer_stats_abund$Elevation

y1 <- obs_summer_stats_abund$MPD_Functional
y2 <- obs_summer_stats_inci$MPD_Functional

y3 <- obs_summer_stats_abund$MNTD_Functional
y4 <- obs_summer_stats_inci$MNTD_Functional

y5 <- obs_summer_stats_abund$MPD_Phylo
y6 <- obs_summer_stats_inci$MPD_Phylo

y7 <- obs_summer_stats_abund$MNTD_Phylo
y8 <- obs_summer_stats_inci$MNTD_Phylo

#################### PLOT 1
plot(x1, y1, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("Obs(MPD"["F)"])), xaxt = "n", ylim = c(0.2,  0.45))
model_1 <- lm(y1 ~ x1)
print("MODEL 1")
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)  
####
points(x1, y2, pch = summer_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y1, pch = summer_pch, lwd = 1, cex = 1, col = summer_colors)
model_2 <- lm(y2 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = "grey75", lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y = polygon.y,col = adjustcolor("grey75", alpha.f = 0.25), border = NA)  
legend("topright", c("Abund.", "Inci."), pch = c(16, 16), cex = 0.9, col = c(summer_colors[48], "grey75"), bty = "n", lty = c(1, 1))




#################### PLOT 2
plot(x1, y3, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("Obs(MNTD"["F)"])), xaxt = "n", ylim = c(0.035,  0.1))
model_1 <- lm(y3 ~ x1)
print("MODEL 1")
summary(model_1)
####
points(x1, y4, pch = summer_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y3, pch = summer_pch, lwd = 1, cex = 1, col = summer_colors)
model_2 <- lm(y4 ~ x1)
print("MODEL 2")
summary(model_2)


#################### PLOT 3
plot(x1, y5, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("Obs(MPD"["P)"])), xaxt = "n", ylim = c(0.02,  0.12))
model_1 <- lm(y5 ~ x1)
print("MODEL 1")
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y=polygon.y,col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)  
####
points(x1, y6, pch = summer_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y5, pch = summer_pch, lwd = 1, cex = 1, col = summer_colors)
model_2 <- lm(y6 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = "grey75", lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y=polygon.y,col = adjustcolor("grey75", alpha.f = 0.25), border = NA)  




#################### PLOT 4
plot(x1, y7, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("Obs(MNTD"["P)"])), xaxt = "n", ylim = c(0.003,  0.02))
model_1 <- lm(y7 ~ x1)
print("MODEL 1")
summary(model_1)
####
points(x1, y8, pch = summer_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y7, pch = summer_pch, lwd = 1, cex = 1, col = summer_colors)
model_2 <- lm(y8 ~ x1)
print("MODEL 2")
summary(model_2)








################### ROW- 2 ############################
x1 <- obs_summer_stats_abund$Elevation

y1 <- obs_winter_stats_abund$MPD_Functional
y2 <- obs_winter_stats_inci$MPD_Functional

y3 <- obs_winter_stats_abund$MNTD_Functional
y4 <- obs_winter_stats_inci$MNTD_Functional

y5 <- obs_winter_stats_abund$MPD_Phylo
y6 <- obs_winter_stats_inci$MPD_Phylo

y7 <- obs_winter_stats_abund$MNTD_Phylo
y8 <- obs_winter_stats_inci$MNTD_Phylo



#################### PLOT 1
plot(x1, y1, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("Obs(MPD"["F)"])), xaxt = "n", ylim = c(0.1,  0.5))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y1 ~ x1)
print("MODEL 1")
summary(model_1)

####
points(x1, y2, pch = winter_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y1, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y2 ~ x1)
print("MODEL 2")
summary(model_2)
legend("topright", c("Abund.", "Inci."), pch = c(winter_pch, 16), cex = 0.9, col = c(winter_colors[48], "grey75"), bty = "n", lty = c(1, 1))



#################### PLOT 2
plot(x1, y3, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("Obs(MNTD"["F)"])), xaxt = "n", ylim = c(0.02,  0.17))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y3 ~ x1)
print("MODEL 1")
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = winter_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)  
####
points(x1, y4, pch = winter_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y3, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y4 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = "grey75", lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor("grey75", alpha.f = 0.25), border = NA)  


#################### PLOT 3
plot(x1, y5, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("Obs(MPD"["P)"])), xaxt = "n", ylim = c(0.03,  0.15))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y5 ~ x1)
print("MODEL 1")
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = winter_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y=polygon.y,col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)  
####
points(x1, y6, pch = winter_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y5, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y6 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = "grey75", lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y=polygon.y,col = adjustcolor("grey75", alpha.f = 0.25), border = NA)  




#################### PLOT 4
plot(x1, y7, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("Obs(MNTD"["P)"])), xaxt = "n", ylim = c(0.003,  0.04))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y7 ~ x1)
print("MODEL 1")
summary(model_1)
####
points(x1, y8, pch = winter_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y7, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y8 ~ x1)
print("MODEL 2")
summary(model_2)


dev.off()





