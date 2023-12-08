obs_summer_stats <- cbind(obs.mpd.a.multivariate_summer, obs.mntd.a.multivariate_summer, 
                          obs.mpd.a.phylo_summer, obs.mntd.a.phylo_summer)
colnames(obs_summer_stats) <- c("MPD_Functional", "MNTD_Functional", "MPD_Phylo", "MNTD_Phylo")
obs_summer_stats <- as.data.frame.matrix(obs_summer_stats)
obs_summer_stats$Season <- "Summer"
obs_summer_stats <- obs_summer_stats[-1,]
obs_summer_stats$Elevation <- as.numeric(rownames(obs_summer_stats))

obs_winter_stats <- cbind(obs.mpd.a.multivariate_winter, obs.mntd.a.multivariate_winter, 
                          obs.mpd.a.phylo_winter, obs.mntd.a.phylo_winter)
colnames(obs_winter_stats) <- c("MPD_Functional", "MNTD_Functional", "MPD_Phylo", "MNTD_Phylo")
obs_winter_stats <- as.data.frame.matrix(obs_winter_stats)
obs_winter_stats$Season = "Winter"
obs_winter_stats$Elevation <- as.numeric(rownames(obs_winter_stats))


obs_stats <- rbind(obs_summer_stats, obs_winter_stats)
#obs_stats <- melt(setDT(obs_stats), id.vars = c("Elevation","Season"), variable.name = "Statistic")
obs_stats$Elevation <- as.numeric(obs_stats$Elevation)
obs_stats$Season <- as.factor(obs_stats$Season)

tiff("./../04_Figures/CurrentNew/Figure2.tiff", width = 12, height = 12, unit = "cm", res = 300)
par(mfrow = c(2,2), oma = c(0.1, 0.1, 0.1, 0.1), mar = c(3, 3, 0.5, 0.5), mgp = c(1.5, 0.15, 0), tck = -0.01, las = 1,
    font.main = 2, cex.main = 1, font.axis = 1, cex.axis = 0.8, cex.lab = 1, font.lab = 1)

x1 <- obs_summer_stats$Elevation
y1 <- obs_summer_stats$MPD_Functional
y2 <- obs_winter_stats$MPD_Functional
y3 <- obs_summer_stats$MNTD_Functional
y4 <- obs_winter_stats$MNTD_Functional
y5 <- obs_summer_stats$MPD_Phylo
y6 <- obs_winter_stats$MPD_Phylo
y7 <- obs_summer_stats$MNTD_Phylo
y8 <- obs_winter_stats$MNTD_Phylo



##################### Plot 1 ######################
plot(x1, y1, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("Obs(MPD"["F)"])), xaxt = "n", ylim = c(0, 0.4))
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
points(x1, y2, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y2 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = winter_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y = polygon.y,col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)  
legend("bottomleft", c("Summer", "Winter"), pch = c(summer_pch, winter_pch), cex = 1, col = c(summer_colors[48], winter_colors[48]), bty = "n", lty = c(1, 1))
text(x = 550, y = 0.39, "A", cex = 1.5, bty = "n")



#################### PLOT 2
plot(x1, y3, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("Obs(MNTD"["F)"])), xaxt = "n", ylim = c(0,  0.15))
model_1 <- lm(y3 ~ x1)
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
points(x1, y4, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y4 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = winter_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y = polygon.y,col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)
text(x = 550, y = 0.148, "B", cex = 1.5, bty = "n")






#################### PLOT 3
plot(x1, y5, pch = summer_pch, cex = 1, col = summer_colors, xlab = "elevation (m)", ylab = expression(paste("Obs(MPD"["P)"])), xaxt = "n", ylim = c(0, 0.1))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
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
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)  
####
points(x1, y6, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y6 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = winter_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y = polygon.y,col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)  
legend("bottomleft", c("Summer", "Winter"), pch = c(summer_pch, winter_pch), cex = 1, col = c(summer_colors[48], winter_colors[48]), bty = "n", lty = c(1, 1))
text(x = 550, y = 0.097, "C", cex = 1.5, bty = "n")



############################ PLOT 4
plot(x1, y7, pch = summer_pch, cex = 1, col = summer_colors, xlab = "elevation (m)", ylab = expression(paste("Obs(MNTD"["P)"])), xaxt = "n", ylim = c(0,0.03))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y7 ~ x1)
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
points(x1, y8, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y8 ~ x1)
print("MODEL 2")
summary(model_2)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_2, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_2, lty = 1, col = winter_colors[48], lwd = 1)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1,y = polygon.y,col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)
text(x = 550, y = 0.029, "D", cex = 1.5, bty = "n")

dev.off()