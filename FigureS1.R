tiff("./../04_Figures/CurrentNew/FigureS1.tiff", width = 16, height = 8, res = 300, unit = "cm")
par(mfrow = c(2,4), oma = c(0.1, 0.1, 0.1, 0.1), mar = c(2, 2.4, 1, 0.1), mgp = c(0.85, 0.15, 0), tck = -0.01, las = 1,
    font.main = 2, cex.main = 1, font.axis = 1, cex.axis = 0.8, cex.lab = 1, font.lab = 2)


################### ROW- 1 ############################
x1 <- as.numeric(rownames(cdf_summer))
y1 <- null1.mpd.a.multivariate_summer$mpd.obs.z
y2 <- null1.mpd.multivariate_summer$mpd.obs.z
y3 <- null1.mntd.a.multivariate_summer$mntd.obs.z
y4 <- null1.mntd.multivariate_summer$mntd.obs.z
y5 <- null1.mpd.a.phylo_summer$mpd.obs.z
y6 <- null1.mpd.phylo_summer$mpd.obs.z
y7 <- null1.mntd.a.phylo_summer$mntd.obs.z
y8 <- null1.mntd.phylo_summer$mntd.obs.z


#################### PLOT 1
plot(x1, y1, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("SES(MPD"["F)"])), xaxt = "n", ylim = c(-3.75,  3.75))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
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
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")



#################### PLOT 2
plot(x1, y3, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("SES(MNTD"["F)"])), xaxt = "n", ylim = c(-3.75,  3.75))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y3 ~ x1)
print("MODEL 1")
summary(model_1)
####
points(x1, y4, pch = summer_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y3, pch = summer_pch, lwd = 1, cex = 1, col = summer_colors)
model_2 <- lm(y4 ~ x1)
print("MODEL 2")
summary(model_2)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")


#################### PLOT 3
plot(x1, y5, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("SES(MPD"["P)"])), xaxt = "n", ylim = c(-3.75,  3.75))
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
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")



#################### PLOT 4
plot(x1, y7, pch = summer_pch, cex = 1, col = summer_colors, xlab = "", ylab = expression(paste("SES(MNTD"["P)"])), xaxt = "n", ylim = c(-3.75,  3.75))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y7 ~ x1)
print("MODEL 1")
summary(model_1)
####
points(x1, y8, pch = summer_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y7, pch = summer_pch, lwd = 1, cex = 1, col = summer_colors)
model_2 <- lm(y8 ~ x1)
print("MODEL 2")
summary(model_2)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")








################### ROW- 2 ############################
x1 <- as.numeric(rownames(cdf_winter))
y1 <- null1.mpd.a.multivariate_winter$mpd.obs.z
y2 <- null1.mpd.multivariate_winter$mpd.obs.z
y3 <- null1.mntd.a.multivariate_winter$mntd.obs.z
y4 <- null1.mntd.multivariate_winter$mntd.obs.z
y5 <- null1.mpd.a.phylo_winter$mpd.obs.z
y6 <- null1.mpd.phylo_winter$mpd.obs.z
y7 <- null1.mntd.a.phylo_winter$mntd.obs.z
y8 <- null1.mntd.phylo_winter$mntd.obs.z



#################### PLOT 1
plot(x1, y1, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("SES(MPD"["F)"])), xaxt = "n", ylim = c(-3.75,  3.75))
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
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")



#################### PLOT 2
plot(x1, y3, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("SES(MNTD"["F)"])), xaxt = "n", ylim = c(-3.75,  3.75))
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
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")



#################### PLOT 3
plot(x1, y5, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("SES(MPD"["P)"])), xaxt = "n", ylim = c(-3.75,  3.75))
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
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")




#################### PLOT 4
plot(x1, y7, pch = winter_pch, cex = 1, col = winter_colors, xlab = "elevation (m)", ylab = expression(paste("SES(MNTD"["P)"])), xaxt = "n", ylim = c(-3.75,  3.75))
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
model_1 <- lm(y7 ~ x1)
print("MODEL 1")
summary(model_1)
#grid(ny = 5, col = "lightgray", lty = 2, lwd = 0.4)
####
points(x1, y8, pch = winter_pch, lwd = 1, cex = 1, col = "grey75")
points(x1, y7, pch = winter_pch, lwd = 1, cex = 1, col = winter_colors)
model_2 <- lm(y8 ~ x1)
print("MODEL 2")
summary(model_2)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")


dev.off()









