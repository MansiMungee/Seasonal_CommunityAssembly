tiff("./../04_Figures/CurrentNew//Figure3.tiff", width = 16, height = 16, res = 300, unit = "cm")
par(mfrow = c(4,4), oma = c(0.1, 0.1, 0.1, 0.1), mar = c(2.5, 2.4, 1, 1), mgp = c(1.1, 0.2, 0), tck = -0.01, las = 1,
    font.main = 2, cex.main = 1.75, font.axis = 1, cex.axis = 1, cex.lab = 1.1, font.lab = 1)


################### ROW- 1 ############################
x1 <- as.numeric(rownames(cdf_summer))[-1]
y1 <- null1.mpd.a.wing_summer$mpd.obs.z[-1]
y2 <- null1.mntd.a.wing_summer$mntd.obs.z[-1]
y3 <- null1.mpd.a.wing_winter$mpd.obs.z
y4 <- null1.mntd.a.wing_winter$mntd.obs.z
#################### PLOT 1
plot(x1, y1, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y1 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 2400, y = 2.75, "Wing", cex = 1.5, bty = "n")
text(x = 550, y = 2.75, "A", cex = 1.5, bty = "n")


#################### PLOT 2
plot(x1, y2, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "", ylab = "SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y2 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "B", cex = 1.5, bty = "n")

#################### PLOT 3
plot(x1, y3, pch = winter_pch, cex = 1.2, col = winter_colors, xlab = "", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y3 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = winter_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(winter_colors[48], alpha.f = 0.25), border = NA)  
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "C", cex = 1.5, bty = "n")

#################### PLOT 4
plot(x1, y4, pch = winter_pch, cex = 1.2, col =  winter_colors, xlab = "", ylab = "SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y4 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col =  winter_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor( winter_colors[48], alpha.f = 0.25), border = NA)  
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "D", cex = 1.5, bty = "n")




























################### ROW- 2 ############################
x1 <- as.numeric(rownames(cdf_summer))[-1]
y1 <- null1.mpd.a.tarsus_summer$mpd.obs.z[-1]
y2 <- null1.mntd.a.tarsus_summer$mntd.obs.z[-1]
y3 <- null1.mpd.a.tarsus_winter$mpd.obs.z
y4 <- null1.mntd.a.tarsus_winter$mntd.obs.z
#################### PLOT 1
plot(x1, y1, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y1 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 2250, y = 2.75, "Tarsus", cex = 1.5, bty = "n")
text(x = 550, y = 2.75, "E", cex = 1.5, bty = "n")

#################### PLOT 2
plot(x1, y2, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "", ylab ="SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y2 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "F", cex = 1.5, bty = "n")

#################### PLOT 3
plot(x1, y3, pch = winter_pch, cex = 1.2, col = winter_colors, xlab = "", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y3 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "G", cex = 1.5, bty = "n")

#################### PLOT 4
plot(x1, y4, pch = winter_pch, cex = 1.2, col =  winter_colors, xlab = "", ylab = "SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y4 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col =  winter_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor( winter_colors[48], alpha.f = 0.25), border = NA)  
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "H", cex = 1.5, bty = "n")



















################### ROW- 3 ############################

x1 <- as.numeric(rownames(cdf_summer))[-1]
y1 <- null1.mpd.a.beak_summer$mpd.obs.z[-1]
y2 <- null1.mntd.a.beak_summer$mntd.obs.z[-1]
y3 <- null1.mpd.a.beak_winter$mpd.obs.z
y4 <- null1.mntd.a.beak_winter$mntd.obs.z
#################### PLOT 1
plot(x1, y1, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y1 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 2400, y = 2.75, "Beak", cex = 1.5, bty = "n")
text(x = 550, y = 2.75, "I", cex = 1.5, bty = "n")

#################### PLOT 2
plot(x1, y2, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "", ylab = "SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y2 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "J", cex = 1.5, bty = "n")

#################### PLOT 3
plot(x1, y3, pch = winter_pch, cex = 1.2, col = winter_colors, xlab = "", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y3 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "K", cex = 1.5, bty = "n")

#################### PLOT 4
plot(x1, y4, pch = winter_pch, cex = 1.2, col =  winter_colors, xlab = "", ylab = "SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y4 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
text(x = 550, y = 2.75, "L", cex = 1.5, bty = "n")























################### ROW- 4 ############################

x1 <- as.numeric(rownames(cdf_summer))[-1]
y1 <- null1.mpd.a.mass_summer$mpd.obs.z[-1]
y2 <- null1.mntd.a.mass_summer$mntd.obs.z[-1]
y3 <- null1.mpd.a.mass_winter$mpd.obs.z
y4 <- null1.mntd.a.mass_winter$mntd.obs.z
#################### PLOT 1
plot(x1, y1, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "elevation (m)", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y1 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
text(x = 2400, y = 2.75, "Mass", cex = 1.5, bty = "n")
text(x = 550, y = 2.75, "M", cex = 1.5, bty = "n")

#################### PLOT 2
plot(x1, y2, pch = summer_pch, cex = 1.2, col = summer_colors, xlab = "elevation (m)", ylab = "SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y2 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col = summer_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor(summer_colors[48], alpha.f = 0.25), border = NA)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
text(x = 550, y = 2.75, "N", cex = 1.5, bty = "n")

#################### PLOT 3
plot(x1, y3, pch = winter_pch, cex = 1.2, col = winter_colors, xlab = "elevation (m)", ylab = "SES(MPD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y3 ~ x1)
summary(model_1)
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
text(x = 550, y = 2.75, "O", cex = 1.5, bty = "n")

#################### PLOT 4
plot(x1, y4, pch = winter_pch, cex = 1.2, col =  winter_colors, xlab = "elevation (m)", ylab ="SES(MNTD)", xaxt = "n", ylim = c(-3,3))
model_1 <- lm(y4 ~ x1)
summary(model_1)
xvals = seq(min(x1), max(x1), length.out = 100)
dfz <- data.frame(x1 = xvals)
CI = predict(model_1, newdata = dfz,interval = "confidence")
CI = as.data.frame.matrix(CI)
abline(model_1, lty = 1, col =  winter_colors[48], lwd = 1.2)
polygon.x1 <- c(dfz$x1, rev(dfz$x1))
polygon.y <- c(CI$lwr, rev(CI$upr))
polygon(x = polygon.x1, y = polygon.y, col = adjustcolor( winter_colors[48], alpha.f = 0.25), border = NA)  
abline(h = 0, lty = 1, lwd = 1, col = "black")
abline(h = -1.96, lty = 2, col = "darkgrey")
abline(h = 1.96, lty = 2, col = "darkgrey")
axis(side = 1, at = seq(500, 3000, by = 750), labels = as.character(seq(500, 3000, by = 750)))
text(x = 550, y = 2.75, "P", cex = 1.5, bty = "n")



dev.off()