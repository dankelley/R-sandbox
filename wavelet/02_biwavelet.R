library("oce")
library("biwavelet")

if (!interactive()) {
    png("02_biwavelet.png",
        width = 7, height = 5, unit = "in", res = 150, pointsize = 10
    )
}

# Test on constructed timeseries similar to BB oscillations, with 2-hour period
# and 1 minute sampling, over a 7 day sampling interval.
hour <- 3600
day <- 24 * hour
period <- 2 * hour
signalInterval <- 1 * day
samplingInterval <- 60 # 60s to mimic one-minute sealevel data
observationInterval <- 7 * day
t <- seq(from = 0, to = observationInterval, by = samplingInterval) # 1-minute samples
set.seed(1234) # make data be uniform across runs
noise <- smooth(rnorm(length(t), sd = 0.01)) # smooth it a bit
A <- 0.05 * exp(-(t - observationInterval / 2)^2 / signalInterval^2)
u0 <- 0.0 # mean current
signal <- A * sin(t * 2 * pi / period)
eta <- u0 + noise + A * sin(t * 2 * pi / period)
# Compute wavelet
U <- biwavelet::wt(cbind(t, eta))

# Plot wavelet
#<> plot(U)
#<> plot(U, ylim = 2^c(10, 17), lwd.sig = 1, xlab = "Time [s]")
#<> abline(h=log2(2*hour))
#<> P <- c(0.75, 1.5, 3, 6, 12, 24)
#<> yat <- log2(P * hour)
#<> axis(side=4, at=yat, labels = P)

par(mfrow = c(3, 1), mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0))

# UL panel: timeseries similar to CL waves
plot(t / day, eta, xlab = "Day", ylab = expression(eta), type = "l")
lines(t / day, A, col = 2)
lines(t / day, -A, col = 2)
mtext("Simulated signal (red is signal envelop, max 0.05)", line = 0.2)

# UR panel: plot from biwavelet package
alpha.coi <- 0.9 # FIXME: what is this?
plot(U,
    xlab = "Time [s]", ylab = "Period [hour]",
    lwd.sig = 1, alpha.coi = alpha.coi
)
abline(h = log2(period), lty = 2) # test: does line go in middle of peak?
mtext("created by plot.biwavelet()", cex = par("cex"))
message("biwavelet: usr=", paste(par("usr"), collapse = " "))

# Bottom panel base plot, mimicking biwavelet for testing
xx <- U$t
yy <- log2(rev(U$period))
yy <- log2(U$period)

# for "wt" scaling; see source for plot.biwavelet
zz <- log2(abs(t(U$power.corr) / U$sigma2))
signif <- t(U$signif)
imagep(xx / day, yy, zz,
    axes = FALSE,
    # using jet to check on biwavelet plot
    xlab = "Day", ylab = "Period [h]", col = oceColorsJet,
    ylim = rev(range(yy)),
    mar = c(3, 3, 1, 1)
)
mtext(expression(log[2] * "(power)"), side = 3, adj = 1, cex = par("cex"))
abline(h = log2(period), lty = 2) # test: does line go in middle of peak?
box()
axis(1)
# Draw y axis as log "by hand"
for (h in c(0.1, 1, 10)) {
    mtext(h, side = 2, at = log2(h * 3600), cex = par("cex"), line = 0.75)
    rug(log2(h * 3600), side = 2, ticksize = -0.06, lwd = par("lwd"))
    rug(log2(h * 3600 * seq(2, 9)), side = 2, ticksize = -0.03, lwd = 0.9 * par("lwd"))
    rug(log2(h * 3600 * seq(2, 9) / 10), side = 2, ticksize = -0.03, lwd = 0.9 * par("lwd"))
}
n <- length(U$t)
bottom <- par("usr")[4]
contour(xx, yy, signif, levels = 1, add = TRUE, drawlabels = FALSE)
X <- c(U$t, rev(U$t))
Y <- c(U$coi, rep(max(U$coi), length(U$coi)))
polygon(X / day, log2(Y), col = "white", border = "white")
box() # redraw, in case polygon erased bottom axis
mtext("base graphics plot.biwavelet()",
    adj = 0,
    cex = par("cex")
)

if (!interactive()) {
    dev.off()
}
