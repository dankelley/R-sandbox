library("oce")
library("biwavelet")

if (!interactive()) {
    png("01_biwavelet.png",
        width = 7, height = 5, unit = "in", res = 150, pointsize = 10
    )
}

# Test on constructed timeseries similar to CL waves
t <- seq(from = 0, to = 365 * 24)
set.seed(1234) # make data be uniform across runs
noise <- smooth(rnorm(length(t), sd = 0.015)) # smooth it a bit
A <- 0.005 + 0.05 * exp(-(t - 200 * 24)^2 / (50 * 24)^2) # amplitude varies with time
period <- 512 # very close to 21 day
u0 <- 0.1 # mean current
signal <- A * sin(t * 2 * pi / period)
u <- u0 + noise + A * sin(t * 2 * pi / period)

# Compute wavelet
U <- biwavelet::wt(cbind(t, u))

par(mfrow = c(2, 2), mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0))

# UL panel: timeseries similar to CL waves
plot(t, u, type = "l")

# UR panel: plot from biwavelet package
alpha.coi <- 0.9 # FIXME: what is this?
plot(U,
    xlab = "Hour", ylab = "Period [hour]",
    lwd.sig = 1, alpha.coi = alpha.coi
)
abline(h = log2(period), lty = 2) # test: does line go in middle of peak?
mtext("created by plot.biwavelet()")
message("biwavelet: usr=", paste(par("usr"), collapse = " "))

# LL panel base plot, mimicking biwavelet for testing
xx <- U$t
yy <- log2(rev(U$period))
yy <- log2(U$period)

# Note: we need to transpose the matrices. We are only using power.corr and
# signif here, though.

# for "wt" scaling; see source for plot.biwavelet
zz <- log2(abs(t(U$power.corr) / U$sigma2))
signif <- t(U$signif)
imagep(xx, yy, zz,
    axes = FALSE, zlab = "Log10(power)",
    xlab = "Time [h]", ylab = "Period [h]", col = oceColorsJet,
    ylim = rev(range(yy))
)
abline(h = log2(period), lty = 2) # test: does line go in middle of peak?
box()
axis(1)
periodLabels <- 2^seq.int(floor(log2(min(U$period))), ceiling(log2(max(U$period))))
axis(2, at = log2(periodLabels), labels = periodLabels)
n <- length(U$t)
bottom <- par("usr")[4]
contour(xx, yy, signif, levels = 1, add = TRUE, drawlabels = FALSE)
X <- c(U$t, rev(U$t))
Y <- c(U$coi, rep(max(U$coi), length(U$coi)))
polygon(X, log2(Y), col = "white")
box() # the polygon erases the bottom axis line
message("FIXME: debugging note: usr=", paste(par("usr"), collapse = " "))
mtext("base graphics plot.biwavelet()",
    adj = 0,
    cex = par("cex")
)

# LR panel as LL but eliminate sub-daily periods
xx <- U$t
yy <- log2(rev(U$period))
yy <- log2(U$period)
# Note: we need to transpose the matrices. We are only using power.corr and
# signif here, though ... see str(U).
zz <- log2(abs(t(U$power.corr) / U$sigma2)) # for "wt" scaling; see source for plot.biwavelet
signif <- t(U$signif)
imagep(xx, yy, zz,
    axes = FALSE, zlab = "Log10(power)", xlab = "Time [h]", ylab = "Period [h]", col = oceColorsJet,
    ylim = c(max(yy), log2(24))
)
abline(h = log2(period), lty = 2) # test: does line go in middle of peak?
box()
axis(1)
periodLabels <- 2^seq.int(floor(log2(min(U$period))), ceiling(log2(max(U$period))))
axis(2, at = log2(periodLabels), labels = periodLabels)
n <- length(U$t)
bottom <- par("usr")[4]
# For next, see 'tol' in edit(plot.biwavelet)
# FIXME: how can signif be > 1?
contour(xx, yy, signif, levels = 1, add = TRUE, drawlabels = FALSE)
X <- c(U$t, rev(U$t))
Y <- c(U$coi, rep(max(U$coi), length(U$coi)))
polygon(X, log2(Y), col = "white") # gray(0, alpha = 0.2))
box() # the polygon erases the bottom axis line
message("FIXME: debugging note: usr=", paste(par("usr"), collapse = " "))
mtext("base graphics (focus on tau>24h)", adj = 0, cex = par("cex"))

if (!interactive()) {
    dev.off()
}
