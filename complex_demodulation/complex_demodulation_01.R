library(oce)
showComplexParts <- FALSE
alpha <- 0.25 # for transparent colour of the 'A' signal (and, later, its inferred value)
if (!interactive()) {
    png("complex_demodulation_%d.png", unit = "in", width = 7, height = 5, res = 300)
}
for (dt in c(1, 10) / 60) { # two sampling intervals (in hours)
    t <- seq(0, 24 - dt, dt) # simulate 1 day
    period <- 2.1
    focus <- 2 # looking for things near this period
    A <- exp(-(t - mean(t))^2 / 4^2)
    N <- rnorm(length(t), sd = 0.05)
    x <- A * sin(2 * pi * t / period + pi / 4) + N
    par(mfrow = c(2 + showComplexParts, 1))
    par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0))
    plot(t, A, ylim = range(x), col = rgb(1, 0, 0, alpha = alpha), lwd = 6, type = "l")
    lines(t, x)
    # lines(t, A, col = rgb(1, 0, 0, alpha=alpha), lwd = 6)
    s <- exp(2i * pi * t / focus)
    y <- s * x
    yr <- Re(y)
    yi <- Im(y)
    YR <- lowpass(yr, n = 1 + focus / dt)
    YI <- lowpass(yi, n = 1 + focus / dt)
    if (showComplexParts) {
        plot(t, YR, type = "l", ylim = range(c(YR, YI)))
        lines(t, YI, col = 4)
    }
    amp <- 2 * sqrt(YR^2 + YI^2)
    plot(t, amp, type = "l")
    lines(t, A, col = rgb(1, 0, 0, alpha = alpha), lwd = 6)
}
if (!interactive()) {
    dev.off()
}
