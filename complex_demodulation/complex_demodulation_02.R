library(oce)
library(signal)
W <- 5 / 60 # 5 cpm / 60 cpm
bf <- butter(n = 3, W = W, type = "low") # 5 Hz low-pass filter
showComplexParts <- FALSE
alpha <- 0.25 # for transparent colour of the 'A' signal (and, later, its inferred value)
if (!interactive()) {
    png("complex_demodulation_02-%d.png", type = "cairo", unit = "in", width = 7, height = 5, res = 300)
    #png("complex_demodulation_02-%d.png", unit = "in", width = 7, height = 5, res = 300)
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
    mtext("Using Butterworth filter with W=", W, " for dt=", t[2] - t[1], line = 0.2)
    # lines(t, A, col = rgb(1, 0, 0, alpha=alpha), lwd = 6)
    s <- exp(2i * pi * t / focus)
    y <- s * x
    yr <- Re(y)
    yi <- Im(y)
    #YR <- lowpass(yr, n = 1 + focus / dt)
    #YI <- lowpass(yi, n = 1 + focus / dt)
    fcutoff <- 1 # think about this
    fnyquist <- 1 / (t[2] - t[1])
    W <- fcutoff / fnyquist
    message("W=", W, " given fcutoff=", fcutoff, " and fnyquist=", fnyquist)
    filter <- butter(n = 3, W = W, type = "low")
    YR <- filtfilt(filter, yr)
    YI <- filtfilt(filter, yi)
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
