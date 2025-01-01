# Altered from example givem by ?signal::filtfilt, but
# that has several comments that are clearly incorrect, as
# noted below.
library(signal)
W <- 0.1
# W=0.1 means a cutoff of 0.1 * Nyquist. Here, the sampling
# rate is 100 Hz, so the Nyquist frequency is 50 Hz. Thus,
# this example has a cutoff of 0.1*50Hz = 5 Hz, not the 10 Hz
# in the original documentation.
bf <- butter(n = 3, W = W, type = "low") # 5 Hz low-pass filter
# The comment in the documentation states that the sample interval
# is 1 second, but it is actually 1/99 second
t <- seq(0, 1, len = 100) # sampling at 100 Hz
t <- seq(0, 1, 0.01) # sampling at 99 Hz
S <- sin(2 * pi * t * 2.3) # signal at 2.3 Hz
N <- 0.25 * rnorm(length(t))
x <- S + N
y <- filtfilt(bf, x)
z <- filter(bf, x)
plot(t, x, type = "l")
lines(t, S, col = 3, lwd = 3)
lines(t, y, col = "red", lwd = 3)
lines(t, z, col = "blue", lwd = 3)
legend("bottomleft",
    lwd = 3,
    legend = c("signal", "data", "filtfilt", "filter"),
    col = c(3, "black", "red", "blue"), bty = "n"
)
