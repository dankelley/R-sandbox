# explore butterworth filter with white noise
library(signal)

W <- 0.1
bf <- butter(n = 3, W = W, type = "low") # 5 Hz low-pass filter
x <- rnorm(1e4)
xf <- filtfilt(bf, x)
par(mfcol = c(3, 2))
X <- spectrum(x, plot = FALSE, spans = c(3, 5))
XF <- spectrum(xf, plot = FALSE, spans = c(3, 5))
plot(X$freq, X$spec, type = "l")
plot(XF$freq, XF$spec, type = "l")
N <- 0.5 # Nyquist
abline(v = W * N, col = 2)
plot(log10(X$freq), log10(X$spec), type = "l")
plot(log10(XF$freq), log10(XF$spec), type = "l")
abline(v = log10(W * N), col = 2)
plot(X$freq, XF$spec / X$spec, type = "l")
abline(v = W * N, col = 2)
abline(h = 1 / 4, col = 2) # 1/4 power at cutoff
plot(log10(X$freq), log10(XF$spec / X$spec), type = "l")
abline(v = log10(W * N), col = 2)
abline(h = log10(1 / 4), col = 2)
