# Contents of the `complex_demodulation` directory

* `com_demod_01_lowpass.R` simple example of complex demodulation, using
  `oce::lowpass()` for the filtering (which has artifacts at the start and end
  of the time series). Note that two PNG files are being made, because I
  discovered an odd feature in plotting semi-transparent lines that I may want
  to reduce to a bug report.

* `com_demod_02_butterworth.R` as `comp_demod_lowpass.R` but use a Butterworth
  filter for the low-pass operation.  This has *much* better endpoint
  behaviour.

* `README.md` the present file.
