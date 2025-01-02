# Contents of the `complex_demodulation` directory

* `complex_demodulation_01.R` simple example of complex demodulation, using
  `oce::lowpass()` for the filtering (which has artifacts at the start and end
  of the time series). Note that two PNG files are being made, because I
  discovered an odd feature in plotting semi-transparent lines that I may want
  to reduce to a bug report.

* `complex_demodulation_02.R` as `complex_demodulation_01.R` but use
  Butterworth filter for the low-pass operation.

* `README.md` the present file.
