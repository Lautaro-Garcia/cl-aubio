# Filterbank

General-purpose spectral filterbank object.

| Functionality               | Function that does that thing                              |
|-----------------------------|------------------------------------------------------------|
| constructor                 | `make-filterbank`                                          |
| clean                       | `clean`                                                    |
| get/set filter coefficients | `filter-coefficients` / `(setf filter-coefficients)`       |
| get/set normalization       | `normalization-enabled?` / `(setf normalization-enabled?)` |
| get/set power parameter     | `power-parameter` / `(setf power-parameter)`               |
| set triangle bands          | `(setf tirangle-bands)`                                    |
