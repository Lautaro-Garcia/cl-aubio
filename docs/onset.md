# Onset detection

| Functionality                                                          | Function that does that thing                                        |
|------------------------------------------------------------------------|----------------------------------------------------------------------|
| constructor                                                            | `make-onset-detector`                                                |
| get/set the silence threshold                                          | `silence-threshold` / `(setf silence-threshold)`                     |
| get/set the peak picking threshold                                     | `peak-picking-threshold` / `(setf peak-picking-threshold)`           |
| reset the onset detector (forget the data about it's latest detection) | `reset-onset-detector`                                               |
| get/set adaptive whitening                                             | `adaptive-whitening-enabled?`/`(setf adaptive-whitening-enabled?)`   |
| get/set the compression factor                                         | `compression-factor`/`(setf compression-factor)`                     |
| get/set the minimum inter-onset interval                               | `minimum-inter-onset-interval`/`(setf minimum-inter-onset-interval)` |
| get the time of the latest onset detected                              | `time-of-latest-onset-detected`                                      |
| detect the onset of an input                                           | `detect-onset`                                                       |
| cleanup memory                                                         | `clean`                                                              |

In this case, the onset is represented as a number that represents the seconds, milliseconds or samples that passed from the beginning
of the audio until the note was hit.
