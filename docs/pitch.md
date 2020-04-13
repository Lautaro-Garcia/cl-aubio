# Pitch detection

| Functionality                                | Function that does that thing                          |
|----------------------------------------------|--------------------------------------------------------|
| constructor                                  | `make-pitch-detector`                                  |
| get/set the tolerance threshold              | `tolerance` / `(setf tolerance)`                       |
| get/set the silence threshold                | `silence-threshold` / `(setf silence-theshold)`        |
| get the confidence of a particular detection | `confidence`                                           |
| get/set the pitch detection unit             | `pitch-detection-unit` / `(setf pitch-detection-unit)` |
| detect the pitch of an input                 | `detect-pitch`                                         |
| cleanup memory                               | `clean`                                                |


A pitch detector object detects pitches, that contain
* The value of the pitch
* A confidence percentage  that this value is accurrate
* The unit of the pitch
