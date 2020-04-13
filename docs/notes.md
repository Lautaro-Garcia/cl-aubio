# Note detection

This API could be considered as the sum of onset and the pitch API, maybe a little bit simplified (you can't change the parameters of the algorithms as much)

| Functionality                            | Function that does that thing                                        |
|------------------------------------------|----------------------------------------------------------------------|
| constructor                              | `make-note-detector`                                                 |
| get/set the silence threshold            | `silence-threshold`/`(setf silence-threshold)`                       |
| get/set the release drop in dB           | `release-drop`/`(setf release-drop)`                                 |
| get/set the minimum inter-onset interval | `minimum-inter-onset-interval`/`(setf minimum-inter-onset-interval)` |
| detect the note of an input              | `detect-note`                                                        |
| cleanup memory                           | `clean`                                                              |


A note has:
* The MIDI value of the note
* It's velocity
