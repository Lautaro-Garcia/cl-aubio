# Tempo detection

| Functionality                       | Function that does that thing                              |
|-------------------------------------|------------------------------------------------------------|
| constructor                         | `make-note-detector`                                       |
| get/set the silence threshold       | `silence-threshold`/`(setf silence-threshold)`             |
| get/set the peak picking threshold  | `peak-picking-threshold` / `(setf peak-picking-threshold)` |

This detector returns a `tempo` object that has the data of:
* The bpm of the tempo
* The last beat position
* The confidence percentage
