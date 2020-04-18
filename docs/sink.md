# Sink
A sink object represents a file where the audio can be written.


| Functionality                   | Function that does that thing                      |
|---------------------------------|----------------------------------------------------|
| constructor                     | `make-sinke`                                       |
| get/set samplerate              | `samplerate` / `(setf samplerate)`                 |
| get/set number of channels      | `number-of-channels` / `(setf number-of-channels)` |
| write samples to a sink         | `write-sink`                                       |
| close source and cleanup memory | `clean`                                            |

There is a simple utility macro defined called `with-source` that handles the creation and the cleanup of a
`sink` object for you.
