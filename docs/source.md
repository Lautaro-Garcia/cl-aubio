# Source
A source object represents a source of audio.

| Functionality                          | Function that does that thing |
|----------------------------------------|-------------------------------|
| constructor                            | `make-source`                 |
| get samplerate                         | `samplerate`                  |
| get number of channels                 | `number-of-channels`          |
| get duration (in frames) of the source | `duration`                    |
| seek source to a particular position   | `seek`                        |
| read source                            | `read-source`                 |
| close source and cleanup memory        | `clean`                       |

A call to `read-source` will return two values:
1. The array of samples in the representation of an aubio `float-vector` or `float-matrix` (it depends whether you specified the input
   as monophonic or polyphonic via the `monophonic` keyword argument) to serve as an input to other `cl-aubio` functions, or as a simple
   lisp array (you can control this with the key argument `:as-lisp-vector`)
2. The number of samples actually read

## Utility macros
* `with-source`: It creates an `source` object and cleans it after it is used
* `do-source`: calls the body for every sample vector returned by a `read-source` call. I thought it would be a common enough use case to include it here.
