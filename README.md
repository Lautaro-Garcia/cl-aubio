# Aubio bindings for Common Lisp

This library provides the c-bindings for the [aubio audio library](https://aubio.org/).


## Pre-requisites
Well, you have to have `libaubio` installed in your computer. You should probably check out the [aubio download page](https://aubio.org/download)
It's worht noting that in this part, you could choose to install other libraries (such as [libsndfile](http://www.mega-nerd.com/libsndfile/) or
[FFTW](http://www.fftw.org/)) and build aubio with them so it will try to leverage some of the work to those libraries. It is totallly optional.


## How are the packages layed out?
This project exposes two packages (each one in its own system):

### `aubio` (or by it's nickname, `cl-aubio`)
This is the one that you should use. It contains all the lipsy translations and utility wrappers for the actual c functions/structures.

### `aubio/bindings`
This package contains the raw bindings, with no name processing or whatsoever. You should only use this package if I was super lazy and
didn't provide a lispy binding for some function.


## Aubio objects
Before explaining each object by itself, I want to tell you that the guys and gals in charge of the aubio project did an extremely good
job at mantaining its API object oriented. And what I am refering to is that
* every object can be created with `new_aubio_<name-of-the-object>`
* and can be destroyed with `del_aubio_<name-of-the-object>`
* and most of them have a `aubio_<name-of-the-object>_do` (which seems to respond of a method object of sorts)

So, in my Lisp translation I'm trying to mantain that convention in this form:
* every object will be created with a `make-<name-of-the-object>` function
* every object will be deleted with a `clean` generic function
* I'll try to make a generic function for the `do` part, but only if I find that it makes sense to have a polymorphic of dealing with some objects.
  In other cases, I probably will generate functions with more descriptive names of what the object is actually doing

### Source
A source object represents a source of audio.

| Functionality                          | Function that does that thing |
|----------------------------------------|-------------------------------|
| constructor                            | `make-source`                 |
| get samplerate                         | `samplerate`                  |
| get number of channels                 | `channels`                    |
| get duration (in frames) of the source | `duration`                    |
| seek source to a particular position   | `seek`                        |
| read source                            | `read-source`                 |
| close source and cleanup memory        | `clean`                       |

A call to `read-source` will return two values:
1. The array of samples in the representation of an aubio `float-vector` or `float-matrix` (it depends whether you specified the input
   as monophonic or polyphonic via the `monophonic` keyword argument) to serve as an input to other `cl-aubio` functions, or as a simple
   lisp array (you can control this with the key argument `:as-lisp-vector`)
2. The number of samples actually read

#### Utility macros
* `with-source`: It creates an `source` object and cleans it after it is used
* `do-source`: calls the body for every sample vector returned by a `read-source` call. I thought it would be a common enough use case to include it here.


### Pitch detection
A pitch object represents a pitch detection algorithm

| Functionality                                | Function that does that thing                          |
|----------------------------------------------|--------------------------------------------------------|
| constructor                                  | `make-pitch`                                           |
| get/set the tolerance threshold              | `tolerance` / `(setf tolerance)`                       |
| get/set the silence threshold                | `silence-threshold` / `(setf silence-theshold)`        |
| get the confidence of a particular detection | `confidence`                                           |
| get/set the pitch detection unit             | `pitch-detection-unit` / `(setf pitch-detection-unit)` |
| detect the pitch of an input                 | `detect-pitch`                                         |
| cleanup memory                               | `clean`                                                |


### Aubio data structures
Aubio defines its own data structures for some particular cases. The ones defined here are:
* `fvec_t` is called `float-vector`
* `fmat_t` is called `float-matrix`

You can transform any of this structures to a lisp one by using the function `aubio-to-lisp`
