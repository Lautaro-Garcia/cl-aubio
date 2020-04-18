# Aubio bindings for Common Lisp

This library provides the c-bindings for the [aubio audio library](https://aubio.org/).


## Pre-requisites
Well, you have to have `libaubio` installed in your computer. You should probably check out the [aubio download page](https://aubio.org/download)
It's worth noting that in this part, you could choose to install other libraries (such as [libsndfile](http://www.mega-nerd.com/libsndfile/) or
[FFTW](http://www.fftw.org/)) and build aubio with them so it will try to leverage some of the work to those libraries. It is totallly optional.


## How are the packages layed out?
This project exposes two packages (each one in its own system):

### `aubio` (or by it's nickname, `cl-aubio`)
This is the one that you should use. It contains all the lipsy translations and utility wrappers for the actual c functions/structures.

### `aubio/bindings`
This package contains the raw bindings, with no name processing or whatsoever. You should only use this package if I was super lazy and
didn't provide a lispy binding for some function.

### `aubio/examples`
A package to contain all the examples of usage of the different aubio objects


## Aubio objects
Before explaining each object by itself, I want to tell you that the guys and gals in charge of the aubio project did an extremely good
job at mantaining its API object oriented. And what I am refering to is that
* every object can be created with `new_aubio_<name-of-the-object>`
* and can be destroyed with `del_aubio_<name-of-the-object>`
* and most of them have a `aubio_<name-of-the-object>_do` (which seems to respond of a method object of sorts)

So, in my Lisp translation I'm trying to mantain that convention in this form:
* every object will be created with a `make-<name-of-the-object>` function
* every object will be deleted with a `clean` generic function
* I'll try to make a generic function for the `do` part, but only if I find that it makes sense to have a polymorphic way of dealing with some objects.
  In other cases, I probably will generate functions with more descriptive names of what the object is actually doing

**Important note:** In order to distinguish the object that represents the detection algorithm and the thing being detected
in this Lispy translation there exists two different objects to reify those two thing (like the `note-detector` and the `note` itself).
In the aubio source, there is no "detector" and the detector is named after the thing it detects. There is no struct or datatype that
reifies the thing being detected (in the previous example, a note is simply a list of three elements)


### Objects currently ported
* [Source](docs/source.md)
* [Sink](docs/sink.md)
* [Pitch detection](docs/pitch.md)
* [Onset detection](docs/onset.md)
* [Note detection](docs/notes.md)
* [Tempo detection](docs/tempo.md)
* [Filterbank](docs/filterbank.md)
* [Aubio data structures](docs/datastructures.md)
