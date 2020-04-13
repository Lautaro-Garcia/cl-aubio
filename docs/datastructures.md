# Aubio data structures
Aubio defines its own data structures for some particular cases. The ones defined here are the following

## `fvec_t` is called `float-vector`

| Functionality                                | Function that does that thing                    |
|----------------------------------------------|--------------------------------------------------|
| create an empty vector with defined length   | `make-float-vector`                              |
| create an empty vector from a Lisp vector    | `make-float-vector-from-lisp-vector`             |
| get/set a sample                             | `get-vector-sample` / `(setf get-vector-sample)` |
| set all elements to a value                  | `set-all-samples`                                |
| set all elements to zero                     | `set-all-samples-to-zero`                        |
| set all elements to one                      | `set-all-samples-to-one`                         |
| reverse                                      | `reverse-vector`                                 |
| multiply every element by a different factor | `weight-vector`                                  |
| copy                                         | `copy-vector`                                    |
| copy and weight in a single operation        | `weighted-copy`                                  |


## `fmat_t` is called `float-matrix`

| Functionality                                        | Function that does that thing      |
|------------------------------------------------------|------------------------------------|
| create an empty matrix with defined width and height | `make-float-matrix`                |
| create a matrix from a Lisp two-dimensional array    | `make-float-matrix-from-array`     |
| get a row (channel) of the matrix                    | `get-channel-data`                 |
| get/set a sample                                     | `get-sample` / `(setf get-sample)` |
| set all elements to a value                          | `set-all-samples`                  |
| set all elements to zero                             | `set-all-samples-to-zero`          |
| set all elements to one                              | `set-all-samples-to-one`           |
| reverse                                              | `reverse-matrix`                   |
| scale every row by a different factor                | `weight-matrix`                    |
| copy                                                 | `copy-matrix`                      |
| multiply with a vector                               | `multiply-matrix-with-vector`      |


You can transform any of this structures to a lisp one by using the function `aubio-to-lisp`
