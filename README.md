# keyhole

keyhole is a small library for performing keyhole surgery on data.

## Installation

Add the following dependency to your `project.clj` file:

```clj
[keyhole "0.1.0"]
```

## Usage

```clj
(ns some-ns
  (:require [keyhole.core :as k]))

;;; Examples taken from the specter readme for comparison purposes

;; Increment all even values under the :a key for a seq of maps:
(k/get-in [{:a 1} {:a 2} {:a 4} {:a 3}] [all* :a even?])
;;=> (2 4)

;; Get every number that is divisible by three out of a seq of seqs:
(k/get-in [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]] [all* all* #(= 0 (mod % 3))])
;;=> (3 3 18 6 12)

;; Increment the last odd number in a seq:
(k/update-in [2 1 3 6 9 4 8] [(filter* odd?) last*] inc)
;;=> [2 1 3 6 10 4 8]

;; Increment all the odd numbers between indexes 1 (inclusive) and 7 (exclusive) with step 3:
(k/update-in [0 1 2 3 4 5 6 7] [(range* 1 7 3) all* odd?] inc)
;;=> [0 1 2 2 4 5 4 7]

;; Replace the subsequence from index 2 to 4 with [-1 -1 -1]
(k/update-in [0 1 2 3 4 5 6 7 8 9] [(range* 2 4)] (constantly [-1 -1 -1]))
;;=> [0 1 -1 -1 4 5 6 7 8 9]
```

## Raison d'être
<img src="https://cloud.githubusercontent.com/assets/1006557/10665167/7eab4846-78c9-11e5-9ac5-a694428435b9.jpg" align="right"/>
This library is heavily inspired by
[specter](https://github.com/nathanmarz/specter).  I think
[specter](https://github.com/nathanmarz/specter) is the best thing to
come along since sliced bread.  If you are happy with
[specter](https://github.com/nathanmarz/specter) there's really no
reason to change.  In fact, keyhole only implements a subset of the
features available in
[specter](https://github.com/nathanmarz/specter), so there are good
reasons to stay put.  That said, I wrote keyhole to improve upon [specter](https://github.com/nathanmarz/specter) in the following ways:

* Handle 'compilation' behind the scenes.
* Improve performance.
* Simplify the API.
* Make the DSL more chill (the use of screaming case seemed way too aggro to me).
* Meant to be required and aliased instead of referred wholesale into the namespace.

## License

Copyright © 2015 Lars Andersen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
