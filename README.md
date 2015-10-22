# keyhole

keyhole is a small library for performing keyhole surgery on data.

## Installation

Add the following dependency to your `project.clj` file:

```clj
[keyhole "0.1.0"]
```
## Raison d'être
<img src="https://cloud.githubusercontent.com/assets/1006557/10664380/339b48d4-78c3-11e5-9666-c58d281872c6.jpg" align="right">

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
