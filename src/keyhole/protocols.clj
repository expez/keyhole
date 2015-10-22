(ns keyhole.protocols)

;;;; These were meant to make keyhole user extensible but they turned
;;;; out quite a bit messier than what I had hoped. The bad news is I
;;;; don't see a good way of cleaning them up either.
(defprotocol Transformer
  "The transformer code is used to perform some transformation on a
  value in a nested datastructure.

  The emitted form should be a function which is ready to consume a value.

  When emitting code, in the transformer function, all the data fields
  of the corresponding keyhole is available.

  The field next-transformer can be used to place the subsequent
  transformer in the right place.

  transformer-basis is used to give some transformations, earlier in
  the chain, a hint about what's going to change so they can
  re-construct properly.  E.g. filter* needs last* to hint that
  filter* only needs to update the last of the filtered values it
  produces.  The basis is a pair of ::val or ::seq and a basis
  function.  The first half of the the pair indicates whether or not
  the return value should be spliced in or not.

  Here's the transformer code for the keyword keyhole:
  `(partial update* ~next-transformer ~k)

  Where update* is update with the parameters re-ordered to
  afford partial application."
  (transformer [this] "Emit transformer code.")
  (transformer-basis [this] "Emit the basis transformer."))

(defprotocol Selector
  "The selector code is used to perform a value lookup in a
  nested datastructure.

  The emitted form should be a function which is ready to consume a
  value.

  When emitting code, in the selector function, all the data fields
  of the corresponding keyhole is available.

  Additionally the field next-selector can be used to place the
  subsequent transformer in the right place.

  Since a selector is a form which evaluates to a partial ready to
  consume a value conditional selection is tricky.  This is solved by
  returning the value ::nothing, which will be removed from the final
  result set.

  Here's the selector code for the keyword keyhole:
  `(comp ~next-selector ~k)"
  (selector [this] "Emit selector code."))
