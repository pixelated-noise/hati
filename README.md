# hati

Maturity level: vaporware

A Clojure library to generate documentation from comments and
docstrings. Inspired by marginalia.

## Thoughts

* Should be able to include arbirtary "articles".
* The "index" page of the documentation should be auto-generated, or
  hand-crafted, or some combination of the two.
* Should be able to work on a arbitrary number of projects to generate
  combined documentation. Per-project documentation will only get you
  so far.
* The "ontology" of prose that emerges from Clojure syntax (and these
  could be rendered differently):
  * docstrings
  * top-level comments (outside of functions)
  * function inner comments
  * comments that are "attached" to some s-expression (as in `(code)
    ;; my comment about the code`)
* Not everyone is interested in everything. Allow users to hide the
  code, and to hide certain categories of tagged paragraphs.
* Provide links back to github (or other code hosting sites).
* What about different versions???
* Dynadoc???

## License

Copyright © 2018 Pixelated Noise Ltd

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
