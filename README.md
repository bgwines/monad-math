# monad-math
Coding up the monad laws from a category-theoretic perspective!

`MonadMath.hs` exposes the three components of a monad: the functor `F` and the natural transformations `µ` and `η`.
`test-monadmath.hs` contains tests of naturality (`µ y . F h = F h . µ x` and `η y . I h = F h . η x`) and monad law tests (`µµf = µfµ` and `µfη = I_F = µηf`).

(Thanks to [Stefan Klinger's excellent monad guide](http://stefan-klinger.de/files/monadGuide.pdf))
