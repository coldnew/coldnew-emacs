This package enable eldoc support when minibuffer is in use.

Eldoc info are shown by default in mode-line.
but you can have eldoc info somewhere else by setting
`eldoc-in-minibuffer-show-fn' to an other function (e.g `tooltip-show').

By default with this package `M-:' will use `pp-eval-expression'
instead of `eval-expression'; You can change that in
`eval-prefered-function'.

It provide also a convenient macro to enable eldoc support
in your own functions using minibuffer or in your defadvices,
that is `with-eldoc-in-minibuffer'.

Users of own minibuffer frame will have to set
`eldoc-in-minibuffer-own-frame-p' to non--nil.

You can turn off at anytime eldoc support in minibuffer
by setting `eldoc-in-minibuffer' to nil.
