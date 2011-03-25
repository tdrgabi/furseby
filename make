notangle src/furseby.nw > src/furseby.lisp
noweave -index -html src/furseby.nw > doc/furseby.html

notangle src/core.nw > src/core.lisp
noweave -index -html src/core.nw > doc/core.html

notangle src/plugins/gutenberg.nw > src/plugins/gutenberg.lisp
noweave -index -html src/plugins/gutenberg.nw > doc/gutenberg.html
