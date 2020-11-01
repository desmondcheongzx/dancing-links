Dancing Links
==============
Donald Knuth observes in [Dancing Links](https://www.ocf.berkeley.edu/~jchu/publicportal/sudoku/0011047.pdf) (Knuth, 2000) that every programmer knows how to remove an element from a doubly linked list via the following assignments:
```
L[R[x]] := L[x], R[L[x]] := R[x]
```
but relatively few realise that the reverse operation to put the element back is just as simple:
```
L[R[x]] := x, R[L[x]] := x
```

While it might be useful to clean up the links of a list element that has been removed, these dangling links are also convenient for algorithms that need to restore the old state of the list. For example, when backtracking. To illustrate the utility of this technique, Knuth described Algorithm X, which is a simple trial-and-error approach, that solves [exact cover](https://en.wikipedia.org/wiki/Exact_cover) problems using these dancing links.

I came across this paper at a time when I was also first introduced to Lisps. The natural outcome of this coincidence was this project to implement Dancing Links in common lisp to learn more about the technique and about lisp. Once we created a general solution for exact cover problems, it was then a small step to create a demonstration that solved [Sudoku](https://en.wikipedia.org/wiki/Sudoku). Indeed most of the additional software comprises the command-line interface to input (and debug the entry) of a sudoku board.
