Dancing Links
==============
[Dancing Links](https://www.ocf.berkeley.edu/~jchu/publicportal/sudoku/0011047.pdf) is a simple technique by Donald Knuth to remove and replace elements in a doubly linked list and Algorithm X (in the same paper) is a simple trial-and-error approach to solving exact cover problems.

The algorithms themselves are straightforward (evidenced by Knuth's inability to come up with a better name for what's essentially brute-force), but they are nonetheless effective. The dancing links enable the search space to shrink considerably after every guess and expand to its original state when backtracking. Moreover, using doubly linked lists to represent these sparse tables ensures that traversing the matrix is fast, even when the traversal method itself is inefficient (as in my code). Or maybe that's just an excuse.

This is my attempt at implementing Dancing Links in common lisp to learn more about the algorithm and about lisp.

## Problems solved
- Exact Cover
- Sudoku
