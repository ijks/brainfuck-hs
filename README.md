# brainfuck-hs
Yet another Brainfuck interpreter in Haskell. It's a good exercise. :)

Note: IIRC, this did not work correctly for some more complicated examples of Brainfuck code.
It should, however, work fine for a straightforward Hello World program.

One choice I made regarding the implementation details of the interpreter is that its 'tape'
is infinite in both directions, and that its cells may have negative values. This might be the
reason that some programs don't work, because they rely on different assumptions.
