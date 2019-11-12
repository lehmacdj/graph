# graph
This is a library/command line application that manipulates graphs.
It implements a variety of unix-like commands in order to provide the
ability to sort/store files.

I will add tutorials/descriptions of basic commands here eventually. :)

# Todo list
small features:
- providers for a few things that I import somewhat frequently
- edit text files -> HUGE FOLLOWUP ITEM teach vim to understand graph filesystem
- use MyPrelude everywhere
- move to base-noprelude
- organize/trim imports

medium features:
- label edges with nodes instead of string labels
- migrate from integer NIDs to UUIDs/GUIDs/something similar
- make it possible to run without a "loaded" graph
- migrate InputT into the monad transformer stack
- implement a better completion engine

## big features:

#### more fully featured programming language

Write some more general commands for:
- sequencing,
- parallel execution,
- nondeterministic execution,
- choice

Ideally these can take the form of the same operators we already have
