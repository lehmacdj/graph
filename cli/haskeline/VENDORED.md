# haskeline, vendored

This is haskeline 0.8.2.1 from Hackage (`stack unpack haskeline-0.8.2.1`),
used by `ge` in place of the release through `extra-deps` in
`../stack.yaml`.

It is vendored because haskeline has no way for a program to bind a key to
an action of its own. The commits that change this directory after the one
that added it are the patch; `jj diff` from that commit shows all of it.

The files keep their upstream formatting: the repository's jj config
excludes `cli/haskeline/**` from the ormolu fix tool.
