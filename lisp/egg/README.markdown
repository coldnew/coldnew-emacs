# Egg (Emacs Got Git).

## Intro

Egg is an Emacs interface to git. It's a suite composed of a
minor-mode and various special-buffers presenting different UIs to
help the user performing many git operations.

- egg-minor-mode: providing git-specific vc-look-alike interface
  including similar key-bindings, a minor-mode menu and history
  annotations (blame).
- egg's status-buffer:
  - index manipulation/commit preparation
  - interactive rebase stepping
  - merge conflict resolution
  - stashing work-in-progress
  - adding ignore pattern
  - staging new files
  - ediff or ediff3 launching. (e.g. 3-way ediff of
    work-dir/INDEX/HEAD, 3-way ediff of work-dir/theirs/ours)
- egg's log-buffer
  - browse repo's history
  - ref (tag, branch, etc) creation and deletion
  - push and fetch
  - start merge/rebase/interactive-rebase session
  - attach/detach HEAD
  - search history (pickaxe)
  - compare revisions
- egg's file-log-buffer: restricted version of the log-buffer, used to
  browse history of a single file.
- egg's reflog-buffer: restricted variation of the log-buffer, used to
  browse the git's reflog and re-attach HEAD.
- egg's query:commit-buffer: restricted variation of the log-buffer,
  used to browse history-search's results (pickaxe)
- egg's stash-buffer: a log-buffer look-a-like, used to browse and
  apply stash entries to work-dir
- egg-grep: a compile-mode which can grep files in non-checkout git
  revisions.
- egg's commit-log-edit buffer: used to compose the commit-message for
  the upcoming commit. it can do some minor index manipulation.
- egg's tag:msg-buffer: used to compose the message of an annotated tag
- egg's diff-buffer: used to view the delta between file or repo revisions.

## History

The design and most ideas in Egg was borrowed/stolen from
Magit. However, egg has grown with many functionalities not present in
magit and should be considered based its own merits.

## MAGIT

Magit is an interface to the version control system Git, implemented
by Marius Voller. His code at: http://philjackson.github.com/magit/
