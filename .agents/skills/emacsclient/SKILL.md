---
name: emacsclient
description: 'Always use emacsclient instead of emacs. This applies to all Emacs operations: user requests, byte compilation, check-parens, running ERT tests, and any other elisp evaluation.'
tools: Bash
---

# Always use emacsclient

The user has an Emacs server running. All Emacs operations must go through `emacsclient`, never `emacs` or `emacs --batch`. This includes both user-requested actions and agent-initiated operations like byte compilation, syntax checking, or running tests.

## Examples

- Open a file: `emacsclient --no-wait "/path/to/file"`
- Evaluate elisp: `emacsclient --eval '(some-function)'`
- Open at a line: `emacsclient --no-wait +42 "/path/to/file"`
- Byte compile a file:
  ```sh
  emacsclient --eval '
  (byte-compile-file "/path/to/file.el")'
  ```
- Check parentheses:
  ```sh
  emacsclient --eval '
  (with-temp-buffer
    (insert-file-contents "/path/to/file.el")
    (check-parens))'
  ```
- Run ERT tests:
  ```sh
  emacsclient --eval '
  (progn
    (load "/path/to/test-file.el" nil t)
    (ert-run-tests-batch-and-exit "pattern"))'
  ```

## Agent introspection API (preferred for symbol lookup)

When a task asks for Emacs symbol introspection from CLI (e.g., function/variable completion, docs, source, value), prefer the dedicated API in `lisp/agent-introspect-cli.el`:

- `agent-introspect-cli-function-completions`
- `agent-introspect-cli-variable-completions`
- `agent-introspect-cli-function-documentation`
- `agent-introspect-cli-variable-documentation`
- `agent-introspect-cli-function-source`
- `agent-introspect-cli-variable-value`

Use them through `emacsclient --eval` and print results with `prin1`/`pp-to-string` so Bash captures machine-readable output.

Examples:

- Function completions
  ```sh
  emacsclient --eval '
  (prin1 (agent-introspect-cli-function-completions "find-file"))'
  ```
- Variable documentation
  ```sh
  emacsclient --eval '
  (prin1 (agent-introspect-cli-variable-documentation "user-full-name"))'
  ```
- Function source
  ```sh
  emacsclient --eval '
  (prin1 (agent-introspect-cli-function-source "find-file"))'
  ```

If the API is unavailable, explicitly `require` it first:

```sh
emacsclient --eval '
(progn
  (require (quote agent-introspect-cli))
  (prin1 (agent-introspect-cli-function-documentation "find-file")))'
```

## Rules

- Always use `emacsclient`, never `emacs` or `emacs --batch`.
- Use `--no-wait` when opening files so the command returns immediately.
- Use `--eval` when evaluating elisp.
- Always format `--eval` elisp across multiple lines with proper indentation.
- For CLI introspection tasks, prefer `agent-introspect-cli` API over ad-hoc elisp.
- Run `emacsclient` commands via the Bash tool.
