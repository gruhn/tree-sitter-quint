# tree-sitter-quint 

Work in progress Tree Sitter grammar for [Quint](https://quint-lang.org/).
Mainly to get editor support in [Helix](https://helix-editor.com/).

To avoid tracking build artifacts in Git, 
the generated parser is not included on the `master` branch.
Check the `release` branch instead.

## Example Helix Config (for now):

```toml
; languages.toml

[[language]]
comment-token = "//"
file-types = ["qnt"]
language-servers = ["quint-lsp"]
name = "quint"
scope = "source.quint"
block-comment-tokens = { start = "/*", end = "*/" }
tab-width = 2
unit = "  "

[[grammar]]
name = "quint"
source = { git = "https://github.com/gruhn/tree-sitter-quint.git", rev = "release" }

[language-server.quint-lsp]
args = ["--stdio"]
command = "quint-language-server"
```
