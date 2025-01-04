---
title: Language Server
---

`harper-ls` is the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) frontend for Harper.
Out of the box, it has built-in support for parsing the comments of most programming languages, as well as any and all Markdown files.

### Cargo

If you have [Rust installed](https://www.rust-lang.org/tools/install), you're in luck!
To install `harper-ls`, simply run:

```bash
cargo install harper-ls --locked
```

### Arch Linux

Harper is available through the `extra` repo:

```bash
sudo pacman -S harper
```

## Dictionaries

`harper-ls` has three kinds of dictionaries: user, file-local, and static dictionaries.

### User Dictionary

Each user of `harper-ls` has their own dictionary, located in the following directories on each operating system:

| Operating System |                                       Location |
| :--------------- | ---------------------------------------------: |
| Linux            |                  `$XDG_CONFIG_HOME/harper-ls/` |
| MacOS            | `$HOME/Library/Application Support/harper-ls/` |
| Windows          |             `FOLDERID_LocalAppData/harper-ls/` |

This dictionary is a simple line-separated word list in plain-text.
You can add and remove words at will.
Code actions on misspelled words allow you to add elements to this list.

This was added in response to [issue #89](https://github.com/automattic/harper/issues/89).

### File-Local Dictionary

Sometimes, you'll encounter a word (or name) that is only valid within the context of a specific file.
In this case, you can use the code action that adds the word to the file-local dictionary.
Any words added to this dictionary will, like the name implies, only be included in the dictionary when performing corrections on the file at that specific path.

You can find the file-local dictionaries in the following directories on each operation system:

| Operating System |                                                                                         Location |
| :--------------- | -----------------------------------------------------------------------------------------------: |
| Linux            | `$XDG_DATA_HOME/harper-ls/file_dictionaries` or `$HOME/.local/share/harper-ls/file_dictionaries` |
| MacOS            |                                  `$HOME/Library/Application Support/harper-ls/file_dictionaries` |
| Windows          |                                              `FOLDERID_LocalAppData/harper-ls/file_dictionaries` |

The format of these files is identical to user dictionaries.

### Static Dictionary

The static dictionary is built into the binary and is (as of now) immutable.
It contains almost all words you could possibly encounter.

I _do_ take pull requests or issues for adding words to the static dictionary.
[Read the documentation on the matter before you do.](../contributors/dictionary)
