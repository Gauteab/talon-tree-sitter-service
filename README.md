# talon-tree-sitter-service

This project aims to bring high level voice commands to Talon for efficient vocal programming in any text editor
and should serve as an early prototype for the system described in my master thesis.

The current implementation is a HTTP server written in PureScript that uses TreeSitter to extract syntactic information
from an Elm program and exposes it to Talon in order to increase accuracy when dictating Elm code, and enable structural navigation and editing in Vim.

# Installation

Start by cloning this repository into your Talon user directory (`~/.talon/user`)
and make sure you have the following programs installed:
- npm
- node@12.11
- vim (>= 8.0) / nvim

If you are new to Vim, create a file called `.vimrc` in your home directory and add the following line:
```vim
source ~/.talon/user/talon-tree-sitter-service/simple.vimrc
```

If you already have vim set up, add the following line to your Vim configuration file:
```vim
source ~/.talon/user/talon-tree-sitter-service/talon.vimrc
```

If you are already using Vim with Talon, you can remove `vim.py` and `vim.talon`,
but keep `vim_editor.py`.
If you're managing the title in your Vim config you might need to comment that code out.

To start the server, navigate to the `backend` directory and run the following commands:
```bash
npm install
npm start
```

Once you have everything set up, open `Example.elm` in Vim.
Your window title should look something like:
`VIM MODE:n | (1,1) | ~/.talon/user/talon-tree-sitter-service/Example.elm`
and you should be seeing some debug output from the server.

# Using the System

The current version supports the following use cases:
- Dictating type signatures
- Navigating between function/type declarations or any identifier.
- Selecting and deleting function/type declarations.

Example commands:
```
go fun view
go type message
go to text
go to next
select type person
delete type message
```

> Note: the server is currently not being notified immediately
> when a change occur, so make sure to save the document after making a change
> so the server is in sync for the next command.

# Backend Development

To build the code we use `Spago` (https://spacchetti.readthedocs.io/en/latest/spago.html)

```
npm install -g purescript-spago
spago run
```
