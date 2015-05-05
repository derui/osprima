# osprima #
This is a JavaScript AST Generator implemented by pure OCaml. This name is \`osprima' based by well-known \`esprima' that is JavaScript Parser implementation.

## Installation ##
You must install requirement softwares before osprima installation follows. 

- OCaml (>= 4.02.1)
- core
- OMake (>= 0.9.8.6-0.rc1)

If you do not have any problem to use [opam](http://opam.ocamlpro.com/), I recommend that you use it.

After requirement softwares installation, Type command below on your terminal (or console).

```
$ git clone https://github.com/derui/osprima.git
$ cd osprima
$ git submodule sync
$ omake install
```

## Limitation ##
This program is imperfect, this have some problems below.

- Can not convert hex decimal to unicode point format.
- Over escaped backslash if it is *\x* or *\u* .
- Print json to stdout, but not pretty printed for it.

... and more.
