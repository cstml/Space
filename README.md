# Space

A declarative, (almost) statically typed, stack programing language. Inspired by
the FMC, FMCt, and Forth.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Space](#space)
    - [Running the language](#running-the-language)
    - [Building the project](#building-the-project)
        - [Using nix](#using-nix)
        - [Using cabal](#using-cabal)
        - [Using stack](#using-stack)
    - [Language tutorial](#language-tutorial)
    - [Type syntax](#type-syntax)
    - [Example files](#example-files)
    - [Potential Ideas](#potential-ideas)
    - [Term equivalence](#term-equivalence)

<!-- markdown-toc end -->

## Running the language

You can play in the `spci` interpreter or run files with the `spc` interpreter.

## Building the project 

There are many ways of building the project.

### Using nix 

1. Run `nix build` inside the repository

2. Run `./result/bin/spc` for the interpreter or `rlwrap ./result/bin/spci` for
   the interactive interpreter. Note that I recommend you use `rlwrap` to get
   the nice line wrpapping.

### Using cabal

1. Run `cabal run spc` for the interpreter or `rlwrap cabal run spci` for the
   interactive interpreter.
   
### Using stack

1. Run `stack run spc` or `rlwrap stack run spci`

## Language tutorial

Let's imagine we have an infinity of stacks, each having a name. From this
infinity of stacks - to make things easier we choose two. One stack, our main
one, is called `@γ`, and another is called `@_` (or the Default location).
Observe that we know they are location due to their name starting with an `@`.

If we wanted to use the stack at location `foo` we would simply refer to stack
`@foo`. Now, let's imagine we have a machine that moves terms in this
`Space`. Say we wanted to put the number `3` onto the main stack, the way to do
so is simply to give the term 3 to our `Space` machine and it will put it on the
stack. This is done simply by evaluating the number 3 like so:

```space
> 3
```

At the end of evaluating this term our stacks would look as follows:

```space-memory
@γ: 3
@_: 
```

Cool, so we've essentially pushed the number 3 to the main stack. What if we
evaluated another number, say `5`, like so:

```space
> 5
```

Well then the term would pe pushed to the main location and the term would be
evaluated again, leading to the following memory:

```space
@γ: 5;3
@_: 
```

What if we wanted to add these two numbers. For that `Space` offers us the
function `+` with the type {int,int} -> {int}.

```space
> +
```

and the memory is now:

```space
@γ: 8
@_: 
```

To move `8` from the main stack to the default location, we now use another
special function, namely `scoop` with the operator `!`. Upon evaluating `!` the
`Space` machine picks up the first term and pushes it to the default location.

```space
@γ: 
@_: 8
```

## Type syntax 
  - Ch           <-> Char
  - {}           <-> Void 
  - Z            <-> Integer
  - [{Z} -> {Z}] <-> Integer -> Integer 
  - [{}  -> {Z}] <-> Void    -> Integer


## Example files 

To see some examples, have a look at the test files.

## Potential Ideas

- [ ] a module system

- [ ] a step debugger that steps through the the term as it executes.

- [x] make a program that actually does something - check examples.

## Term equivalence

| Haskell     | Space                     |
|-------------|---------------------------|
| x = 2       | [2];<x>                   |
|             | <x> [2]  (space notation) |
|             |                           |
| f x = x + 2 | [<x>;2;x;+];<f>           |
| z = f 2     | 2;f;<z>                   |
|             |                           |

