---
title: Bootstrapping this site
author: Brendon A. Kay
tags: hakyll, haskell, hugo, functional programming
---

For my new website, I figured my first post could be about getting this thing set up.
This website is generated using [Hakyll](http://jaspervdj.be/hakyll), a Haskell static site generator (SSG).
What's nice about SSGs is you write the content of the site in some kind of markup language and then generate the HTML,
JavaScript, and CSS with a compiler. Consequently, deploying is easier because
you only have to deploy static assets.

## Why Hakyll?
Lately I have been learning functional programming out of self interest. During my
educational journey I have dabbled in Scala, JavaScript, Python, and then eventually Haskell.
Haskell has been the most rewarding to learn, in my experience. It has also been
the most challenging. I wanted more practice using Haskell, and I have previous
experience with Hugo, so I figured Hakyll would be a fun port of my website.
This time with a blog.

## What separates Haskell from the rest
Compiled. Statically typed with type inference. The interpreter is awesome.
The community.

## Setting up my development environment
- [NeoVim](https://neovim.io/)
- [HLS](https://github.com/haskell/haskell-language-server)
- [Stack](https://docs.haskellstack.org/en/stable/)

It helped using stack to install GHC and all other Haskell environment dependencies.
I'd be interested in taking using [Nix](https://github.com/NixOS) next time, rather than stack. Nix seems to be very popular in Haskell.

### Useful `stack` commands
Mentioned in [a Hakyll tutorial](https://jaspervdj.be/hakyll/tutorials/01-installation.html)
```
stack build
```
then
```
stack exec site watch
```
