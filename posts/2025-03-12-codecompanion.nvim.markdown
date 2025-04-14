---
title: LLM Coding with Codecompanion and NeoVim
author: Brendon
---

In this post I'm going to discuss using LLM tools integrated into my favorite
editor, NeoVim. Specifically, I'll be discussing my thoughts on the NeoVim
plugin `codecompanion.nvim`. I'll refer to it as just Codecompanion from this
point on. The LLM that I will use with Codecompanion is GitHub Copilot. I know
there are likely better ones out there. I plan to look into Claude in the
future.

Before I continue, I'd like to preface this article with a statement: I am not
an LLM expert. I am just learning, like a lot of other folks, and so I might be
using these things incorrectly or inefficiently. Please reach out and let me
know if that's the case.

Also, I'm not some self-identified "vibe coder". Nor do I think LLMs, or "AI",
are going to dramatically change our lives for better or worse. I think these
are new tools and we will be learning how to use them and integrate with them
over the coming years. It may be that they make certain tasks obsolete, as we'll
see in this post, but also, I don't think they're quite there yet to supersede
entire populations of software engineers. People whose job it is to wrangle the
complexity of business logic and computer systems. They do seem able to make us
more productive in this endeavour, though.

<!--TODO: Add link for Cursor-->

## Background

Unless you are lucky or extremely intentional in avoiding it, you have
probably experienced the LLM hype in one form or another. For most software
engineers, this has come in the form of coding assistance. Some folks
will just use the basic chat web interface, like chatgpt.com. Others are
beginning to use fully integrated IDEs like Cursor.

Cursor's product provides features that take chatting with the LLM a step
furthor. For example, while chatting with the LLM you can have it diff your
buffer with code sugeestions. This is a seamless experience since most software
engineers are familiar with diff tools. Mostly from having to deal with the
unfortunate situation of merge conflicts. Either way, it allows you to be
selectively choose code suggestions right there in your editor. No going
back-and-forth between chat windows and manually copying the code suggestions.

<!--TODO: Link NeoVim's Copilot plugin.-->

There are other features as well, such as in-line code suggestions. I've had
mixed experience with the in-line stuff. So far, I have only used Copilot's
NeoVim plugin for this. If I were to guess, I'd say it's about 40% successful in
suggesting the right code.

Then I discovered the Codecompanion project. This was at work, when a coworker
posted about it in our Vim channel. Codecompanion provides a lot of the same
features as Cursor, but in NeoVim, and for free. Although I do encourage anyone
using it to donate what they can. From the docs, they say:

> CodeCompanion is a productivity tool which streamlines how you develop with
> LLMs, in Neovim.

## Codecompanion features

In this section I will discuss the features available in Codecompanion.

### Chat

Chat is the main way of interfacing with an LLM right now. Most people just go
to ChatGPT's website and start chatting with it by typing or even talking via
voice-to-text. Coding with the LLM is no different. What's become very useful is
the ability to enhance chat with some familiar editor features, like diff
management.

Codecompanion also provides default prompts for quicker a quicker chatting
experience. These are prompts you can enter into an LLM chat, in a few words,
that are often repeated. CodeCompanion comes with the following default prompts:

- `Explain` - Explain how code in a buffer works
- `Fix Code` - Fix the selected code
- `Explain LSP Diagnostics` - Explain the LSP diagnostics for the selected code
- `Unit Tests` - Generate unit tests for selected code
- `Generate a Commit Message` - Generate a commit message

These, combined with the `@editor` agent discussed later, make for a pretty
seamless editing experience.

### Variables

> Variables allow you to share data about the current state of Neovim with an
> LLM.

I pretty much always stick with `#buffer`. So far, I haven't had to use the
other two, but `#lsp` and `#viewport` seem like powerful tools. One callout in
the docs is that `#lsp` can be used to evaluate diagnostics. I could see that
being helpful for confusing error messages, which happen all the time in
Haskell.

### Slash commands

> Slash Commands enable you to quickly add context to the chat buffer.

`/WORKSPACE` seems very powerful. Especially with the ability to build your own.
`codecompanion-extending-workspace`

Working with haskell I unfortunately received this error:

```
There are no Tree-sitter symbol queries for `haskell` files yet.
Please consider making a PR.
```

### Agents / Tools

> In the plugin, tools are simply context and actions that are shared with an
> LLM via a `system` prompt. The LLM and the chat buffer act as an agent by
> orchestrating their use within Neovim.

The |codecompanion-usage-chat-buffer-agents-editor| tool enables an LLM to
modify code in a Neovim buffer. This is especially useful if you do not wish to
manually apply an LLM’s suggestions yourself. Simply tag it in the chat
buffer with `@editor`.

<!--TODO: Add a list of tools-->
<!--TODO: mention automatic toold mode-->

Unfortunately, editing Haskell did not go so well. It would make the correct
diff, but it would also delete large chunks of code that I needed. I think this
is very well could be user error. It also could be the LLM model that I used.

## Using Codecompanion

I gave this a spin on a personal project that I have in early development. This
is a Haskell program that simulates a baseball game using "dice", i.e.
`randomRIO`. In the `Logic.hs`, where the game logic is written, I tried
`:CodecompanionChat` and then entered `#buffer Explain`. I got the following
reply:

> This module is designed to simulate the logic of a baseball game using dice
> rolls to determine the outcomes of pitches and actions. It uses the `State`
> monad to manage and update the game state throughout the game.

If you looked at the source, there aren't a lot of comments in this file. It
gave me a pretty good breakdown of each function's purpose. Pretty good success
for basic LLM interaction.
