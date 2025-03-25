---
title: Using `codecompanion.nvim`
author: Brendon
---

Let's get this out of the way; AI is a tool. I've had mixed success with
Copilot's autocomplete feature, but the chat can be really useful. If you've
ever heard of Cursor before, it tries to improve on the developer experience by
going beyond autocomplete suggestions like Copilot, and integrating the chat
with code diffs.

I recently discovered `codecompanion.nvim` which aims to provide a similar
experience. Per their helpfile, `:h codecompanion.txt`, it reads,

> CodeCompanion is a productivity tool which streamlines how you develop with
> LLMs, in Neovim.

All documentation referred to in this article will be sourced from the
Codecompanion NeoVim help document, unless otherwise specified.

# Prerequisites

Make sure you have an account with an LLM. I'm going to use GitHub Copilot with
Claude enabled.

NeoVim is another obvious one that you may want installed. I will assume the
reader is fairly familiar with this tool I will assume the reader is fairly
familiar with this tool.

# Chat

Useful command:

> `gy` to yank the last codeblock in the chat buffer

One feature I really like is default prompts. These are prompts you can enter
into an LLM chat in a few words that are often repeated. CodeCompanion comes
with the following default prompts. Here is a list of these as of this writing:

- `Explain` - Explain how code in a buffer works
- `Fix Code` - Fix the selected code
- `Explain LSP Diagnostics` - Explain the LSP diagnostics for the selected code
- `Unit Tests` - Generate unit tests for selected code
- `Generate a Commit Message` - Generate a commit message

These, combined with the `@editor` agent discussed later, make up the bulk of
the benefits I have realized from using this plugin.

# Variables

> Variables allow you to share data about the current state of Neovim with an
> LLM.

# Slash commands

> Slash Commands enable you to quickly add context to the chat buffer.

`/WORKSPACE` seems very powerful. Especially with the ability to build your own.
`codecompanion-extending-workspace`

# Agents / Tools

The |codecompanion-usage-chat-buffer-agents-editor| tool enables an LLM to
modify code in a Neovim buffer. This is especially useful if you do not wish to
manually apply an LLM’s suggestions yourself. Simply tag it in the chat
buffer with `@editor`.

# Conclusion

Using Codecompanion has been great. The documentation is well written and rich
with examples.

<!--TODO: Link home manager config-->

If you would like to check out my simple config, please see my Nixvim
homemanager congiuration.

You can probably use `codecompanion-usage-action-palette` to pimp out your key
bindings.

Check out https://github.com/olimorris/codecompanion.nvim/blob/main/lua/codecompanion/config.luafiles
for some example configs.

This is just the tip of the iceberg with this tool. Agentic workflows would be
an interesting topic to cover in the future.
