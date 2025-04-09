---
title: Coding with LLMs and `codecompanion.nvim`
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

# Background

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
mixed experience with the in-line features. So far, I have only used Copilot's
NeoVim plugin for this. If I were to guess, I'd say it's about 40% successful in
suggesting the right code.

Then I discovered the Codecompanion project. This was at work, when a coworker
posted about it in our Vim channel. Codecompanion provides a lot of the same
features as Cursor, but in NeoVim, and for free. Although I do encourage anyone
using it to donate what they can.
