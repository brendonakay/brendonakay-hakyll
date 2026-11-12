---
title: Radiation
author: Brendon
tags: opinion, rant
---

## Historical radiation

Clarence Madison Dally was a late-19th century glass blower who helped Thomas
Edison with his early experiments on x-rays. He helped develop the
fluoroscope, an x-ray imaging device with a calcium tungstate screen.[^1]

Dally is thought to be one of the first people to die from complications of
radiation poisoning. Edison's biography, published in 1910, six years after
Dally's death in 1904, notes that "...the X-ray had affected poisonously my
assistant, Mr. Dally."[^2] He did not die from a single incident with
the technology. Instead, he was repeatedly exposed to the harmful rays of
radiation by using and demonstrating the device for Edison.

Eventually, it poisoned him. It would be another 24 years before the
International X-ray and Radium Protection Committee was established and set
standards for this harmful but useful technology.

It takes time for us to get used to new and dangerous technologies.

## Contemporary radiation

Claude Code [emerged via
dogfooding](https://www.anthropic.com/features/making-of-claude-code) from the
Anthropic engineering team. In a series of interviews with the employees
responsible for the creation of the agentic coding harness, one of them
admits that, by Winter of 2025, they no longer write any code by hand.

Well, it's almost Winter of 2026 and, I hate to say it, but neither do I.

So, while I myself am a user of this technology--and perhaps because of this--I
do wonder if it is possible these engineers are like that of the early x-ray
folks. Exposed to dangerous amounts of their own product.[^3]

But it does seem to me that by possessing large quantities of raw AI you can
develop a kind of psychosis. We have Geiger counters, and so I don't think this
technology is actually radioactive, or I'd expect we'd know it by now.

Instead, it could be a sort of brain poison.[^4]

## Lead vests

"Slow is smooth," [as I've said before](/posts/2025-12-02-slow-is-smooth.html), is
why I am typically a slow adopter of new tools. This is personal preference, so
just to be clear, I am not trying to promote my opinion as fact, but it
could be what prevents me from overdosing on the poison.

Remember, I am a user of AI, of LLMs. I use them at work. I use them to help me
study. I use them to help me edit. I use them--period. I still have my
hesitations, but I am a user. When I write about my thoughts on AI, and compare
it to radiation poisoning, I am not trying to pretend that I am better than
anyone else, or know what you should or should not do.

All I'm really trying to say is: be careful.

Being a slow adopter in this moment could be the contrarian take; defensive.
That works in some contexts but not all. But being a slow adopter could mean
just waiting to adopt patterns and technologies on your engineering team until
they have proven their worth.

There's a lot of buzz out there right now. Let the dust settle and you'll see
clearly past the hype.

## Radiation tactics

Recently at work I have been given the exciting opportunity to implement formal
verification in our CI system. I hope to write more about this soon, but the
gist of it is that we are using Lean 4 to prove certain invariants in our
application. Then we use differential random testing to test the implementation
against the verified Lean model.

I have not tried Lean 4 with open models, but I do see this sort of
thing as a model-agnostic pattern--something you don't need a special harness
for: leveraging mathematics, proof assistants, and LLMs to harden you
applications. These proofs not only guarantee that the model satisfies your
specification, they also make it much harder for LLM-generated code to
violate certain guarantees about the application logic.

## Right language

Aldous Huxley once wrote:[^5]

> It is very significant that if we have to talk about the universe as the
> continuum which it is, we cannot do so in terms of any of the ordinarily
> existing languages, which deceive us all the time; it has to be done in terms
> of calculus, a special language invented for the express purpose of talking
> about the world as a continuum.

I believe that by using Lean to _prove_ our application's behavior is correct we
are speaking the right language with our agentic coworkers. Keeping us safe
from hallucinations and undefined behavior. It will be interesting and fun to
continue learning more about this new paradigm.

[^1]: [https://en.wikipedia.org/wiki/Clarence_Madison_Dally](<>)

[^2]: [https://www.gutenberg.org/...](https://www.gutenberg.org/files/820/820.txt#:~:text=the%20X%2Dray%20had%20affected%20poisonously%0Amy%20assistant%2C%20Mr.%20Dally%2C)

[^3]:
    Saying that got me thinking: perhaps drug dealers have been dealing with
    this kind of problem for many years already, and perhaps we could
    learn a thing or two from _them_.

[^4]: Poison as in "alcohol is a poison": deadly, but not absolutely so.

[^5]: [https://huxleyarchive.org/Non-fiction/BOOKS/The%20Divine%20Within.pdf](<>)
