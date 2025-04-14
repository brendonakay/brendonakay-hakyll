---
title: Hardening Applications
author: Brendon
---

# Hardening Applications

## Outline

- Applications with runtime exceptions are buggy.
- Using haskell means we operate on the entire input domain of the function.
- Writing applications in languages like Python gets tiring trying to handle all of the exceptions.
- Writing Haskell takes a lot of up front effort, but once that is done, the application is _correct_.
- A proper type system lets you build correct abstractions for free.
- A correct application is a hardened application.

I'm about to argue that production software should not be written in a programming language with a weak type system.

For that, I need to describe "weak"

## Resources

- [ https://www.gtf.io/musings/why-haskell ]

---

# Post

## Hardening Applications

I think that with applications, except for those utilizing on-demand compute (i.e. AWS
lambdas), runtime exceptions are the source of a lot of developer and user frustration.

## Philosophical bantering

We can visualize our interaction with software like a tree. Many trees, even.
The trunks of these trees are effects, usually inputs, from the user that branch out into the
various logical conditions of that effect. In the program, what I'm loosely referring to
here is the [call stack](https://en.wikipedia.org/wiki/Call_stack).

> In computer science, a call stack is a stack data structure that stores information about the active subroutines of a computer program.

With on-demand compute, there's just one tree in the context of the inputs of
this program. This means that if something suddenly interrupted
the traversal of this tree we would have just one thing to look at.

With most other programs, that could be running multiple subroutines on many
different CPU cores (Note: I don't mean to say that you
can't utilize multiple cores in on-demand compute functions, but that would be
within the context of the single process.), this means sudden interruptions are
a little more challenging to track down. They are also more likely to occur,
given the larger number of trees in this scenario.

As a user of software, we would like to be able to give it some instructions (a
set of inputs; of effects), and receive some sort of _expected_ output. A person clicks play
on YouTube and the video plays. Someone opens a game on their PlayStation
and the game starts up. Or it could be the power button was pushed to turn on a
desktop computer and the BIOS processes this signal and turns the computer on,
as _expected_. I highlight "expected" here because that's an essential part of
human interaction with computer software. We have expectations. If these expectations are
violated, than something was incorrect. There was an error.

As writers of software, we should ensure our software is
correct before it is run by a user.

## Exceptional bantering

What I was referring to in the previous section about interruptions during tree
traversals were runtime exceptions. I'm convinced these are the little
devils that infect some software and make it a pain in the ass to use or
maintain. I'd like to be clear that runtime exceptions are not the only things
can cause interruptions, but they are the things we can address
and improve upon greatly.
