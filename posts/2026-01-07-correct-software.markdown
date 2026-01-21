---
title: Hopefully, a Shift Towards Correct Software
author: Brendon
tags: opinion, response, formal verification, lean
---

## Formal Verification and Artificial Intelligence

In a recent [blog
post](https://martin.kleppmann.com/2025/12/08/ai-formal-verification.html)
Martin Kleppmann makes a prediction that formal verification will go mainstream.
When I first read this I paused, bent into the fetal position, and openly wept
into the palms of my hands as I felt the familiar feeling of hope again. *Could it
be that something possibly interesting is bubbling up from the tar-pit of AI hype?*

Dramatic, I know. But things have been bleak lately.

## Correct Software

Kleppmann wrote the absolutely wonderful book [*Designing Data-Intensive
Applications*](https://www.oreilly.com/library/view/designing-data-intensive-applications/9781491903063/).
This book is one of the most significant recent installments in software engineering
literature and I am excited for its second edition coming soon. So when Mr.
Kleppmann makes a prediction I listen. 

In his article, he makes the argument that, with the help of AI, i.e. Large
Language Models (LLMs), formal verification will be much cheaper to do and
therefore more software will be formally verified as a result.

For those of you that are new to formal verification, the book [*Theorem Proving
in Lean
4*](https://leanprover.github.io/theorem_proving_in_lean4/Introduction/#Intro)
describes it this way in their first chapter.

> *Formal verification* involves the use of logical and computational methods to
> establish claims that are expressed in precise mathematical terms. These can
> include ordinary mathematical theorems, as well as claims that pieces of
> hardware or software, network protocols, and mechanical and hybrid systems
> meet their specifications.

What this effectively does is eliminate certain classes of bugs from your code.
You are proving your system is *correct according to your specification*.

## The Road to Paradise

I have a deep interest in functional programming and mathematics -- especially in
the intersection where these two meet -- and one example is formal verification.
Formal verification comes in many shapes and sizes, but the one I will talk
about in this post is a *proof assistant*: [The Lean theorem
prover](https://lean-lang.org/).

As mentioned previously, with Lean you can prove mathematical theorems, such as
[*Euler's Formula*](https://en.wikipedia.org/wiki/Planar_graph#Euler's_formula),
or whether a complicated data science pipeline in your webapp will guarantee
correct results. The challenge mainly is modeling the system or theorem you are
trying to prove in Lean's language.

Thankfully, Lean has done a wonderful job of creating a language -- a full
fledged programming language -- with built-in operators for propositions and
quantifiers. So, a mathematician will feel right at home, but they've also
created new tools such as
[`tactics`](https://leanprover-community.github.io/mathematics_in_lean/), which
allow you to be more interactive with your proof. Kind of like programming.

## My Experience

Kleppmann's post gives me hope. After reading it, I've been motivated to learn
Lean, which has been an absolute pleasure so far. I have some experience with
mathematical proofs from my education, hobbies, and interests, so I am certainly
not a beginner in this area; however, I have regularly used LLMs in my
educational journey with Lean, and they have honestly been very helpful. The
rigidity of the Lean compiler, when used as corrective feedback for the LLM,
helps converge more quickly on a successful proof.

But still, a lot of the challenge comes from modeling the system. Abstracting
and modeling has always been more of an art than a science, and hopefully LLMs
can help the developer think more about the bigger picture and actually *prove*
their systems. But for me, I will continue learning Lean with the hope that I
can take these concepts (or hopefully the language itself) and apply them in my
day-to-day job writing software.

The **ultimate** end-goal is to *prove* my *haters* wrong, of course. I'm sure
Lean is capable of this.
