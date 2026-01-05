---
title: Slow is Smooth, Smooth is Fast
author: Brendon
tags: opinion, philosophy, process
---

## Starting Fast

I was motivated to write this post by today's popular belief that if you are
not rapidly adopting the latest technologies and prioritizing time-to-market
over food and water then you are a loser and should feel bad (and you will burn
with the rest of the losers). Said another way, in today's market you **must
move fast**. I believe this mindset can actually be harmful to the success of a
project. That being "bleeding edge" will only bleed you out. Instead, I
subscribe to the saying, "slow is smooth, smooth is fast", and I'd like to share
my thoughts on how this has worked for me and many other people.

Once upon a time in my career I was called out by a senior colleague for being a
"shotgun" programmer. Now, they didn't say this to be mean; they are
even a good friend. This was some feedback I received during a one-on-one
mentor session. They pointed out that when I picked up a bug ticket, or a small
feature task, I tried to turn it around as quickly as possible and that caused
me to merge and ship a few pull requests that didn't fully fix the bug or missed some
acceptance criteria. For small tasks I thought I could "bang out", I was
prioritizing speed over quality, and I was certainly not pondering the broader
implications of my actions.

## In the Literature

In John Ousterhout's book [*A Philosophy of Software
Design*](https://web.stanford.edu/~ouster/cgi-bin/book.php) he talks about the
"tactical tornado" programmer. I think my colleague could have used this term
instead of "shotgun" programmer. A tactical tornado is when a programmer
responds to tasks tactically, rather than strategically. The person is not
thinking about the larger picture, not pausing to reflect on if this quick-fix
is worth it in the long run. 

For further clarification, here is how Ousterhout defines the tactical vs
strategic mindsets. Here is the tactical one.

> In the tactical approach, your main focus is to get something working, such as
> a new feature or a bug fix. At first glance this seems totally reasonable:
> what could be more important than writing code that works? However, tactical
> programming makes it nearly impossible to produce a good system design.
> The problem with tactical programming is that it is short-sighted... As a
> result, planning for the future isn’t a priority.

And the strategic one.

> Thus, you should not think of “working code” as your primary goal, though of
> course your code must work. Your primary goal must be to produce a great
> design, which also happens to work. This is strategic programming. Strategic
> programming requires an investment mindset. Rather than taking the fastest
> path to finish your current project, you must invest time to improve the
> design of the system.

I completely resonate with Ousterhout's strategic / investment mindset. If you
haven't checked out this book, it's short and sweet, and it explains more about
thinking strategically and having an investment mindset when it comes to your
projects. I have always been attracted towards slow, steady growth. And when you
are investing in yourself, you will need to reflect on what's working and what
isn't.

When I reflected on why I was defaulting to a tactical mindset, I think the most
obvious answer to me was that I had learned these bad behaviors from working in
certain environments. The one that stands out to me the most was working in a
start up early in my career. These conditions were chaotic at best, and the
main goal was to keep customers happy. I don't blame the startup for my bad
habits. Business is business. It would take me a while before I would
learn that thinking and acting strategically would provide for a much calmer
and constructive work environment, and that at first I would have to *slow down*.

## Navy SEAL Badasses

["Slow is smooth, smooth is
fast"](https://www.navyseal.com/slow-is-smooth-smooth-is-fast/ ) comes from the Navy
SEALs. In the article linked:

> This phrase isn't just about being slow or fast; it's about finding a rhythm
> that balances precision and pace, ultimately leading to swifter progress.

So, it's not just about slowing down, it's also about being smooth. I think of
this like practicing an instrument or a dance, some kind of skill that takes
time to learn; you need to go slow at first. They say that when learning the
piano, if you do not go slowly at first, you will make mistakes trying to do a
piece fast, and those mistakes will be engrained in your motor skills. This is
made even worse by the fact that they are harder to unlearn than learning
them in the first place.

This is not to say you should optimize on slowness and just stop altogether.
There is a balance. Work needs to be done. Don't take what I'm saying here to
the extreme. Start by going slow, making mistakes (early and often), iterating
on these, and building up confidence in each repetition. In the next section I
will give a few examples of how I started to go slower and gain smoothness.

## Going Slow and Smooth

A tough thing for me to learn at first was Continuous Deployment (CD). This was a new
and scary concept. Deploy to production during working hours? Crazy. Of course,
I was very precautions initially, writing many tests and manually validating
every step. Starting slowly though, with panic in my heart every time I hit the
"Deploy" button in our internal web GUI, after I got a few successful deployments
under my belt I became more confident with our CD system. Eventually I was
shipping code faster than I ever had at any previous org.

Another experience I had with starting slow and going smooth is with product
ideation. I once worked on a greenfield project and we started with a very
focused set of features on the initial release. We prototyped it first and when
we saw how customer's responded to the prototype we froze that project and
started fresh with the narrow set of features initially scoped. In this new
effort, a lot more time was spent up front on project code architecture,
database modeling, UI/UX,and internal platform integration (i.e. how was this
new product going to fit in to our suite of products). We were starting slow,
and this meant that feature development was not prioritized initially.

Eventually we launched, and once we saw what was working and what wasn't with
our customers, we began thinking about next steps. This product became a
template for new projects that we might bring in down the road. Features became
very straight forward to iterate on, with many development tasks only requiring
us to define an API schema, some user expectations, and the rest fell into
place. Many of our engineers wanted to work on this project because of its
quality of code.

In the examples I have provided there is the common theme where we went slow to
allow ourselves to properly learn and adapt to the thing we were trying to
accomplish. Slow and smooth repetitions allowed us to build confidence.

And of course, practice makes perfect. Do not expect that just because you are
going slow that you will not fail. Again, you need to fail **early and often**.
Go slow so that these failures are not catastrophic. Go steady so that you are
in control.

## Stay Smooth

People say "that was a smooth landing" when the plane lands comfortably at the
destination, with very little turbulence (this is in fact never the case coming
in to Denver). "Smooth sailing" is used to describe all sorts of good
situations, usually when things are going well, with little friction. Or I've
heard people describe engines or machines "running smoothly" when they are
operating optimally. I think it's easy to connect "smooth" with being good, but
it's not immediately obvious that going slow leads to smooth. Start slow, build
on the basics, practice, and you will find in time that you are operating
smooth and fast.

In this post I discussed how, at first, I did not know going slowly could
benefit me and the projects I was working on. I talked about my first
introduction to this concept, coined by the Navy SEALs. When my colleague told
me to slow down, to think strategically. And how maybe some of these bad habits
- reacting tactically - were reinforced within certain working environments.
Lastly, I gave a few examples of how, when I finally slowed down, I could begin
to make steady progress and build on solid foundations.

Perhaps this is all a retelling of the tortoise and the hare but I digress.
