---
title: Auth0 Elm Tutorial
author: Brendon A. Kay
tags: elm, auth0
---

While writing my post on SaaS I wanted to familiarize myself with Elm, which
would serve as my frontend in my SaaS example.

https://auth0.com/blog/creating-your-first-elm-app-part-1/
https://auth0.com/blog/creating-your-first-elm-app-part-2/

This is sort of an extended version of the quoter tutorial on the elm website.

# Why `elm`?
DSL for developing web _applications_. Similar to Haskell.

# Part 1

## Challenges
The tutorial is out of date so I had to upgrade Gulp and modify a few Elm
commands.

No `elm package install` command anymore. Instead it's `elm make`.

Arrays in the `gulpfile.js` had to be converted to `gulp.series()`. This turned
out to be sort of correct, but gulp still only sort of worked. Trying `webpack`
instead.

Eventually I had to abandon setting up `webpack`. JavaScript is not yet my
strong skill. I found this project https://github.com/simonh1000/elm-webpack-starter
and decided to use it as my template to complete the auth0 tutorial.

Hopefully I can learn more about the JavaScript ecosystem from this template.

Rather than going with `Html.program` I had to use `Html.document` for Elm
version 0.19

Also, it's `Http.post` instead of `Http.send`, which seems more appropriate to
me.

So far each change in 0.18 to 0.19 seems reasonable.

# Part 2
>
>Now we'll continue to build out our Chuck Norris Quoter app to add user registration, login, and make authenticated API requests with JSON Web Tokens. We'll also use JavaScript interop to persist user sessions with local storage.
