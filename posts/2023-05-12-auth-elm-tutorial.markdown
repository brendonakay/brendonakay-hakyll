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

I got seriously hung up on this part. This involved using a function `http.send`
that has been removed in Http 2.0.0. I got hung up on how to return the right
type for the `Cmd`.

In order to keep moving forward with elm and JWT (authentication) I had to find something easier.
I found git@github.com:simonh1000/elm-jwt.git which comes with a nice `node`
example.

This new repo works well and it's up-to-date on 0.19. Yeehaw. I also like (the
quick explanation about JWT
decoding)[https://github.com/simonh1000/elm-jwt/tree/master#decode-a-token]

# Conclusion
This was a painful lesson in how best to learn new things. Don't try to learn
two new things at once. It's usually better to go straight to the source, i.e.
Elm docs, JWT docs. However, I did learn more about the JavaScript ecosystem
with `webpack` and `npm`. I also learned how important it is to find current
material on the research subject.
