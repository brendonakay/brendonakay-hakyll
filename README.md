# Brendon A. Kay's Hakyll site

This is my personal website and blog. Here I plan to write about software
engineering and math related topics.

## Site commands

I prefer not to use `stack` so I call `cabal new-run` for all `site`
commands. I am not installing `site` because I am currently using NixOS and
installing things via cabal gets a little fussy.

Watch the site locally:

```Bash
cabal new-run site watch
```

## FAQ

Sometimes shit breaks. Running `cabal new-run-site rebuild` has fixed things int
he past.

## TODO

- [ ] Make a script that publishes posts.
- [ ] Add tags.
  - https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
