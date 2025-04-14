# Brendon A. Kay's Hakyll site

This is my personal website and blog. Here I plan to write about software
engineering and math related topics.

## Site commands

I prefer not to use `stack`. So I call `cabal new-run` for all `site`
commands. I am not installing `site` because I am currently using NixOS and
installing things via cabal gets a little fussy. That's likely a user problem.

Watch the site locally:

```Bash
cabal new-run site watch
```

## FAQ

Sometimes shit breaks. Running `cabal new-run-site rebuild` has fixed things int
he past.

## TODO

- [ ] Make a script that publishes posts.
- [ ] Add better code styling CSS.
- [ ] Add tags.
  - https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html

## Blog post ideas

- Math notation in Markdown
- Vimwiki
- Hakyll
- Diceball Haskell
- Servant and SQLite
- Adding authz to Diceball
- HTMX
- Nix
- Servant (maybe combined with HTMX)
- Amazonka
- Algebraic path finding
- "The importance of book clubs in Engineering"
- "The acquisition all-hands is decadent and depraved"
  - Homage to Hunter Thomson's The Kentucky Derby...
  - How depraved people are when a company is purchased.
- "Business logic not business effects"
  - The importance of "imperative shell, functional core".
