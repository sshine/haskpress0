# haskpress0

0th iteration of a minimalistic blog platform in Haskell.

Why not use a static site generator like Hakyll?

Because I actually want dynamic features and more flexibility at run-time.

I want a blog system that uses git and Markdown as its back-end, but I also
want a blog system that is flexible enough to host pages for friends, let them
update their pages without direct git interaction. I don't want to install
WordPress or PHP because of the security risk, but I want many of the
conveniences of those systems.

Design goals:

 - HTML templates so that a blog can be embedded with a website.
 - Dynamically add/modify pages and posts while site is running.
 - Express pages and posts in Markdown and store via git.
 - Edit pages either in files or in-page editor.
 - In-page editor should support Markdown and/or WYSIWYG.
 - WYSIWYG should be approximately as nice as Medium.com's.
 - In-page editing should git commit and push changes back.
 - Caching of in-page editor changes so no changes are lost.

This idea was inspired by [Dixonary's microcosmos][microcosmos], made with
Warp. I've went with [Spock][spock], which is made on top of Scotty, which is
made on top of Warp. In that sense it is considerably less lightweight, but
Spock is still considered a lightweight framework.

[microcosmos]: https://github.com/dixonary/microcosmos
[spock]: https://www.spock.li/
