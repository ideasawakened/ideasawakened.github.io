---
layout: post
title: "Code count badges for your GitHub repositories"
date: 2020-01-08 12:00:00 +0000
last_modified_at: 2020-10-27 12:00:00 +0000
categories: [Tooling, SCM]
tags: [GitHub, Tokei, JCL]
permalink: post/code-count-badges-for-your-github-repositories
published: true
image: /assets/blog/GitHub/Badges-For-Your-Repository/JediCodeLibrary.png
description: Add live badges to your GitHub repositories displaying lines of code
---
**"Badges? We ain't got no badges. We don't need no badges. I don't have to show you any stinking badges!"** Ok, I got that out of the way. If you don't know that popular [**reference**](https://en.wikipedia.org/wiki/Stinking_badges), I suggest you watch [**The Treasure of the Sierra Madre**](https://www.imdb.com/title/tt0040897/).

![Lines of Code bade for JEDI Code library on GitHub](/assets/blog/GitHub/Badges-For-Your-Repository/JediCodeLibrary.png)

This is a quick post to demonstrate a very simple edit to your GitHub repository's **readme.md** page which will reveal to visitors how many lines of code are available. Lines of code? We don't need no Lines of Code. I don't have to show you any stinking... ok, let's not have that old argument. Let's just assume that Lines of Code does mean _something_ to _some people_, for _some reason_.

First off, you certainly have choices. There is more than one free line of code badge service for use with GitHub (and GitLab.) I settled on **Tokei** which is a code counter written in Rust which claims [**support for 170 languages**](https://github.com/XAMPPRocky/tokei#supported-languages), including our favorite Pascal/Delphi.

In only takes a few seconds to add your badge. Simply visit your repo on GitHub and edit your _readme.md_ page directly on the site (or edit locally and push the change) You'll typically want to add an image link somewhere near the top line of your page. The image link _source_ will refer to the Tokei webservice which will scan your repo and report back the lines of code in a badge displayed as an SVG image. (Note that the very first image view can take a few seconds to show up, but it's amazingly quick even for large repositories.)

For example, a popular repository on GitHub for Delphi users is the **JEDI Code Library** found at [**https://github.com/project-jedi/jcl**](https://github.com/project-jedi/jcl). To create a badge, you'll follow the URL scheme for Tokei:
````
https://tokei.rs/b1/{host: values: github|gitlab}/{Repo Owner eg: XAMPPRocky}/{Repo name eg: tokei}
````

So, in order to add a Tokei code badge to the Jedi repo, you would insert this simple markdown image reference:
````markdown
![](https://tokei.rs/b1/github/project-jedi/jcl)
````

Which would yield a code badge like this:

![Example badge](/assets/blog/GitHub/Badges-For-Your-Repository/ExampleBadge.png)


The image is SVG-based and for the super curious, an example of the raw content returned is:
````xml
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="140" height="20">
  <linearGradient id="smooth" x2="0" y2="100%">
    <stop offset="0" stop-color="#bbb" stop-opacity=".1"/>
    <stop offset="1" stop-opacity=".1"/>
  </linearGradient>

  <mask id="round">
    <rect width="140" height="20" rx="3" fill="#fff"/>
  </mask>

  <g mask="url(#round)">
    <rect width="90" height="20" fill="#555"/>
    <rect x="90" width="50" height="20" fill="#007ec6"/>
    <rect width="140" height="20" fill="url(#smooth)"/>
  </g>

  <g fill="#fff" text-anchor="middle" font-family="DejaVu Sans,Verdana,Geneva,sans-serif" font-size="11">
    <text x="45" y="15" fill="#010101" fill-opacity=".3">lines of code</text>
    <text x="45" y="14">lines of code</text>
    <text x="115" y="15" fill="#010101" fill-opacity=".3">464.4K</text>
    <text x="115" y="14">464.4K</text>
  </g>
</svg>
````

Very cool! With one simple line of markdown, you can have a live line-of-code badge on your repo. (I just created pull request [#77](https://github.com/project-jedi/jcl/pull/77) asking JCL to update their readme with this badge.)  
*Update:* the PR was merged on Oct 27, 2020 and now the JCL code badges are live on GitHub.com.

Here are some live badges (captured in static image form for use with this blog article) for a few other popular Delphi related GitHub repos:


![Code Bades for some Delphi libraries on GitHub](/assets/blog/GitHub/Badges-For-Your-Repository/SampleCodeBadges.png)

By default, the badge displays lines of code. You can have multiple badges, each with a **category** parameter. Current category choices available include: _code_, _blanks_, _files_, _lines_, and _comments_.

Here are all of the available live Tokei badges for the JCL repo. (_Note: I captured an image of the live code badges to speed up display of the blog article._)

![Code Bades for JCL Repo](/assets/blog/GitHub/Badges-For-Your-Repository/Badges-For-JCL-Repository.png)

It couldn't get much simpler to add live badges to your GitHub repos. And thankfully this particular tool supports Delphi code, which is sometimes difficult to find these days.