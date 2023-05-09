---
layout: post
title: "Subversion isn't dead - get a 1GB repo free forever"
date: 2019-11-22 12:00:00 +0000
last_modified_at: 2019-11-22 12:00:00 +0000
categories: [Tooling, SCM]
tags: [git, Subversion, Discounts]
permalink: post/subversion-isn-t-dead-get-a-1gb-repo-free-forever
published: true
image: /assets/blog/SCM/git/keep-calm-and-git-sucks.png
description: Free 1GB subversion repositories available from Helix TeamHub
---
Many people think the battle was won long ago and that GIT is the undisputed victor in the version control war. I imagine that everyone will agree that GitHub, GitLab, AWS CodeCommit, and Azure Devops offer great services for millions of developers. Heck even Bitbucket is dropping support for Mercurial, scheduled for June 1, 2020. However, I believe that GitHub's community and added value features were the main deciding reasons for GIT's "victory" and the tool itself.

In a word, GIT sucks. It's certainly popular, and it has many fans, but how many articles are out there that rant about how bad GIT really is? Here are just a few:

-   [https://enux.pl/article/en/2014-01-21/why-git-sucks](https://enux.pl/article/en/2014-01-21/why-git-sucks)
    
-   [https://merrigrove.blogspot.com/2014/02/why-heck-is-git-so-hard-places-model-ok.html](https://merrigrove.blogspot.com/2014/02/why-heck-is-git-so-hard-places-model-ok.html)
    
-   [https://stevebennett.me/2012/02/24/10-things-i-hate-about-git/](https://stevebennett.me/2012/02/24/10-things-i-hate-about-git/)
    
-   [https://ohshitgit.com/](https://ohshitgit.com/)
    

![Git Sucks T-Shirt](/assets/blog/SCM/git/keep-calm-and-git-sucks.png)

How many articles are out there explaining how to fix problems in GIT?

![Git Sucks diagram why](/assets/blog/SCM/git/GITSucks.png)

For those still protesting, take a quick look at [**Subversion vs Git: Myths and Facts**](https://svnvsgit.com/).

But truth be told, I'm using GIT daily now and the more you use it, the less it sucks. However, don't get me wrong, it still sucks.

I could rant further on the suckiness of GIT, however the main thrust of this blog post was a nugget discovered today that I thought deserved sharing. And that is a 5-user account with 1GB of repository storage for free from Perforce. This allows multi-repository hosting with your choice of Mercurial, Git, or SVN. While you can get a lot more free space at AWS with GIT (currently 50GB), the number of reputable vendors offering decent-sized SVN repositories for free is very limited these days, so this was a surprising find.

1GB of storage should easily handle nearly all small development projects and many medium ones as well. Upgrades are currently very reasonable at $19/user/year and you get an additional 1GB of storage per user. The service scales to thousands of concurrent users and terabytes of data.

[**Helix TeamHub**](https://www.perforce.com/products/helix-teamhub) includes source code repository management, and a host of other features like issue tracking, code reviews, wikis, and a host of webook integrations to popular tools like Jira, Basecamp, Jenkins, Slack, Trello, Zendesk, etc. There's a quick 3-minute overview video on [**YouTube**](https://youtu.be/yZ2b4XDoVCI).

For those that require it, Helix TeamHub is also available in an Enterprise on-premise version.

![Helix TeamHub features](/assets/blog/SCM/git/helix-teamhub-featurematrix.jpg)

Is this a GitHub killer? Probably not, but it has strong potential as a real alternative for small to medium business usage to migrate to the cloud with their tool of choice. It seems to be able to easily scale up for large companies (which are already Perforce's main customers.) This service allows all types of SVN proponents to keep fighting the battle!