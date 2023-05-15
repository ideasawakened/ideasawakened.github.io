---
layout: post
title: "Unofficial Delphi LSP fix for 11.2 is now available"
date: 2023-01-12 12:00:00 +0000
last_modified_at: 2023-01-13 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [11 Alexandria, Bruneau Babet, David Millington, Undocumented]
permalink: post/unofficial-delphi-lsp-fix-for-11-2-is-now-available
published: true
image: /assets/blog/RAD-Studio-11-Alexandria/Delphi-11.2-SmallUpdate.png
description: 'An update for Delphi LSP issues in RAD Studio 11 Alexandria has been informally released.  Links to the update are included in the blog post'
---
For users that have upgraded to RAD Studio 11.2, there have been some annoying problems with CodeInsight regressions with the LSP functionality randomly failing. The 11.3 beta process is ongoing (if you have the Premium Update Subscription you can join the beta, see the [**Embarcadero blog post**](https://blogs.embarcadero.com/premium-update-subscription-customers-invited-to-join-rad-studio-malawi-beta/) from last month.) As part of Embarcadero's quality-focused 11.3 beta process, they have been attacking the LSP failures and informally released a small update available on their Quality Portal.

![Informal update available for Delphi 11.2](/assets/blog/RAD-Studio-11-Alexandria/Informal-Update-Available-For-Delphi-11.2.png)

This has not been officially announced by Embarcadero, but the information is available to all registered users and not part of a secret beta-only update. The Quality Portal issue is [**RSP-39812 LSP and Code Completion not working properly**](https://quality.embarcadero.com/browse/RSP-39812) and **Bruneau Babet** attached a file in a comment which contains the latest DelphiLSP.exe and dcc32280.dll files for Win32 usage. Shortly afterwards, another file was updated that contained Win64 support based on a request. This particular QP issue has been getting some attention as it was created less than three months ago and already has 110 votes and 86 people watching for changes and has accumulated 86 comments so far.

Embedded within one of those 86 comments is [**this link**](https://etsfftp01.embarcadero.com/?u=kXCp&p=cMga&path=/DelphiLSP.zip) for the updated Win32 version and [**this link**](https://etsfftp01.embarcadero.com/?u=zBEX&p=BHyZ&path=/dcc64280.zip) for the Win64 version. As stated above and also within the comments - this is an **informal update** and **no support is provided** for its usage. These files contain fixes for all known CodeInsight regressions when something is working in version 11.1 but fails in the 11.2.

One nice follow-up comment states:

````
For me, it works much better, no need to reload the project. (original 11.2 LSP worked 2,3 times, then the compilation get ultra slow + code insight did not work any more)

Code completion seems to work as well.

Win32, Huge old project, with many 3rd party components.

Thank You!
````

This is an unconventional response and is very nice to see from Embarcadero support! Bruno is requesting feedback in the ticket - either positive or negative. So, update your 11.2 with this fix and let him know how it works! Add a comment to the ticket and also hit the Vote link while you are there if you are interested in LSP related fixes.

## UPDATE 1
See **David Millington**'s response on Facebook. I will reproduce it here:

````
Be VERY careful using this. It is provided on QP for the benefit of the people involved in that bug report, to assist testing. It is \_not\_ an unofficial or "informal update". You are using a test DelphiLSP.exe file and test compiler DLLs which may have bugs, version incompatibilities, etc. Do not use this fix on a build machine for anything that gets to customers, or a machine that you cannot roll back to a pre-applying-fix state, including all DCUs etc built after applying the fix.

For example, the comments in the bug report say, > "...not setup to run validating tests against a copy of R11.2 w/ these binaries updated" > "the usual caveats apply"

This is NOT intended for production use but to assist with verifying our in-progress fixes with the assistance of those people who are involved in the bug report. It is NOT intended for wider scope or usage.

I know it's a really severe issue and I really understand the keenness for anything that addresses it, but we are working hard on a real fix: please, if possible, wait for that.
````

## UPDATE 2

**Bruneau Babet** added to the comments on the ticket:

````
...there are known scenarios under which LSP navigation will fail. I can list a few that I recall:_
1.  if a project turns off 'Symbol Reference Info' (i.e. $Y-).   
2.  If the symbol is in another project in the Project Group - this is reported via RSP-28457.
3.  If you are the declaration point: the compiler has not yet seen the definition. This is reported via RSP-29415.
4.  Overloads - RSP-36456, RSP-37457.
5.  Forward Decl - RSP-34762
6.  Identifiers w/ Unicode characters - RSP-12523
7.  Private Types - RSP-36542, RSP-37508.
````

As you can see from Bruneau's comments, there is still much work to do on LSP. I am hoping for a much improved LSP in 11.3 when it is released!