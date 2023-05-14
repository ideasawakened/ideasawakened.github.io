---
layout: post
title: "Overflow and Range checking are now enabled by default for Debug builds"
date: 2021-11-09 12:00:00 +0000
last_modified_at: 2021-11-09 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [11 Alexandria]
permalink: post/overflow-and-range-checking-are-now-enabled-by-default-for-debug-builds
published: true
image: /assets/blog/RAD-Studio-11-Alexandria/Overflow-and-Range-Checking-square.png
description: "A new feature in RAD Studio 11 Alexandria is changing the default values for Overflow and Range checking on debug build configurations."
---
I did not notice this new feature of RAD Studio 11 Alexandria until today. It has been a popular request for a few years now on Quality Portal with [**RSP-16751**](https://quality.embarcadero.com/browse/RSP-16751) collecting 76 votes since January 2017. From a quick search, it is in the Top 25 of all time for votes received and was in the Top 10 of open issues sorted by vote count before this version was released. Since it was such a popular issue, it is interesting to me that I have not seen many references to the change, so I decided to do a quick blog post.

## New Feature in RAD Studio 11 Alexandria

For new projects created in RAD Studio, two compiler options have their default values changed. These options are listed under the **Runtime errors** section and they both now are **True** by default as seen below:

![Screenshot of build config options with Overflow and Range checking enabled](/assets/blog/RAD-Studio-11-Alexandria/Overflow-and-Range-Checking-are-now-enabled-by-default-in-RAD-Studio-11-for-debug-builds.png)

[**Overflow checking**](https://docwiki.embarcadero.com/RADStudio/en/Overflow_checking_(Delphi)) and [**Range checking**](https://docwiki.embarcadero.com/RADStudio/en/Range_checking) are now enabled by default for Debug build configurations on new projects created in RAD Studio (for all compilers.) Note: if you open an existing project, your current project settings will not be changed. This change only affects new projects created.

This change seems to be much more appropriate for Debug build configurations but I imagine there may be some complaints. These can definitely slow down your debug builds and force you to recognize errors that have not been detected before. But to be fair, these are **_debug builds_** and should be treated as such.

**Kudos to Embarcadero for addressing a long-standing open item with many votes!** They have been attempting to do a lot of clean up lately and it is much appreciated. However, I am actually glad they are not implementing some of the Top 10 open items. Things like [**RSP-13340**](https://quality.embarcadero.com/browse/RSP-13340) for Expanded Helpers and [**RSP-12100**](https://quality.embarcadero.com/browse/RSP-12100) for Ternary Operators can certainly wait until after the tooling catches up with inline variables. (Please vote for [**RSP-22089**](https://quality.embarcadero.com/browse/RSP-22089), [**RSP-33176**](https://quality.embarcadero.com/browse/RSP-33176), [**RSP-32507**](https://quality.embarcadero.com/browse/RSP-32507), and [**RSP-23096**](https://quality.embarcadero.com/browse/RSP-23096).)
