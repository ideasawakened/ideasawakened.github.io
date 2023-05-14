---
layout: post
title: "Patch misfire - try/finally fix to be re-released"
date: 2021-05-01 12:00:00 +0000
last_modified_at: 2021-05-01 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [10.4 Sydney, João Antônio Duarte, Dalija Prasnikar, Takeshi Arisawa, Vincent Parrett]
permalink: post/patch-misfire-try-finally-fix-to-be-re-released
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/RSP-33117.jpg
description: "One of the patches for Delphi 10.4.2 needs to be re-released. (Try/Finally fix on Non-Windows platforms.)"
---
As posted in [**yesterday's blog post**](https://www.ideasawakened.com/post/two-patches-for-rad-studio-10-4-2-sydney-released), there were two patches released for a few critical bugs in Delphi 10.4.2. One bug fix was [**RSP-33117**](https://quality.embarcadero.com/browse/RSP-33117) which involves an issue with TRY/FINALLY when there was a RAISE statement within the FINALLY section on non-Windows platforms.

In the comments section, **João Antônio Duarte** initially reported that the patch did not correct the issue and **Dalija Prasnikar** confirmed that it was still broken. **Takeshi Arisawa** also confirmed that the patch did not contain the fix for RSP-33117 so a re-release is needed: 
>This is a problem with the packaging. We will release another patch about this RSP-33117 issue.

![Delphi 10.4.2 patch misfire](/assets/blog/RAD-Studio-10.4-Sydney/RSP-33117-Patch-Misfire.jpg)

On the plus side, **Vincent Parrett** reported successfully testing the other major 10.4.2 patch:
>So far so good. I turned LSP back on, was able to do a full build (100+ projects in the group) - then debug the main project, exit, change some code, compile and debug again. The IDE is using a lot of memory though. Code navigation between projects is still broken for LSP (when only referencing dcp's) - so back to classic code insight it is.
