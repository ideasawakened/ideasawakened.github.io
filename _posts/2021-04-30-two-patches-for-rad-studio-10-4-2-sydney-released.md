---
layout: post
title: "Two patches for RAD Studio 10.4.2 Sydney released"
date: 2021-04-30 12:00:00 +0000
last_modified_at: 2021-04-30 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [10.4 Sydney]
permalink: post/two-patches-for-rad-studio-10-4-2-sydney-released
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.2-Patch1-and-Patch2-square.png
description: "Quick summary of the two patches released today for Delphi 10.4.2 including fixes for F2046 Internal Error and F2084 Internal Error which was occuring for some users of RAD Studio 10.4.2 Sydney."
---
As posted on Embarcadero's [**blog post**](https://blogs.embarcadero.com/rad-studio-10-4-2-general-patch-and-delphi-compiler-patch/), there were two patches released today that are important for many wanting to use C++ Builder or Delphi 10.4.2.

As discussed in a previous [**blog post**](https://www.ideasawakened.com/post/rad-studio-10-4-2-is-great-but-here-are-some-notes) here, 10.4.2 was a great release but there were a few important issues to be aware of. Two of the top issues seem to have been addressed by the patches released today.

## Delphi 10.4.2 Compiler Patch #1

### Quality Portal Issues Fixed

-   [**RSP-32768**](https://quality.embarcadero.com/browse/RSP-32768) **F2046 Internal Error - out of memory on large projects** This seems like the most widely experienced issue for many developers trying to use 10.4.2. The same projects are buildable (albeit slowly) with Delphi 10.4.1. _There are currently 39 votes for this issue on Quality Portal with 43 watchers._
    
-   [**RSP-33425**](https://quality.embarcadero.com/browse/RSP-33425) **F2084 Internal Error - sporadic build failures** This error occurs during compiling Win32/Win64 larger projects. The same projects are buildable without issue with Delphi 10.4.1. _There are currently 29 votes for this issue on Quality Portal with 33 watchers._
    
-   [**RSP-33232**](https://quality.embarcadero.com/browse/RSP-33232) **64-bit DLL Debugging broken with runtime packages** Cannot debug 64-bit dll in the IDE with runtime packages as the breakpoints are never hit. _There are currently 4 votes for this issue on Quality Portal with 5 watchers._
    
By default, a backup is stored in a folder named similar to: 
`C:\Program Files (x86)\Embarcadero\Studio\21.0\_patch-backup\2021-04-30 15.38.58\bin`

You can analyze the patch contents within the GetIt repository folder. On my system, the files for this patch can be found in the folder: `C:\Users\Darian\Documents\Embarcadero\Studio\21.0\CatalogRepository\10.4.2CompilerPatch-10`

## Delphi 10.4.2 General Patch #1

### Quality Portal Issues Fixed

-   [**RSP-33117**](https://quality.embarcadero.com/browse/RSP-33117) TRY..FINALLY block is broken for non-Windows platforms An embedded try/except with a raise statement after the except is the culprit according to a comment from **Dave Nottage** _There are currently 32 votes for this issue on Quality Portal with 29 watchers._
    
-   [**RSP-32951**](https://quality.embarcadero.com/browse/RSP-32951) is referenced in the blog post and in the patch notes, but that issue is a duplicate and is closed. The open issue is [**RSP-29696**](https://quality.embarcadero.com/browse/RSP-29696) C++64: Debugger fatal error: debug kernel not responding. The debug process willl be terminated. _There is currently 1 vote for this issue on Quality Portal with 7 watchers._
    
-   [**RSP-32939**](https://quality.embarcadero.com/browse/RSP-32939) Debugging of DLL with runtime packages is not possible at all. This must be another duplicate as this particular issue is marked as closed. (Or perhaps an undocumented related internal issue was closed.) _There are currently 3 votes for this issue on Quality Portal with 6 watchers._
    
-   [**RSP-32043**](https://quality.embarcadero.com/browse/RSP-32043) Fatal linker error: Type index XXX is bad in module YYY when using clang compiler _There are currently 3 votes for this issue on Quality Portal with 6 watchers._
    
-   [**RSP-33406**](https://quality.embarcadero.com/browse/RSP-33406) E2213 Bad packaged unit format _There are currently 0 votes for this issue on Quality Portal with 5 watchers._
    

As with the first patch, a backup is found in **Studio\\21.0\\\_patch-backup** folder. The patch contents are also in the GetIt repository folder, but in the **10.4.2GeneralPatch-10** subdirectory.

## Notes:

-   I'm currently using a locally managed network license and after each patch installation process, I was repeatedly asked to register my copy of Delphi. Eventually, I figured out that I had to go into the Advanced screen, click on my network license, and then click on the Cancel button to get the IDE to finish loading.
    
-   For those that do not utilize GetIt, the patches are not yet available in [**my.embarcadero.com**](http://my.embarcadero.com/) but will be soon.
    
-   After installing the patches, the version is still reflected as: **Embarcadero® Delphi 10.4 Version 27.0.40680.4203** within the About screen. You can verify installation by viewing the **Patches and Hotfixes** category in the GetIt Package Manager as displayed below:
    

![Screenshot showing Patch #1 and Patch #2 installed within GetIt Package Manager](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.2-Patch1-and-Patch2-installed.png)

Unfortunately, the other issues listed in my previous blog post on 10.4.2 remain unresolved. Perhaps an additional patch or two for 10.4.2 will arrive before the 10.5 release later this year.
