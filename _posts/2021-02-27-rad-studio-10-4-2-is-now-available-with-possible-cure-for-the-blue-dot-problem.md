---
layout: post
title: 'RAD Studio 10.4.2 is now available with possible cure for the "Blue Dot Problem"'
date: 2021-02-27 12:00:00 +0000
last_modified_at: 2021-02-27 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [David Schwartz, Discounts, 10.4 Sydney]
permalink: post/rad-studio-10-4-2-is-now-available-with-possible-cure-for-the-blue-dot-problem
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/RADStudio-LSP-Improvements-square.png
description: "RAD Studio 10.4 Sydney Update 2 was recently released with many new features and improvements.  One such improvement solves an old debugging issue where the blue dots in the editor do not match the source code lines."
---

If you are long-time Delphi user then you probably know what I mean when I reference the "**Blue Dot Problem**". For those that do not, I will start with an example.

The blue dots displayed in the editor gutter area represent executable lines of code that you can step through with the debugger and set breakpoints on. You should be able to quickly notice the Blue Dot problem in the image below. The blue dots should start on the first **begin** statement and there should obviously be a dot on the last **ShowMessage** line. There is also a trailing blue dot on empty line number 38. This is because the entire set of blue dots in this code snippet has been shifted one line down.

![Example of the Blue Dot Problem. Dots in the editor gutter are one-off from source code lines.](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-Blue-Dot-Problem-Example.png)

Why do these Blue Dot Problems occur? There are a wide number of issues that have been blamed over the years. However, one of the major underlying causes is due to the compiler only recognizing the "Windows" line end combination of Carriage Return+Line Feed (CRLF) pairs. Other major systems like Linux, macOS only use a single LF character to end a line of text so if you are sharing source files between these systems, then those files may end up being problematic for the Delphi compiler. Sometimes a file gets corrupted and is full of proper CRLF delimited text and only a line or two has a single CR or LF line ending character. (Note that this is a very old problem that has plagued many applications and not just the Delphi editor and compiler.) As **David Schwartz** pointed out on Facebook, this can also happen when you copy and paste code from something that only has linefeed characters (perhaps from an internet source.)

This is one of the [**many issues addressed**](http://docwiki.embarcadero.com/RADStudio/Sydney/en/New_features_and_customer_reported_issues_fixed_in_RAD_Studio_10.4.2) with the recent RAD Studio 10.4.2 Sydney release. There have been many reports of this Blue Dots Problem over the years with [**RSP-28156**](https://quality.embarcadero.com/browse/RSP-28156) being an example of the specific error above. The solution for many of these Blue Dot Problems is a new **Line Endings** configuration option to handle "**Inconsistent line endings**" with the default choice of "**Convert files with known extensions to CRLF**" This new setting is available in the **Tools**\->**Options** menu under the **User Interface**\->**Editor** section. You can currently select from the following choices:

-   **Preserve**
    
-   **Convert all files to CRLF**
    
-   **Convert file with known extensions to CRLF**
    
-   **Ask**
    

The current list of **Known extensions** include:

-   **pas;dpr;dpk;inc;dfm;xfm;fmx;lfm;nfm;dpkw;cpp;c;cc;hpp;h;hh;cxx;hxx;bpkw;i**
    

![New config screen in Delphi 10.4.2 for configuring Line Endings](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.2-New-LineEndings-Configuration.png)

With the introduction of 10.4.2, the number of times that the Blue Dots displayed in your editor are out of sync with the lines of source code should rapidly diminish! **Huzzah**! Kudos to Embarcadero for addressing this definitely sporadic, but infinitely annoying problem.

With nearly 1,000 issues addressed in RAD Studio 10.4.2, this is seemingly another **must-have release**. Sooner or later, with releases like this coming out of Embarcadero, Delphi may no longer be such a [**secret weapon**](https://www.ideasawakened.com/post/a-good-delphi-developer-is-usually-a-10x-developer)!

Since it's Delphi's 26th Birthday, there's a nice discount available if you purchase a new license or upgrade. Check out their [**specials page**](https://www.embarcadero.com/radoffer) for the latest details! It's time to **upgrade to Delphi 10.4.2**!

![Delphi 26th birthday](/assets/blog/Delphi-Programming/Delphi-Is-26-years-old.jpg)
