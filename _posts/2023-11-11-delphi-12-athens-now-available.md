---
layout: post
title: "Delphi 12 Is Now Available"
date: 2023-11-11 12:00:00 +0000
last_modified_at: 2023-11-11 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [12 Athens, DelphiKB]
permalink: post/delphi-12-now-available
published: true
image: /assets/blog/RAD-Studio-12-Athens/Delphi12.png
description: "Delphi 12 has been released"
---

The latest major version of Delphi was released on November 7, 2023.  This has been another huge effort towards quality improvements with over 1,200 customer-reported Quality Portal issues closed with this release (along with hundreds of additional internal issues.)  The release name is "RAD Studio 12 Athens" and this is a major new release that is binary incompatbile with the previous Delphi 11. (See this [**blog post on binary compatibility issues**](https://ideasawakened.com/post/about-binary-compatibility-on-new-versions-of-delphi).)

![RAD Studio 12 Athens has been released](/assets/blog/RAD-Studio-12-Athens/RADStudio12NowAvailable.png)

The Delphi "Master Release Wiki" page on GitHub has been updated with a new [**wiki page dedicated to RAD Studio 12**](https://github.com/ideasawakened/DelphiKB/wiki/D29.ATHENS.12.0.0.0).  This wiki page contains a bunch of links related to this release.  If you know of any missing links, please contact me or create a Pull Request for me to merge.


## What's New in Delphi 12?

Their [**DocWiki page on Athens**](https://docwiki.embarcadero.com/RADStudio/Athens/en/What%27s_New) is quite extensive and covers much of what is listed here.

### A few really cool new features include:

- "Find in Files" now has an Exclude feature (ignore searching __history, __recover folders)
- New VCL FormTabsBar control
- New JSON Data Binding Wizard
- Offline GetIt features with loading packages from local folders
- LSP is supposed to be much better 
- The Delphi compiler now offers detailed information in case of a circular unit reference error. Expanding the error message now provides additional information regarding the actual sequence of unit references leading to the circular unit reference
- For TList and descendant classes, the "List Index out of bounds" error message was enhanced to include the index being used and the valid range (or the fact that the structure is empty). It includes the class name.
- The TForm class has a new ShowInTaskbar property, controlling if a form should have a matching entry in the Windows taskbar, even if it’s not the main form.
- Did you know you can access GetIt via a browser?  [https://getitnow.embarcadero.com/](https://getitnow.embarcadero.com/)

### Some breaking changes include:

- Floating-Point Exceptions are now disabled by default on all platforms. (_Default8087CW_ changed on Win32 but it is also changed across all platforms)
To restore previous behavior, add this line to your DPR before the Application.Initialize statement: `System.Math.SetExceptionMask([exPrecision, exUnderflow, exDenormalized]); // Enable exInvalidOp, exZeroDivide, and exOverflow exceptions`   You can manually re-enable floating point exceptions in an application at the thread level.
- NaN operations changed 
- Ensure you select "Modeling" during installation or the Code Formatter will not be available (they are deprecating all the old DotNet technology within the IDE.)
- VCL font size changes (when calling TFont.Assign the DPI and the IsScreenFont value is taken into consideration).  Code like this `Font.Height := MulDiv(Font.Height, M, D)` should be changed to `Font.ChangeScale(M, D, isDpiChanged)`
- NativeInt is now a "Weak Alias" which mainly affects method overloads
- If you create a form with another form as it's Parent, then that form displays within the boundaries of the parent (part of their MDI and VCL forms overhaul)
- The compiler now uses the XML documentation output directory for the XMLDoc artifacts as expected. It was using the C++ .hpp folder instead.

### Other notes:

- RAD Studio 12.0 adds support to SQLite 3.42 while maintaining the option of using SQLite 3.31.1 with FireDAC encryption (FDE). Since version 3.42, SQLite dropped the mechanism FireDAC uses for encryption support, so it can no longer be used. (The default linkage mode is static with SQLite version 3.31.1 with FDE.)
- The [**wiki page dedicated to future releases**](https://github.com/ideasawakened/DelphiKB/wiki/Future-Releases-for-RAD-Studio-and-Delphi) was updated with a few notes from the release webinar and the beta links moved to the RAD Studio 12 page.  (Note that Embarcadero no longer releases public roadmaps.)
