---
layout: post
title: "Update 3 for RAD Studio 10.3 Rio has been released!"
date: 2019-11-21 12:00:00 +0000
last_modified_at: 2019-11-21 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [10.3 RIO]
permalink: post/update-3-for-rad-studio-10-3-rio-has-been-released
published: true
image: /assets/blog/RAD-Studio-10.3-Rio/RAD-Studio-10.3.3-Rio-Released.jpg
description: 10.3 Rio has been released with Android 64-bit support as well as iOS 13 and macOS Catalina
---
![RAD Studio 10.3.3 RIO Desktop image](/assets/blog/RAD-Studio-10.3-Rio/10.3.3-RIO-1200x628.jpg)

This is likely the last update for 10.3 Rio before we get a new major release sometime in 2020. That is, besides hotfixes/patches of course - and there's already a [**patch**](https://cc.embarcadero.com/item/30899) available for fixing a problem of requiring an app restart when a user switches between light and dark themes.

10.4 may include new IDE tooling with Language Server Protocol support, VCL high DPI Styles, unified memory management (disable ARC on all platforms), unified online/offline installer using GetIt, Metal 2 support on Apple, and a redesign of livebindings. As usual, there's no announced target date but given past release dates I imagine it will be _around_ March 2020.

Meanwhile, 10.3.3 includes the highly anticipated **Android 64-bit support**, as well as support for **iOS13** and **macOS Catalina** thereby making Delphi's cross-platform support so much stronger. What other dev environment can come close to the rapid native development across so many platforms offered by Delphi today?

![Android 64-bit now supported in Delphi 10.3.3](/assets/blog/RAD-Studio-10.3-Rio/Android-64bit-provisioning.png)

Unfortunately, C++ Builder users will have to wait for macOS Catalina support but for the Delphi developers, it's available today.

10.3.3 has just been added to our GitHub hosted wiki page covering a [**list of Delphi releases**](https://github.com/ideasawakened/DelphiKB/wiki/Delphi-Master-Release-List). It's definitely a work in progress, but currently contains a number of links for the various 10.3 releases. After Embarcadero's (great) change to release multiple major updates per year, along with the number of patches rising, it is starting to become quite a chore to keep track of everything. In the year since 10.3 was released on 2018.11.21, there has been three major releases, nine patches, and two new bundled offerings. Embarcadero has definitely been pushing hard to expand Delphi's functionality and platform support!

The wiki has a full page on [**10.3.3**](https://github.com/ideasawakened/DelphiKB/wiki/D26.RIO.10.3.3.0) that includes multiple links to blog posts, downloads, wiki pages, and YouTube video. This page was created a month ago and still contains links to a few Beta announcements involving this release. I intend to add a child page for each patch as they become available.

Some 10.3.3 interesting features to inspect as soon as the download completes:

-   InterBase 2020 developer edition now included
    
-   HTTP Client library now has connection timeout support
    
-   IDE corrections, including font size overrides
    
-   Enterprise Connectors from CData (claim by [**entering serial number here**](https://reg.codegear.com/srs6/promotion.jsp?promoId=561))
    

With these new platforms in 10.3.3 it is another large step forward in the history of Delphi. However, one problem with all these changes is the repeated requirement to uninstall/reinstall the product, or to manually apply patches. There's a potential fix coming next year in 10.4 with expected enhancements to GetIt. Meanwhile, an internal installer tool has reached the "works on my machine" stage which introduces the ability to update to the latest update release in-place without an uninstall and without using the provided installer (which is extremely slow to complete.) If the projected 10.4 GetIt functionality doesn't solve this update problem, then we may release this tool for others to use. Most developers that I know work on virtual machines. We need the ability to quickly tear down and rebuild new dev machines on demand. In addition, the dev environment should be versionized along with the products being produced so you can reproduce the same dev environment utilized to build the binary releases. While the repeated updates are great, the multi-hour uninstall/reinstalls of Delphi for each release needs a solution. This doesn't include the pain in managing third party components. Besides GetIt, there are at least four other Dependency Managers for Delphi out there, which we'll cover in a future blog post.

![Delphi 10.3.3](/assets/blog/RAD-Studio-10.3-Rio/DX10_3_3.png)