---
layout: post
title: "RAD Studio 10.4 Sydney is out - a few tweaks required and Community Edition notes"
date: 2020-05-29 12:00:00 +0000
last_modified_at: 2020-06-10 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [David Millington, Marco Cantu, Sarina DuPont, DelphiKB, Embarcadero, 10.4 Sydney]
permalink: post/rad-studio-10-4-sydney-is-out-a-few-tweaks-required-and-community-edition-notes
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/Rad-Studio-10.4.jpg
description: RAD Studio 10.4 Sydney released and it's a fantastic new version.  However, there's already a patch and a couple manual adjustments needed. 
---
RAD Studio 10.4 Sydney has been released, and it's the biggest release in many years containing many improvements across the board.

![RAD Studio 10.4 Sydney](/assets/blog/RAD-Studio-10.4-Sydney/RAD-Studio-10.4-Sydney.jpg)

With such a large release, there was bound to be a few minor annoyances. We'll cover a few items that have come up so far that you will run into after installing 10.4:

![Completed screen shot](/assets/blog/RAD-Studio-10.4-Sydney/RAD-Studio-10.4-Sydney-Operation-Completed-Screen.jpg)

## The first patch is available

As **David Millington** [**posted**](https://community.idera.com/developer-tools/b/blog/posts/rad-studio-10-4-patch-1-missing-files-and-c-debugging---and-a-new-way-to-install-patches) on the Developer Tools Blog, they have released RAD Studio 10.4 Patch 1. This mostly corrects for some missing files in the installation. It also updates the defaults.xml and CodeGear.Deployment.Targets build files.

You could download the patch manually from CodeCentral and apply it manually like hotfixes were handled in the past (see our [**wiki page for the patch details**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.1).) You need to be aware that the IDE won't be able to detect that this patch was installed if you apply it manually.

A better approach is to rely on the new feature of the IDE with integrated patch management! The Welcome Page will display a notification that a patch is available allowing you to leverage GetIt to easily download and install the patch. For users of the Web Installer, this works great:

![RAD Studio IDE informs you of a patch being available](/assets/blog/RAD-Studio-10.4-Sydney/10.4-patch-welcome-screen-darkmode.png)

## Installing from ISO keeps the Catalog Repository offline

If you installed RAD Studio from the ISO image, then you will have a problem getting the first Patch through GetIt as the Catalog Repository is left in offline mode when using the offline ISO based installer. Below is an example screen shot of this issue:

![GetIt Error - offline mode](/assets/blog//RAD-Studio-10.4-Sydney/10.4-Sydney-GetIt-Error-Offline-Mode.png)

Thankfully, as David mentions in his blog post above, there is a quick and easy fix. From the command line, run:

````shell
GetItCmd.exe -c=useonline
````

Note that you can use the parameter "useoffline" to later go back offline, if desired.

For the curious types, from a user **Kostya** on a the Dev Tools forum [**post comment**](https://community.idera.com/developer-tools/general-development/f/rad-studio-general/72358/installed-10-4-getit-can-t-install-anything), this correlates to the registry key:

> Computer\\HKEY\_CURRENT\_USER\\SOFTWARE\\Embarcadero\\BDS\\21.0\\CatalogRepository

with the "ServiceKind" value needing to be "Online" 

This action is documented in the **Release Notes for 10.4 Sydney** on the [**docwiki**](http://docwiki.embarcadero.com/RADStudio/Sydney/en/Release_Notes#Offline_Installer).

![GetIt command](/assets/blog/RAD-Studio-10.4-Sydney/10.4-Sydney-GetIt-Cmd.png)

## RAD Studio 10.4 Demos need to be updated

As **Marco Cantu** points out in his [**blog post**](https://community.idera.com/developer-tools/b/blog/posts/updating-rad-studio-10-4-sample-to-fix-lf-issue) in the Dev Tools community, there was a configuration error in the sample repository. The demos were configured with Unix-style (_LineFeed_ only) rather than Windows-style (_Carriage Return and LineFeed)_ as the RAD Studio IDE expects. The source repository has been corrected so you simply issue a **svn update** command in the Samples folder to update your copy.

This is a screen shot of the revision in the SVN Log:

![SVN Log](/assets/blog/RAD-Studio-10.4-Sydney/10.4-Delphi-SVN-Log.png)

## Waiting on some components to support 10.4 Sydney

There is a growing list of tools and components which support RAD Studio 10.4 Sydney. You can see a list from Code Partners [**here**](https://code-partners.com/third-party-support-for-rad-studio-10-4/). Unfortunately, a few tools provided by Embarcadero in recent versions were not included with the initial Sydney release including: [**Konopka Signature VCL Controls**](https://community.idera.com/developer-tools/b/blog/posts/konopka-vcl-controls-radiant-shapes-now-part-of-the-berlin-bonus-pack), [**Bookmarks**](https://community.idera.com/developer-tools/b/blog/posts/new-productivity-tooling-in-rad-studio-10-3-1-bookmarks), or [**Navigator**](https://community.idera.com/developer-tools/b/blog/posts/new-productivity-tooling-in-rad-studio-10-3-1-navigator) IDE experts. You'll have to wait a little while before these become available to install with GetIt. The only estimated date offered is "_as soon as possible_."

## 10.4 Community Edition not yet available

A comment made by **Sarina DuPont** on a Dev Tools [**blog post**](https://community.idera.com/developer-tools/b/blog/posts/rad-studio-10-4-now-available-learn-more) states

> 10.3.3 versions of Delphi and C++ Builder Community Edition remain available for download. 10.3.3 was an excellent release and Community Edition users can perfectly well continue to work with that release. 

> The majority of customers who need the best performance, quality and features should be able to purchase the latest release. Our paying customers deserve a premium experience. That, coupled with increased non-compliant usage of Community Edition (which we're actively addressing with our legal team), has resulted in delaying a new release of CE. Over the coming months, we'll determine when a 10.4 version of CE will be released. 

The RAD Studio 10.3.3 version is still available so anyone can download the [**Community Edition**](https://www.embarcadero.com/products/delphi/starter) for free today. It just won't be the latest 10.4 release, which is a little odd, but they are apparently sorting it out internally. Apparently there are some that are not following the requirements as detailed in the [**FAQs**](https://www.embarcadero.com/products/delphi/starter/faq). As the saying goes, "a few bad apples spoil the barrel."

## TMS Academic License

It's not all bad news for Community Edition users - did you see the new [**TMS Academic**](https://www.tmssoftware.com/site/academic.asp) licensed product offering? These are fully functional and fully free versions of TMS Delphi products for students and teachers for non-commercial use. When you pair this with the Community Edition, you have an **extremely powerful free offering** as the TMS VCL UI Pack has over 600 VCL controls targeting the Win32 and Win64 platforms. (More TMS products will get an academic licensed version over the coming months.)

![TMS Academic license](/assets/blog/Delphi-Programming/TMS-Academic-Getting-Started.png)

## Summary

I have switched all new development to my 10.4 Sydney virtual machine and I find it to be a refreshing interface, quick to use, and the new [**LSP-based Code Insight**](https://community.idera.com/developer-tools/b/blog/posts/new-in-delphi-10-4-redesigned-code-insight) is simply fantastic. Given the usability improvements in the IDE, installer upgrades, VCL/FMX framework features added, improved platform support across the board and very large number of bug fixes, 10.4 Sydney is a **must-upgrade** release. If you are using the free Community Edition, hang in there as you will definitely enjoy the upgrade once it's available. (Meanwhile, if you qualify, grab your free copy of the TMS VCL UI Pack!)

You can track all of the recent activity related to 10.4 Sydney in our [**10.4 Sydney wiki page on GitHub**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.0). Also, follow on [**Twitter**](https://twitter.com/ideasawakened) for further RAD Studio related posts. I've also connected to a number of additional RAD Studio developers in the last few weeks on LinkedIn - feel free to send a connection request via my [**LinkedIn profile page**](https://www.linkedin.com/in/darianm/). Let's all work together to increase the visibility of the RAD Studio, Delphi, C++ Builder and the third-party community!


## Update note
As of June 10, 2020 David Millington noted that editing the registry to fix the issue is not recommended. Use the GetItCmd.exe command instead
