---
layout: post
title: "RAD Studio 10.4.2 is great, but here are some notes"
date: 2021-03-01 12:00:00 +0000
last_modified_at: 2021-05-13 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [10.4 Sydney, Code Partners, CNPac, Virtual Box, Community Edition, DelphiKB]
permalink: post/rad-studio-10-4-2-is-great-but-here-are-some-notes
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/GetIt-Offline-Mode-RADStudio.png
description: "10.4.2 was just released and here are some notes about some Installation problems, notable bugs, but no update yet on theCommunity Edition."
---
I am quite thrilled with the latest 10.4.2 release of Delphi as there is a lot to love in this new version! However, there are some stumbling blocks which I wanted to document in a quick blog post to save a few people some time.

## **Install Related Issues**

There always seems to be many issues related to installing the latest version. The good people at **Code Partners** recently put out a post covering various installation issues, including where to download the latest version (tldr; [**my.embarcadero.com**](https://my.embarcadero.com/)) Check out their post here: [**https://code-partners.com/how-to-download-the-latest-version-of-rad-studio-delphi-or-cbuilder/**](https://code-partners.com/how-to-download-the-latest-version-of-rad-studio-delphi-or-cbuilder/)

There are quite a few people that realize a full uninstall is needed before installing 10.4.2 so they go ahead and manually uninstall 10.4.1 and then install 10.4.2. That does seem like a reasonable action, but it is wrong. When the installer runs, it automatically looks for previous versions and will prompt you to remove the previous registry settings (it defaults to "no") before it automatically uninstalls the old version for you. If you manually uninstall the software, then you need to manually run the [**Migration Tool**](http://docwiki.embarcadero.com/RADStudio/en/Settings_Migration_Tool) to save your settings before uninstalling the old version. You have been warned...again!

There are some reports like [**RSP-32778**](https://quality.embarcadero.com/browse/RSP-32778) of the **Win32 Paths** being the only paths properly saved during an install. To be safe, you should make a backup of your **HCKU\\Software\\Embarcadero\\BDS** registry key before installing the latest version if you have a lot of component paths to maintain.

As documented in the [**Installation Notes**](http://docwiki.embarcadero.com/RADStudio/Sydney/en/Installation_Notes), there is a current problem with **64-bit Linux Paths** being incomplete if you use the ISO based installer. To fix, paste the following string into the Browsing path field for 64-bit Linux:

> $(BDS)\\source\\rtl\\common;$(BDS)\\source\\rtl\\sys;$(BDS)\\source\\rtl\\linux;$(BDS)\\source\\ToolsAPI;$(BDS)\\source\\IBX;$(BDS)\\source\\Internet;$(BDS)\\source\\Property Editors;$(BDS)\\source\\soap;$(BDS)\\source\\xml;$(BDS)\\source\\Indy10\\Core;$(BDS)\\source\\Indy10\\system;$(BDS)\\source\\Indy10\\Protocols;$(BDS)\\source\\fmx;$(BDS)\\source\\databinding\\components;$(BDS)\\source\\databinding\\engine;$(BDS)\\source\\databinding\\graph;$(BDS)\\source\\data;$(BDS)\\source\\data\\ado;$(BDS)\\source\\data\\cloud;$(BDS)\\source\\data\\datasnap;$(BDS)\\source\\data\\dbx;$(BDS)\\source\\data\\dsnap;$(BDS)\\source\\data\\Test;$(BDS)\\source\\data\\vclctrls;$(BDS)\\source\\rtl\\posix;$(BDS)\\source\\rtl\\posix\\linux;$(BDS)\\source\\data\\datasnap\\connectors;$(BDS)\\source\\data\\datasnap\\proxygen;$(BDS)\\source\\DataExplorer;$(BDS)\\source\\Experts;$(BDS)\\source\\indy\\abstraction;$(BDS)\\source\\indy\\implementation;$(BDS)\\source\\indyimpl;$(BDS)\\source\\Property Editors\\Indy10;$(BDS)\\source\\soap\\wsdlimporter;$(BDS)\\source\\Visualizers;;$(BDS)\\source\\data\\rest;$(BDS)\\source\\data\\firedac;$(BDS)\\source\\tethering;$(BDS)\\source\\DUnitX;$(BDS)\\source\\data\\ems;$(BDS)\\source\\rtl\\net

![Warning - The Embarcadero GetIt server could not be reached](/assets/blog/RAD-Studio-10.4-Sydney/GetIt-Offline-Mode-RADStudio.png)

Following the same behavior as recent versions, and [**as stated in the release notes**](http://docwiki.embarcadero.com/RADStudio/Sydney/en/Release_Notes), if you are installing in **offline mode** using the ISO based installer, then the Catalog Repository remains in offline mode after the installation completes. While the repository is in offline mode you will get warnings displayed in the IDE like the one in the image above stating that **The Embarcadero GetIt server could not be reached**. You will need to manually change GetIt to switch to online mode by running the following command and restarting the IDE:

````
GetItCmd.exe -c=useonline
````

Issues related to this problem keep being added to the Quality Portal like [**RSP-33077**](https://quality.embarcadero.com/browse/RSP-33077). Please vote or comment on these issues so that this particular problem will be addressed in a future update.

## **Multiple IDE Windows**

One of the cool new (or perhaps re-implemented) features of 10.4.2 is the ability to have your Code Editor window on one screen and your Form Designer window on another. This feature works well if your Form Designer window remains docked. If you are attempting to use a floating Form Designer, then you may run into problems. (I hope a patch will be released soon.) This is documented in a few separate issues including [**RSP-32501**](https://quality.embarcadero.com/browse/RSP-32501) and [**RSP-33090**](https://quality.embarcadero.com/browse/RSP-33090).

I received a question today from someone trying to figure out how to get this new feature to work. An easy solution is to bring up your form as a docked window in the IDE and then view the source of that form by hitting **F12** to toggle views. Now use the **View**\->**New Edit Window** menu option (or right-click the unit name in the editor tab and click on **New Edit Window**.) This will add a new floating Code Editor Window that you can drag to your second monitor. Go back to the docked Code Editor Window and hit **F12** to toggle back to Form Designer View and you will end up with a docked Form Designer Window on your main screen and a floating Code Editor Window on another screen.

It is probably obvious that you can only have one version of the unit's Form Designer Window open at any given time. However, you can certainly have multiple Code Editor Windows of the same unit open and editable without any issue. This is because every Code Editor Window uses an internal buffer which is shared between all other instances. This means that changes made in one Code Editor Window instantly appear in any other.

## **Other Issues**

The XML documentation feature was enhanced with 10.4.2, but apparently at least one regression was introduced. This is documented in [**RSP-33091**](https://quality.embarcadero.com/browse/RSP-33091) and occurs when you have an empty remarks element in the XML documentation in the interface section of your unit.

There was a report of CNPack causing some editor display artifacts in 10.4.2. I do not know if this has been addressed by the CNPack developers yet.

The new warnings and hints displayed with Error Insight seem to disappear if an error is also displayed in a method as documented in [**RSP-32783**](https://quality.embarcadero.com/browse/RSP-32783).

There was a comment that 10.4.2 introduced a GDI error when running RAD Studio under a Virtual Box based virtual machine that did not occur in 10.4.1. This is limited to having the component toolbar enabled. (DirectX 11 support needed - install Virtual Box enhanced video extension pack as a possible solution.)

## **Community Edition**

Predictably, after another release of RAD Studio there are many questions about the availability of an updated Community Edition. As repeatedly covered in their launch webinar Q&A section, the only official answer available is the following comment:

> A new community version is in the works but no timeframe to announce for now.

What follows is my basic response to a user questioning the availability of the Community Edition the other day:

Only the people at Embarcadero have any real idea when a new Community Edition will be released. It is like trying to assume when a patch is going to be released, or the next major version. You can certainly make a guess, but you only really know after it is released.

If new users are trying to learn Delphi, then there is a **Free Trial** available with the full Architect version: [**https://www.embarcadero.com/products/delphi/start-for-free**](https://www.embarcadero.com/products/delphi/start-for-free) There are also **Academic** licenses available for those currently in school: [**https://learndelphi.org/licenses**](https://learndelphi.org/licenses/) And finally, there is the **Community Edition** which is meant for hobbyists and startups with less than $5k in revenue: [**https://www.embarcadero.com/products/delphi/starter**](https://www.embarcadero.com/products/delphi/starter)

I currently do not fully understand the concern about the Community Edition being "late" unless perhaps there are some Open Source projects that are dependent on supporting the latest version and they are using the CE edition as their main development tool. Please let me know if there are issues with Open Source projects waiting on a CE release and I will try to help with a Pull Request to support the latest version.

There are three different free options available via Free Trial, Academic License and Community Edition. There are three different paid options available via the Professional, Enterprise, and Architect Editions. That is quite a nice coverage! If you are truly anxious about getting your hands on 10.4.2, then either download the trial or **buy a copy**. There is currently a promotion going on with a nice discount for Delphi's 26th birthday. Check out their [**specials page**](https://www.embarcadero.com/radoffer) for more information.

Additionally, there is some confusion on the actual restrictions of the Community Edition. Embarcadero has dedicated a [**FAQ Page**](https://www.embarcadero.com/products/delphi/starter/faq) just to answer the most common questions.

## **Additions**

Here are some items added after this article was originally posted.

For a long time, there has been an option to flip the Read Only flag in the source code editor so that you could make easily make temporary changes to a file that is marked read only in the file system. This seems to be broken in 10.4.2 with a few different error reports, including [**RSP-33130**](https://quality.embarcadero.com/browse/RSP-33130), [**RSP-32764**](https://quality.embarcadero.com/browse/RSP-32764), and [**RSP-33099**](https://quality.embarcadero.com/browse/RSP-33099).

This is a possible worrisome issue with lifetime of variables being incorrect in a nested block: [**RSP-33134**](https://quality.embarcadero.com/browse/RSP-33134). There are no notes on the issue yet, so it has not yet been validated.

There's apparently a code completion regression that may cause some annoyance until it is patched, found in [**RSP-33118**](https://quality.embarcadero.com/browse/RSP-33118).

A **potential showstopping bug** is discussed in [**RSP-33117**](https://quality.embarcadero.com/browse/RSP-33117). It seems that a Try/Finally block can be broken if there is a **raise** statement within the **Finally** section for all platforms except Windows. I would keep an eye out for updates on this one. I would also assume a hotfix is in the works. UPDATE: this was fixed with 10.4.2 patch #3 ([**10.4.2.3**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.2.3))

Some fatal compiler errors (**F2046 out of memory**) are being reported on some projects attempting to be built with 10.4.2 that built successfully with 10.4.1. See [**RSP-32768**](https://quality.embarcadero.com/browse/RSP-32768) UPDATE: this (and LSP performance) was greatly improved with 10.4.2 patch #1 ([**10.4.2.1**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.2.1)) but some issues remain, see [**RSP-33962**](https://quality.embarcadero.com/browse/RSP-33962).

## **Further Updates**

I will attempt to update this blog post with any major issues that I find with 10.4.2. There is also a [**wiki page**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.2.0) available on GitHub covering this and other recent releases that will be updated as new information is discovered.

You can directly monitor the **Quality Portal** yourself by viewing the latest issues created for 10.4.2. You can input a query like this to view new issues:

> project = RSP AND created>=-1w and affectedVersion = "10.4 Sydney Release 2" ORDER BY created DESC

Here is a [**link**](https://quality.embarcadero.com/browse/RSP-33122?jql=project%20%3D%20RSP%20AND%20created%3E%3D-1w%20and%20affectedVersion%20%3D%20%2210.4%20Sydney%20Release%202%22%20ORDER%20BY%20created%20DESC) for that particular query that you can run at any time.
