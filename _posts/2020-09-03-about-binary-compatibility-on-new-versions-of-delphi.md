---
layout: post
title: "About binary compatibility on new versions of Delphi"
date: 2020-09-03 12:00:00 +0000
last_modified_at: 2022-03-15 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [Uwe Raabe, Marco Cantu, David Millington, DelphiKB]
permalink: post/about-binary-compatibility-on-new-versions-of-delphi
published: true
image: /assets/blog/Delphi-Programming/Delphi-Version-Updates-WordCloud-Small.png
description: Delphi binary compatibility is not guaranteed between major versions but is for point releases.
---
The latest Delphi version 10.4.1 was [**released yesterday**](https://blogs.embarcadero.com/rad-studio-10-4-1-has-been-released/) and there's always a few questions about third party components and version upgrades so I thought I'd put up a quick post in response.

Every _major_ release of Delphi has always been binary-incompatible with the major version prior (with the historical exception of Delphi 2006 and Delphi 2007 which were intentionally binary compatibile.) For example, this means that all of the DCUs and BPLs created by Delphi 10.3 Rio are not compatible with Delphi 10.4 Sydney. You have to rebuild those binaries to use them in the newer version.

When Embarcadero first took over the development of Delphi, they kept up with the historical release cycle of one-major-version-per-year until they ramped up development with the release of RAD Studio XE4 in April 2013 which was only 7 months after the release of XE3. They kept the rapid pace with a release of XE5 a few months later in September 2013 and maintained that rapid pace of releases for the next few years. Following the historical practice of Borland, if any fixes were needed to a major release, a small patch was released. (It typically involved one to three patches per major release.) These patches were sometimes called hotfixes and sometimes referred to with an Update number.

Releases started changing with the introduction of RAD Studio 10 Seattle back in 2015. Along with the name change, they started doing larger point releases. These point releases allow them to continue introducing new functionality and fixes but also go back to the one major version per year approach, taking some stress off of the third party component vendors (as vendors need to release new versions of their products every time a new major version of Delphi is released.) There are still a limited number of patches (hotfixes) released as needed.

As an example, RAD Studio 10.3 Rio was released in November 2018. It was followed up with 10.3.1 in February 2019; 10.3.2 in July 2019; 10.3.3 in November 2019. All of these 10.3 releases were produced to be binary compatible. Since they are binary compatible, if you purchased a component package from a third party vendor that supports 10.3, you do not need to get an update from that vendor to support any of the 10.3 point releases. In addition, Delphi 10.3 Rio had 3 patches, 10.3 Rio Update 2 had 5 patches, and 10.3 Rio Update 3 had 11 patches. This adds up to 23 distinct 'releases' of Delphi 10.3 Rio, and they all produce binary compatible DCUs and BPLs. That takes an enormous amount of effort and discipline on the part of Embarcadero's development team.

![Delphi Version Compatibility word cloud](/assets/blog/Delphi-Programming/Delphi-Version-Updates-Binary-Compatiblity.png)


## Delphi 10.4 Sydney Update 1
For Sydney, this means if you have Delphi 10.4 installed and want to re-use the components in this latest version released yesterday (10.4.1) then due to binary compatibility being assured by Embarcadero across these point releases, **you do not need to get a new build of your components from your third party component vendor for a point release.** You really shouldn't ever have a problem with any of your 10.4 components no longer working in 10.4.1. (Well... if the component relied on 'buggy' behavior which is 'fixed' in a point release, then it could be a problem. If Embarcadero screws up and introduces an interface-breaking change somewhere in the update, it can certainly cause problems. You can safely consider both of these scenarios to be pretty rare.)

On the topic of upgrading, you should note that you currently have to fully uninstall Delphi 10.4 to install 10.4.1 on the same machine. If you start the 10.4.1 install process, it will detect the presence of 10.4 and ask you to uninstall the previous version and provide an opportunity to save the settings. You could also run the [**Settings Migration Tool**](http://docwiki.embarcadero.com/RADStudio/en/Settings_Migration_Tool) manually to save your registry settings to restore them later (or migrate those settings to a different machine.) If you manually uninstall 10.4 without using the Settings Migration Tool, then your registry settings will be set back to default values on the install of 10.4.1. (So - either use the tool or simply start the install of 10.4.1 and use the built-in functionality.)

For a good article on why binary compatibility is important between point releases, see **Uwe Raabe**'s blog article on [**The Mysterious Case Of The Lost Inherited Call**](https://www.uweraabe.de/Blog/2018/05/28/the-mysterious-case-of-the-lost-inherited-call/) dealing with 10.2 update issues. His main conclusion was simply:

> When it comes to packages and you want to be binary compatible between your package versions, you better not touch the interface sections of your units! Introducing a new inheritance level for a virtual method – or worse, removing one – will break binary compatibility. That is a direct consequence of how the compiler resolves inherited calls.

Also see the comments on **Marco Cantu's** [**blog post of the 10.2.2 release**](https://blog.marcocantu.com/blog/2017-december-delphi-10-2-2.html) where they accidently introduced a breaking change and had to quickly re-issue a new build of 10.2.2. It also led to some issues with a [**10.2.2 patch**](https://blogs.embarcadero.com/rad-studio-10-2-2-tokyo-february-2018-patch-re-released/) that definitely caused some mayhem for some users and third party component providers such as [**Devart**](https://forums.devart.com/viewtopic.php?t=36345).

So for the most part, the component incompatibility problem centers around **major releases**. Major releases can certainly be pretty painful to manage based on the number of third party components that you have to re-install. If you have the source code to the components, then you can normally just rebuild them in the next major release without waiting on the component vendor to provide you with an update. (Although you may have to tweak the package project file or an include file to make it compatible with the new compiler/package version numbers, but that is normally a few minute job.) This is one reason why you should always buy the source code of any third party component if given the opportunity.

For those Delphi developers that have been around a while, they are nearly guaranteed to have dealt with these issues many times. It's a highly sensitive topic as it's consumed so much of our time over the years. And the more third party components that you have to manage, the more of a time sink it can be. It's also why there are a handful of package managers out there trying to simplify package management for Delphi, but unfortunately nothing is standardized yet that will work with older versions of Delphi, although there is a little [**hope**](https://www.finalbuilder.com/resources/blogs/introducing-dpm-a-package-manager-for-delphi).

Embarcadero is obviously aware of this problem and has responded with a built-in package manager for RAD Studio called the [**GetIt Package Manager**](http://docwiki.embarcadero.com/RADStudio/en/GetIt_Package_Manager). If your components are supported by GetIt, then it only takes a few button presses to get them installed in the latest version. Since the 10.4 Sydney release, Embarcadero has also started to use GetIt to install the latest patches to prevent the manual operation that it's always been. They have seen some progress with this as the first patch for 10.4 was automatic, whereas patches 2 and 3 were automatic downloads, but manual installs.

Note that most of this information is for Delphi and not for its RAD Studio brother, C++ Builder where I'm told that binary compatibility isn't treated as strictly. I've isolated myself from much of this pain over the years. At my last job, we stuck ourselves on Delphi 5 for more than a decade before finally upgrading to Delphi 2007, with much of the delay due to third party component baggage. I would wager that there are many Delphi teams out there stuck at a particular version due to various third party component issues. I'll stress one more time - when buying a component, always buy the source code if there is an option to do so. Component vendors can come and go - if you have purchased the source code to the components, much of this pain is avoided as you can simply rebuild on the next major release of Delphi. (And it's another reason to buy from well-established component vendors like [**tmssoftware.com**](http://tmssoftware.com/).)

I hope this clarifies some of the issues surrounding version compatibility with Delphi. Note that some of this info is reproduced by memory, so I'll update this page if I receive any corrections. Keeping track of all the version releases is one reason I started keeping a [**wiki page of releases since 10.3 Rio**](https://github.com/ideasawakened/DelphiKB/wiki/Delphi-Master-Release-List). One of these days I may have this wiki page fully populated...

If you have any questions, or corrections, please connect with me on [**LinkedIn**](https://www.linkedin.com/in/darianm/) or [**Twitter**](https://twitter.com/ideasawakened) and send me a direct message and I'll try to help.

## UPDATE1
Another piece of the puzzle when it comes to new versions are the MSBuild config files and apparently there were some changes made in the 10.4.1 installation which required a third party component provider (TRichView) to re-build and re-issue their installer. See their [**forum post**](https://www.trichview.com/forums/viewtopic.php?p=38479#p38479) for a few details. From a follow-up on Twitter asking for which config files were changed: "_Codegear.\*.target in bin folder. Previously, in conditions, all environment variables were in strings, like Exists('$(path)'). Now sometimes they are not in quotes._"

## UPDATE2
Binary compatiblity was broken with 10.4.1 for IDE plugins with custom docked windows. See **David Millington**'s [**blog post**](https://blogs.embarcadero.com/ide-plugins-in-rad-studio-10-4-1). This means that your 10.4 IDE experts/plugins may cause access violations or other errors when attempting to use them in 10.4.1. If you have the source, you'll have to rebuild them in 10.4.1 or you'll need to get a new build from the supplier of the plugin.

So far, only (some) IDE Experts and Plugins need to be rebuilt for 10.4.1. The primary focus of this article - binary compatibility across minor point releases - remains true for all known Delphi Components built with version 10.4...meaning they should work without having to rebuild them for 10.4.1.

## UPDATE3
[**RAD Studio 11.1**](https://github.com/ideasawakened/DelphiKB/wiki/D28.ALEXANDRIA.11.1.0.0) was released as of March 15, 2022. As mentioned in a [**blog post**](https://www.ideasawakened.com/post/the-end-of-rad-studio-10-x-named-releases-with-version-11-coming-soon) last year, Embarcadero has changed their numbering scheme starting with RAD Studio 11 Alexandria. They have moved away from the 10.major.minor release version numbering system. RAD Studio 11.1 is now a minor release, meaning that it is intended to be fully binary-compatible with the initial 11(.0) release. In other words - **you do not have to reinstall RAD Studio 11 components for the RAD Studio 11.1 release**.