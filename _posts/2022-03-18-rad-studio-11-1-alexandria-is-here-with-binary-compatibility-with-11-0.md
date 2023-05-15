---
layout: post
title: "RAD Studio 11.1 Alexandria is here with binary compatibility with 11.0"
date: 2022-03-18 12:00:00 +0000
last_modified_at: 2022-03-18 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [11 Alexandria, DelphiKB]
permalink: post/rad-studio-11-1-alexandria-is-here-with-binary-compatibility-with-11-0
published: true
image: /assets/blog/RAD-Studio-11-Alexandria/Delphi-11.1.png
description: "Update 1 for RAD Studio 11 Alexandria has been released and it is binary compatible with the 11.0 major release."
---
Embarcadero has released the first update to RAD Studio 11 Alexandria on March 15th. I have seen a number of questions about this update and I wanted to post a quick blog entry to help address the binary compatibility question.

![RAD Studio 11.1 Alexandria has been released](/assets/blog/RAD-Studio-11-Alexandria/RADStudio-11.1-Released.png)

[**Update 1**](https://github.com/ideasawakened/DelphiKB/wiki/D28.ALEXANDRIA.11.1.0.0) is indeed binary compatible with the 11.0 release. For Delphi developers, this means that you **do not** have to rebuild/reinstall your components for this 11.1 release. If you have 11 Alexandria already installed, simply install this release and it will detect your existing installation and prompt you to uninstall the previous version and save your settings. If you accept the default values during the install process, 11.1 will be installed and your registry settings will be maintained, preventing you from the requirement of reinstalling your third-party components.

Some of the confusion is due to the fact that Embarcadero changed their version numbering with the 11 Alexandria release. This was covered in a [**blog post last July**](https://www.ideasawakened.com/post/the-end-of-rad-studio-10-x-named-releases-with-version-11-coming-soon). They had started a 10x versioning scheme with the release of 10 Seattle back in 2015 (to match the release of Windows 10.) The next major release was 10.1 Berlin, which was followed by 10.2 Tokyo up to 10.4 Sydney. As with all major releases (with one notable exception in 2007), these versions were not binary compatible with each other.

Embarcadero also had minor point releases for each of these major versions. For example, there were two point releases for 10.4 Sydney in September 2020 and February 2021, which I label as 10.4.1 and 10.4.2. Finally, there were hotfixes/patches released for every release. (Three fixes were also released for 10.4 Update 2 which I label as 10.4.2.1, 10.4.2.2, and 10.4.2.3.) This adds up to a lot of releases coming out of Embarcadero! They have really been working hard at updating RAD Studio these last few years and sometimes it can seem difficult to keep track of them all. You can see a detail of the recent releases on this [**wiki page**](https://github.com/ideasawakened/DelphiKB/wiki/Delphi-Master-Release-List).

Point releases are intended to be backwards compatible to their corresponding major release. This was covered in another [**blog post back in 2020**](https://www.ideasawakened.com/post/about-binary-compatibility-on-new-versions-of-delphi) which discussed binary compatibility issues. The vast majority of the time, **an update to a major release is fully binary compatible to the major release**. There should be no problems with third party component packages being fully compatible and there should be no need to rebuild/reinstall your components...with one exception: The main problem happens if you **manually uninstall** the previous major release **and** do not save your registry settings. If you do this, then you will need to reinstall your third party components. Therefore, do not uninstall manually - simply use the 11.1 installer and let it manage the upgrade process for you and you should not have any problems.

**NOTE:** there is one bug that you should be aware of: Library paths can sometimes be lost in a new install, see [**RSP-28434**](https://quality.embarcadero.com/browse/RSP-28434) Therefore, you will want to ensure that you backup your registry settings before applying the 11.1 update if you use platforms other than Win32.

**An additional tip**: since there are some implementation changes between 11 and 11.1 you may need to know if your code is working in 11.1. You can check for the existence of the new RTL System constant, RTLVersion111 via code like:

````pascal
{$IF RTLVersion111}

//do something special if 11.1

{$ENDIF}
````