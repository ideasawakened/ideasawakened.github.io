---
layout: post
title: "Follow up patch for Delphi 10.4.2 released to address Try/Finally issue"
date: 2021-05-03 12:00:00 +0000
last_modified_at: 2021-05-03 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [10.4 Sydney, DelphiKB, Pavel Třešňák, Łukasz Demczuk, Marco Cantu]
permalink: post/delphi-10-4-2-patch-released-for-try-finally-issue
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/RSP-33117-ReReleased.png
description: "A follow-up patch for RSP-33117 was released today by Embarcadero that should address Try/Finally issues on non-Windows platforms."
---
A third patch is now available via GetIt for Delphi 10.4.2 which should address [**RSP-33117**](https://quality.embarcadero.com/browse/RSP-33117) which is an issue with Try/Finally under non-Windows platforms. As mentioned in a [**previous blog post**](https://www.ideasawakened.com/post/patch-misfire-try-finally-fix-to-be-re-released), this fix was intended to be addressed in an earlier patch release, but some files were missing from the installation package. Embarcadero very quickly released a follow-up patch today.

![Patch re-released for Delphi 10.4.2](/assets/blog/RAD-Studio-10.4-Sydney/RSP-33117-ReReleased.png)

As with the other 10.4.2 patches, a backup is stored by default in the folder: `C:\Program Files (x86)\Embarcadero\Studio\21.0\_patch-backup`. The patch contents are stored in the GetIt CatalogRepository folder, by default in the user's Documents directory such as: `C:\Users\Darian\Documents\Embarcadero\Studio\21.0\CatalogRepository\10.4.2RTLPatch-10`. It is a little interesting to see that the first three patches were labeled Patch4, Patch5, Patch6 internally...perhaps they released three patches privately to customers in attempt to beta test these released patches.

The three patches have been added to the [**list of recent releases**](https://github.com/ideasawakened/DelphiKB/wiki/Delphi-Master-Release-List) in a GitHub wiki page dedicated to tracking Delphi releases.

Also note that for those that do not utilize GetIt, the first two patches for 10.4.2 are now available for direct download in [**my.embarcadero.com**](http://my.embarcadero.com/) and this third one should be available soon.

Related follow up: **Pavel Třešňák** and **Łukasz Demczuk** also commented on [**RSP-32768**](https://quality.embarcadero.com/browse/RSP-32768) (F2046 out of memory), which was another major issue addressed by these recent patches, that the patch did not correct build errors on their particular projects. 

**Marco Cantu** replied: 
>For many customers this issue is addressed by the following patch: https://blogs.embarcadero.com/rad-studio-10-4-2-general-patch-and-delphi-compiler-patch/
We'll keep investigating the scenarios in which this is still an issue. Seems like a cache problem.
