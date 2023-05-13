---
layout: post
title: "RAD Studio 10.4.1 Sydney Released - and Delphi is better than ever!"
date: 2020-09-02 12:00:00 +0000
last_modified_at: 2020-09-04 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [Kiriakos Vlahos, Grijjy, DelphiKB, Embarcadero, 10.4 Sydney]
permalink: post/rad-studio-10-4-1-sydney-released-and-delphi-is-better-than-ever
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.1-Now-Available-small.jpg
description: A quick overview of RAD Studio 10.4.1 Sydney which was released today.
---
I was really pumped with RAD Studio 10.4 Sydney was released in May. I have been rediscovering my love for Delphi lately and the 10.4 release sounded like it was going to be amazing. I stated back in May that [**Delphi 10.4 Sydney is the best tool for creating Windows Applications**](https://www.ideasawakened.com/post/delphi-10-4-sydney-is-the-best-tool-for-creating-windows-applications) and I received more than a few negative responses (plus a number of positives ones as well.) I disregarded the negativity and greedily installed 10.4 just a few hours after its release.

The 10.4 release turned out to be a little premature as there were some immediate fixes needed (see [**a few tweaks required**](https://www.ideasawakened.com/post/rad-studio-10-4-sydney-is-out-a-few-tweaks-required-and-community-edition-notes) blog post.) But the manual work arounds weren't too complicated or time consuming so it really wasn't a big deal. Well, after two months of working with 10.4 I was a little disgruntled due to the ongoing issues and followed up with another blog post in July stating that [**10.4.1 was hopefully coming soon**](https://www.ideasawakened.com/post/rad-studio-10-4-1-codename-foraker-hopefully-coming-soon). In that post, I guessed we would have to wait for 10.4.1 until the first week of September and that guess turned out to be spot-on as [**RAD Studio 10.4.1 was released today**](https://blogs.embarcadero.com/rad-studio-10-4-1-has-been-released/).

![Delphi 10.4.1 Now Available](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.1-Now-Available.jpg)

## RAD Studio 10.4.1 Now Available

So far, the latest **Delphi 10.4.1 is amazing**. True, I only installed it today and I haven't had much time to discover any of the bugs that will certainly turn up, but I did run my suite of unit tests and they all passed, including the Stress Test on the TThreadedQueue class that was revisited in a recent [**blog post**](https://www.ideasawakened.com/post/revisting-tthreadedqueue-and-tmonitor-in-delphi) which generated some interest from other developers. **Kiriakos Vlahos** (online username of [**pyscripter**](https://github.com/pyscripter/pyscripter)) led the charge and some [**fixes were proposed**](https://www.ideasawakened.com/post/tmonitor-event-stack-fix-by-a-true-delphi-mvp) that eventually made it into 10.4.1! My **TThreadedQueue tests went from Red to Green**. It's a good day indeed. Besides a number of fixes to TThreadedQueue, there was a major focus applied to the Parallel Programming Library in this release and many open issues were addressed.

Another area of focus in 10.4.1 was [**Custom Managed Records**](https://community.idera.com/developer-tools/b/blog/posts/custom-managed-records-coming-to-delphi-10-4). This was an highly anticipated new feature initially targeted for the 10.3 Rio release but was pushed back to 10.4 Sydney. From the release notes, the latest versions improves Managed Records quality, including:

-   Return values optimizations
    
-   Cleanup of a few compiler errors
    
-   Improvements to operator Assign
    
-   Changes to Default(T) for managed records
    
-   Better coexistence of managed records and generics
    
-   Some cases of missing invocation of Initialize
    

Even though Delphi has had Managed Records for a very long time, I stayed away from CMRs in the 10.4 release as there were reports of issues received pretty quickly after release. Now that 10.4.1 has addressed the most common concerns, I'll likely start integrating them into my codebase. The fine folks at **Grijjy** have a few cool blog posts on the subject that can be used as a nice reference. See [**Custom Managed Records for Smart Pointers**](https://blog.grijjy.com/2020/08/12/custom-managed-records-for-smart-pointers/) and [**Automate Restorable Operations with Custom Managed Records**](https://blog.grijjy.com/2020/08/03/automate-restorable-operations-with-custom-managed-records/).

It seems like most of the small annoyances introduced in the very large 10.4 release seem to have been addressed. Their release post stated that over 800 quality improvements made it into 10.4.1! They listened to the community and this major upgrade was nearly entirely focused on addressing customer issues. Can you think of many other software packages which you use on a daily basis that has ever issued a major release nearly completely devoted to bug fixes? There aren't many that actually take such a bold action. **Thank you Embarcadero!**

There are a few new features, like a replacement for the complex class: [**TMultiReadExclusiveWriteSynchronizer**](http://docwiki.embarcadero.com/Libraries/en/System.SysUtils.TMultiReadExclusiveWriteSynchronizer) (also referred to as TMREWSync.) This class has always been a little rough around the edges in Windows and it really didn't make the cut in other platforms. I've used this quite a bit over the years, so the TLightweightMREW replacement will be interesting to test. It appears to be a wrapper for the [**Slim Reader/Writer Locks**](https://docs.microsoft.com/en-us/windows/win32/sync/slim-reader-writer--srw--locks) on Windows (Vista or later) so it should be pretty solid. Eric Grange covered SRW on an older [**blog post**](https://www.delphitools.info/2013/12/09/slim-multi-read-single-write-locks/) in which he compared them to TMREW and Critical Sections. His post and the comments are a good review before converting all of your TMREWs into SRWs.

Another new feature is the introduction of the automatic version suffix for packages. There is a new dropdown available in the **LIB suffix** setting in the **Project Options**\->**Description** page. Set this to **$(Auto)** and the package will automatically have the current package version appended. This will eliminate that manual step to update this value after every major release saving those that work with many packages a lot of effort over the long run. (Some people have been waiting for this specific feature for a very long time!) **UPDATE**: If you insert the directive manually into the DPK file, the feature doesn't currently work as expected. If use a project file and use the menus to manage the option, then the feature works as expected. See: [**RSP-30820 Fail to load package with LIBSUFFIX AUTO**](https://quality.embarcadero.com/browse/RSP-30820).

## Summary

So far, the common Code Insight glitches that came along with the new Language Server Protocol support added to 10.4 have been smoothed over. This was a very large step forward in the development of the Delphi IDE and one of the major features of 10.4. Unfortunately, after many struggles with it, I previously disabled the feature on my system. I have my positive thinking cap on and _I do not plan on ever turning it off again_. For those that manage very large projects in Delphi, there have been a major increase in performance of the LSP-Based Code Insight in the 10.4.1 release. Code Completion is also much faster.

I'm still waiting on some minor issues to be addressed, but my big asks were taken care of in this latest release. It's onward and upward from here. The future of Delphi is looking brighter today!

Here's a link to the [**Release Notes**](http://docwiki.embarcadero.com/RADStudio/Sydney/en/10.4_Sydney_-_Release_1) that contains a nice list of items changed and this is a link to the full [**list of Bugs Fixed**](http://docwiki.embarcadero.com/RADStudio/Sydney/en/New_features_and_customer_reported_issues_fixed_in_RAD_Studio_10.4.1).

## Notes On The 10.4.1 Install Process

-   If you plan on installing 10.4.1 on the same machine as 10.4, you'll need to do a full uninstall first. (There is an option for preserving settings in the uninstall if you run the 10.4.1 installer and have it handle the uninstall of 10.4.)
    
-   10.4.1 is binary compatible with 10.4 so you shouldn't have to worry about third party components.
    
-   If you install from the ISO Image (which is considered 'offline mode') then you should perform a manual fix for GetIt as the catalog repository is left in offline mode. If you don't do the fix, then you will have problems installing anything with GetIt. At a command prompt, type in: **GetItCmd.exe -c=useonline**
    
-   Note that Windows 10 is officially required for installing RAD Studio, but there have been reports of successful installs of 10.4 on Windows XP if you just ignore the warnings. (Of course, the VCL/RTL classes leveraging new APIs won't work on the old systems.)
    

### Upgrade Pricing Is Back

For those of you that let your subscription expire and have been wanting to get the latest version but didn't want to pay full price...for a **limited time** you can upgrade at a [**large discount**](https://www.embarcadero.com/radoffer) good until **September 25, 2020**. In addition, as a bonus for Enterprise and Architect Edition users, there's a special [**Bonus Pack**](https://blogs.embarcadero.com/promos/EnterpriseComponentPack2020.php) available free which includes components from DevExpress, nSoftware, DevArt, TMSSoftware and more. It's time to get off the fence and back into Delphi and/or C++ Builder!

### Wiki Page Added

I've added a new [**wiki page on GitHub for Delphi 10.4.1**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.1.0) and have updated it with the latest info. I added quite a bit to the [**Delphi 10.4 Page**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.0) over time and will attempt to keep these pages updated going forward. I may eventually make it back to older versions. I started this wiki with the [**Delphi 10.3 Page**](https://github.com/ideasawakened/DelphiKB/wiki/D26.RIO.10.3.0.0) and the original idea was to work my way backwards, but I simply haven't had the time. Feel free to send a Pull Request or create an Issue with any info you'd like added or corrected.