---
layout: post
title: "RAD Studio 10.4.1 hopefully coming soon"
date: 2020-07-22 12:00:00 +0000
last_modified_at: 2020-07-22 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [David Millington, Stefan Glienke, Dalija Prasnikar, Roadmap, 10.4 Sydney]
permalink: post/rad-studio-10-4-1-codename-foraker-hopefully-coming-soon
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/PatchAvailable.jpg
description: Patch #2 has been released for 10.4 Sydney that addresses multiple issues and also introduces a few kinks.
---
RAD Studio 10.4 Sydney was a large release and I was certainly excited to get my hands on it. Unfortunately, the release process was a bit messy and some initial cleanup was required (see my previous [**blog post**](https://www.ideasawakened.com/post/rad-studio-10-4-sydney-is-out-a-few-tweaks-required-and-community-edition-notes) for some of the details.) After using the new version for a few weeks, it became quite apparent that there were some major problems with the [**LSP-based CodeInsight**](https://community.idera.com/developer-tools/b/blog/posts/new-in-delphi-10-4-redesigned-code-insight) feature which was one of the hallmarks of the release. I reluctantly disabled this new feature as it became a distraction due to some errant functionality.

The [**second patch**](https://community.idera.com/developer-tools/b/blog/posts/patch-2-for-rad-studio-10-4-now-available) for 10.4 Sydney has been released and the excitement is somewhat rekindled due to the list of 50+ customer reported fixes being included. There are also some internal issues corrected within the IDE and VCL Styles as discussed in David Millington's [**blog post**](https://community.idera.com/developer-tools/b/blog/posts/10-4-patch-2-delphi-code-completion-and-ide-fixes). It's a rather large patch (nearly 200 MiB) but once again the install process isn't automated as you should use GetIt to download the patch and then manually apply it. David has another blog post full of screenshots to act as an [**install guide for patch #2**](https://community.idera.com/developer-tools/b/blog/posts/10-4-patch-2-installation-guide). (Note that there was a [**report**](https://quality.embarcadero.com/browse/RSP-30109) of a corrupted download via GetIt so if you have similar problems you can download patch #2 directly from [**my.embarcadero.com**](https://my.embarcadero.com/#downloadDetail/763) but also note that the Welcome page may not recognize that you have installed Patch #2 as seen in [**RSP-30098**](https://quality.embarcadero.com/browse/RSP-30098).)

![Patch #2 for Delphi 10.4 Sydney](/assets/blog/RAD-Studio-10.4-Sydney/10.4-Sydney-Patch2.png)

There are some issues with Patch #2, with a regression posted by Stefan Glienke: [**RSP-30078**](https://quality.embarcadero.com/browse/RSP-30078). (Temporary workaround is removing record constraint as Dalija Prasnikar notes in [**RSP-30126**](https://quality.embarcadero.com/browse/RSP-30126).) Here is the detailed description from RSP-30078:

> With Patch 2 probably in order to fix [**RSP-28761**](https://quality.embarcadero.com/browse/RSP-28761) the record constraint now disallows any record with managed fields.

> In context of allowing a generic record to be a variant record it makes sense to not allow T to be a managed type because that can cause some severe defects with the variant record.

> Up until before 10.4 patch 2 it was possible to put a record with a managed field into such a variant record and the compiler properly called initialization and finalization code for the variant record - however as soon as the data in the variant record was not for the record with managed field things could blow up (which is why non generic variant records don't allow managed types in them).

> So there were conflicting specs here at work:

> \* a generic record can be a variant record

> \* the record constraint allows records with managed types (and probably should allow also the new custom managed records to be consistent)

> There is one thing that should be fixed though which worked before:

> Putting any managed type into a generic variant record like in the before mentioned RSP-28761 without messing with the record constraint.

> A regular generic type with the record constraint should allow all kinds of records and other value types but a generic variant record (which is only possible with the record constraint) should only allow non managed types.

On a positive side, Stefan did add a comment in a [**Praxis thread**](https://en.delphipraxis.net/topic/3147-patch-2-for-rad-studio-104-now-available/) that "Spring4D is not negatively affected by Patch2" (But note a potential IDE failure from a Spring4D user that is documented in [**RSP-30092**](https://quality.embarcadero.com/browse/RSP-30092).)

Also be aware that the behavior of [**Default**](https://stackoverflow.com/a/38393505)() has changed with Patch #2, as detailed in [**RSP-30093**](https://quality.embarcadero.com/browse/RSP-30093).

According to a comment by David yesterday on his installation guide blog post, Patch #3 will be released soon with further critical fixes for 10.4 Sydney. The next patch or two should really solidify 10.4 and then we can really look forward to the eventual release of 10.4.1 Foraker. (I'm hoping that a fix for [**RSP-30088**](https://quality.embarcadero.com/browse/RSP-30088) will be in the next patch. This particular issue should prompt some further discussion on always running your tests in Debug and Release modes if you aren't already doing that today!)

Given the release cadence for 10.3 Rio, I am _guessing_ that 10.4 Sydney Update 1 will be released around the first week of September. The last roadmap was from August 2019 and I'm also guessing (hoping) that a new one will be published before 10.4.1 is released. The next roadmap will be **very interesting** to see where Delphi and C++ Builder are headed!

![Roadmap by Embarcadero for Delphi features](/assets/blog/RAD-Studio-10.4-Sydney/August-2019-Embarcadero-roadmap.png)

Have you switched to 10.4 Sydney yet? What's holding you back? I'm definitely curious to hear any feedback - please leave a comment on this blog, or send me a message on [**Twitter**](https://twitter.com/ideasawakened) or [**LinkedIn**](https://www.linkedin.com/in/darianm/).
