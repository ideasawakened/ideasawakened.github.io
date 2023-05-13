---
layout: post
title: "RAD Studio 10.4.2 is now available with a much enhanced Error Insight"
date: 2021-02-25 12:00:00 +0000
last_modified_at: 2021-02-25 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [Embarcadero, DelphiKB, Discounts, 10.4 Sydney]
permalink: post/rad-studio-10-4-2-is-now-available-with-a-much-enhanced-error-insight
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/RADStudio-LSP-Improvements-square.png
description: "RAD Studio 10.4.2 was recently released and includes many improvements to the LSP based editor resulting in an serious upgrade to Error Insight.  Along with other major IDE improvements, this 10.4.2 release marks a decidedly major upgrade to the IDE used by Delphi and C++ Programmers."
---
Last year I [**posted**](https://www.ideasawakened.com/post/newly-discovered-hidden-benefits-of-inline-variables-in-delphi) about the cool new inline variable feature of Delphi and referenced the upcoming LSP-based Code Insight in 10.4 and in a tongue-in-cheek manner I stated that Error Insight "Shall Not Suck" via a Gandalf meme:

![Gandalf stating that Error Insight "You Shall Not Suck"](/assets/blog/Delphi-Programming/Gandalf-Error-Insight-No-Longer-Sucks.jpg)

After nine months of effort by the RAD Studio dev team, we have seen the release of 10.4 Sydney in May of 2020, followed by the first major update 10.4.1 in September and now 10.4.2 was released yesterday, completing the 10.4 Sydney major release cycle (excluding any hotfixes/patches that may be released for 10.4.2.)

One of the major features of 10.4 Sydney was the introduction of an LSP-based Code Insight and it provided for much excitement in the community. After testing 10.4.2, I hereby declare Error Insight as not-only "not sucking" but it is now actually "really cool." Overall, Code Insight was working well in 10.4 but now some powerful new features have been added.

One of the first things I used to do after every install of Delphi was to disable Error Insight. It was just a habit to use the Tools->Options menu and change a few things before doing anything with the new version. (After a 'few' times of doing this manually, I eventually started merging a custom registry file after new version installs to save me the repetition.) Now, after the release of 10.4.2, the first thing that should be done is to go into Tools->Options and change the Error Insight defaults to use the enhanced new features which are not enabled by default (assumedly to keep behavior the same as previous versions.)

![Delphi 10.4.2 Error Insight configuration page](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.2-ErrorInsight-Configuration.png)

I recommend changing the "**Show Error Insight Levels**" from the default of "**Errors Only**" to "**Everything**" and ensure the "**Show Error Insight in editor gutter"** option is checked. There is one more option that you can configure based on your preference and that is the "**Editor rendering style**." There are four styles available: **Classic**, **Smooth** **Wave**, **Solid Line**, and **Dots**. Below is a screenshot from the Embarcadero Release Webinar video demonstrating the four choices available and how the warnings and hints have custom colors (note: a replay of the webinar will be available on YouTube soon.) For me, the Dots option seems nice on longer expressions, but it is a little hard to read on single-character variables. I am leaning towards the Smooth Wave choice myself, but to each his own. As discussed in the Q&A portion of the webinar today, a future update may include the ability to change the colors of these indicators.

![Overview of some LSP Improvements in RAD Studio 10.4.2](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-LSP-Improvements.png)

For those of you that automate your Delphi setup process, here is an example .reg file to configure these new settings:

````
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\22.0\Editor\Source Options\Borland.EditOptions.Pascal]
"ErrorInsight Gutter"="True"
"ErrorInsight Marks"="Smooth Wave"
"ErrorInsightLevel"="Everything"
````

Note: the REG\_SZ values for the **ErrorInsightLevel** key currently include:

- None
- Errors
- Errors and Warnings
- Errors, Warnings and Hints
- Everything

And the REG\_SZ values for the **ErrorInsight Marks** key currently include:

- Classic
- Smooth Wave
- Solid Line
- Dots

I look forward to the many posts on 10.4.2 by others and I will attempt to do at least a few posts myself on the many improvements. For now, I am really enjoying the responsiveness of the IDE, the greatly increased compiler speed for large projects, new features and fixes in Code Insight - including the **Awesome Error Insight!** Kudos to Embarcadero!

Needless to say, I would assume that Gandalf would be much happier after the release of 10.4.2!

![Gandalf smiling](/assets/blog/Gandalf-Smiling.jpg)

Since it's Delphi's 26th Birthday, there's a nice discount available if you purchase a new license or upgrade. Check out their [**specials page**](https://www.embarcadero.com/radoffer) for the latest details! It's time to **upgrade to Delphi 10.4.2**!

![Delphi is 26 years old](/assets/blog/Delphi-Programming/Delphi-Is-26-years-old.jpg)
