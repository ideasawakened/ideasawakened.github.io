---
layout: post
title: "Customize the RAD Studio 11 Welcome Page for single click activations"
date: 2021-09-09 12:00:00 +0000
last_modified_at: 2021-09-09 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [11 Alexandria, Undocumented]
permalink: post/customize-the-rad-studio-11-welcome-page-for-single-click-activations
published: true
image: /assets/blog/RAD-Studio-11-Alexandria/Delphi-11-Welcome-Page-Tweak-square.png
description: "A new feature of RAD Studio 11 is a redesigned Welcome Page screen.  A quick config change allows for single clicks on this new screen."
---
One of the features of RAD Studio 11 is a completely new **Welcome Page** screen. Embarcadero utilized a VCL designed screen instead of relying on browser-based content (as used in previous versions) mainly to allow for better High DPI support and also allow for more powerful features. Since it was a new screen, they decided to use their recently released [**virtual list control**](https://docwiki.embarcadero.com/Libraries/en/Vcl.ControlList.TControlList) (which was added in [**10.4.2**](https://blogs.embarcadero.com/two-new-vcl-controls-coming-in-rad-studio-10-4-2/)) for dynamically drawing lists that automatically resize based on content and screen size.

I really like the look and feel of the new Welcome Screen as it looks and feels polished and well designed. However, this new screen may cause a little annoyance for some users as the new default behavior requires a double-click to open a listed item (versus a single-click on items listed on the Welcome Screen in prior versions.) This is easily addressed with an addition to the Windows Registry to switch the behavior to a single-click. Simply add a new registry entry below and restart the IDE to enable this custom behavior:

````
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\22.0\WelcomePage]
"ActivateLinkOnTitle"=dword:00000001
````

Once you set the **ActivateLinkOnTitle** value to **1** the title line of each listed item is turned into a hyperlink. As seen in the image below, you will be able to single-click to open a project listed in the _Open Recent_ items frame. Simply remove the registry entry or change the value to **0** to return to the default double-click behavior.

![Delphi 11 welcome page tweak to turn recent files into links](/assets/blog/RAD-Studio-11-Alexandria/Delphi-11-Welcome-Page-Tweak.png)

This was a great last minute change added based on feedback received during the Beta testing process. This may become a configurable item at some point in the future.

Many more great changes are coming to the IDE as Embarcadero continually improves RAD Studio. For example, one feature that is pending final documentation and polish is the ability to create custom plugins to this newly revamped Welcome Screen so you can add your own functionality as needed.
