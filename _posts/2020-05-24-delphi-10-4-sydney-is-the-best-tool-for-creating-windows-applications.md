---
layout: post
title: "Delphi 10.4 Sydney is the best tool for creating Windows Applications"
date: 2020-05-24 12:00:00 +0000
last_modified_at: 2020-05-24 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [Jim McKeeth, Microsoft Windows, Embarcadero, DelphiKB]
permalink: post/delphi-10-4-sydney-is-the-best-tool-for-creating-windows-applications
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/Delphi-Best-Windows-Dev-Tool.png
description: The latest Delphi release from Embarcadero helps to maintain its long time superiority in Windows application development. 
---
The best tool is getting even better! The latest release of [**Delphi**](https://www.embarcadero.com/products/delphi) is to be launched [**this week**](https://community.idera.com/developer-tools/b/blog/posts/open-for-business-with-10-4-may-2020-gm-update) and the support for Windows is much improved with High DPI Styles and per control styling support!

**Delphi has long been a favorite tool of many Windows developers.** That statement may be considered as simply _wishful thinking_ to many people, but sometimes the truth is not as obvious as it should be. The 'truth' to many is that Visual Studio is the ultimate tool for Windows developers. While I may concede that truth for many C++ developers, it's definitely debatable for .NET developers. (Simply ask a Visual Basic developer how they feel about [**no more language features planned**](https://devblogs.microsoft.com/vbteam/visual-basic-support-planned-for-net-5-0/)**.**)

Many .NET developers started with **WinForms** back in 2002 - including a whole bunch of Delphi developers that jumped ship to the greenfields of .NET from what many consider a historical pinnacle of Delphi development, **Delphi 7** (an ancient version which many developers still use today!) Some developers have stayed with WinForms over the years, but many moved to **WPF** and XAML in .NET 3.0 back in 2006. Others chose to take the five-year **Silverlight** boondoggle starting in 2007. WPF went stagnant around 2013 as the year prior Microsoft released **Metro Apps** in Windows 8. Microsoft quickly shifted the label to **WinRT** in Windows 8.1.

![Microsoft UWP](/assets/blog/Microsoft-Windows/Universal-Windows-Platform/UWP.jpg)

By 2015, an updated app platform for Windows 10 was named **UWP** as the new 'final' solution but again the improvements didn't help it to gain traction. Microsoft was recognizing the obvious resistance and quickly created new technologies to bridge applications across various platforms. They developed an iOS bridge, an Android bridge, a web apps bridge, and a classic Win32 desktop apps bridge. (Did you know that there still is an [**Objective-C for Windows**](https://github.com/microsoft/WinObjC)?) The Android bridge is dead. While the Win32 bridge is still active, it has not been utilized much.

The new ultimate solution starting in late 2019 has been [**.NET Core 3.0**](https://devblogs.microsoft.com/dotnet/announcing-net-core-3-0/) and most of the old technologies like WinForms are being reinvigorated to work with it (well, except Silverlight of course!) The new goal is to make "every platform feature available to every developer" to solve the "massive divide" between platforms due to the rejection of UWP. (Note that WinForms and WPF are **not** cross-platform options with .NET Core 3.0 so they get rejected once again.)

The very latest technology revolution unveiled by Microsoft in the last few days is [**Project Reunion**](https://blogs.windows.com/windowsdeveloper/2020/05/19/developing-for-all-1-billion-windows-10-devices-and-beyond/). This is supposed to be the new way to drag all the Win32 applications into the future (something they have been trying very hard to do over the last 14+ years!) A quote from Microsoft: "_The idea behind Project Reunion is that it allows developers to build one Windows application and target all 1 billion Windows devices....We’re bringing together the combined power of Win32 and UWP so developers no longer have to choose because we’re unifying these existing APIs and in some way decoupling them from the OS._"

![Project Reunion](/assets/blog/Microsoft-Windows/Microsoft-Project-Reunion.jpg)

I would pose a fairly significant question behind Microsoft's bold statement of a billion Windows devices out there: **how many applications on those billion devices are Win32 versus UWP?** I'm sure Embarcadero has a strong opinion on that specific topic! To be fair, there are a few dozen people in that commune in Belize which still use Windows Phones. And there are a few thousand people using the $3,500 HoloLens headsets. (It's $1,500 less than their first version after all so it's bound to gain traction!) And they actually sold over [**2,000 surface hub customers**](https://www.thurrott.com/mobile/microsoft-surface/86566/surprise-surface-hub-huge-success-literally) in the first nine months after its release in 2016. (Of course, I say most of this in jest... Microsoft has tried hard with UWP and much of their technology is very cool, but Win32 still dominates Windows.)

Meanwhile, during the many years that Microsoft has been trying to nail down the proper alternative to Win32 (including WinForms, WPF, Silverlight, Metro, WinRT, UWP, .NET Core...), the world renown Win32 powerhouse of old, Delphi 7, has consistently improved over time into its very latest incarnation: [**Delphi 10.4 Sydney**](https://www.embarcadero.com/products/delphi). Microsoft is fully embracing Win32 again and if you have struggled for years trying to keep up with Microsoft technology shifts, perhaps now is the time to take another look at Delphi?

![10.4 Meme](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4-Sydney-versus-VisualStudio.png)

Sign up for the [**upcoming webinar on 10.4 Sydney**](https://register.gotowebinar.com/register/7159476968330857739) on Wednesday May 27th and see what's new! (Note that Delphi 10.3.3 is the latest version available today, check out a [**wiki page**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.0) that I maintain with a growing number of links on the pending 10.4 release.)

Note that there are two major user interface frameworks that Delphi currently supports: **VCL** and **FMX**. (See the [**blog post from Jim McKeeth**](https://community.idera.com/developer-tools/b/blog/posts/firemonkey-vs-vcl) for a quick comparison.) The VCL framework is what Delphi has always used to target Win32. While the [**haggard .NET developers**](https://www.infoq.com/news/2011/06/Win8-Doubt/) have struggled for years to keep up with the alphabet soup from Microsoft and their ever-changing technology shifts, the quiet VCL developers have happily leveraged the long-term, focused, continued improvements invested by Embarcadero to make their Windows applications better. You can get a visual representation of this history by viewing the [**History of Delphi Innovations Timeline**](https://delphi.embarcadero.com/history-of-delphi-innovations/) from Embarcadero which was recently released for the 25-year anniversary of Delphi.

The bulk of your Delphi code can be used with either the VCL or FMX framework which greatly simplifies your application's migration to another supported platform, if desired. If you decide to stick to the Windows platform (which is easy to understand given it's huge 70+% desktop market share), either framework offered by Delphi will allow your application to be highly polished, responsive, and [**feel very much at home to your Windows client users**](https://community.idera.com/developer-tools/b/blog/posts/5-unique-delphi-features-for-windows-10). Currently, approximately 40% of Delphi users leverage FMX for cross-platform development while the remainder target Windows.

Delphi is available in [**four editions**](https://www.embarcadero.com/products/delphi/product-editions), including a Community edition that is **free to use**. For most commercial developers, the Enterprise Edition is the edition to buy, but check out the [**Feature Matrix**](https://www.embarcadero.com/docs/rad-studio-feature-matrix.pdf) to drill down into the various options available by edition. As with most business software these days, Delphi is sold on a subscription basis with an annual renewal fee. [**Click here**](https://www.embarcadero.com/products/delphi/start-for-free) to start a 30-day trial of the Architect Edition and take it for a spin. You could be an old Delphi 7 developer trying to support features in Windows 10, a frustrated .NET developer (especially all of those Visual Basic developers feeling abandoned), or simply someone looking to make a real decent living selling enterprise software produced by one of the most powerful tools available.

### **Summary**

We have no choice but to wait and see how the latest idea from Microsoft to target Win32 with **Project Reunion** plays out. One truth that we can all accept is Microsoft will never stop pushing new or re-branded technologies to keep market share for Windows as high as possible. Another is that Embarcadero will always work hard to keep VCL the very best Windows framework available. If Project Reunion isn't replaced by something different next year, then I'm quite certain that Delphi developers will be using it to leverage their code base well into the future. But regardless of Microsoft's success with their latest technology shift, Delphi is an extremely powerful tool for Windows and cross-platform development today, and well into the future.

And if you are a C++ developer, did you know that [**RAD Studio**](https://www.embarcadero.com/products/rad-studio) offers a single IDE that allows you to build Delphi and C++ applications? [**Click here**](https://www.embarcadero.com/products/rad-studio/start-for-free) to start a 30-day free trial of RAD Studio Architect Edition.

### **Some highlights from Delphi 10.4 Sydney:**

\- Improves the Windows 10 experience with the [**new TEdgeBrowser**](https://blog.marcocantu.com/blog/2020-may-edge-browser-component.html)

\- Improves High DPI support for the VCL framework

\- Adds [**per-control styling for VCL**](https://www.uweraabe.de/Blog/2020/05/15/delphi-10-4-leverages-vcl-styles/)

\- Improved FMX multi-platform support such as [**iOS storyboard support**](https://dannywind.nl/delphi/ios-storyboard/) and [**dependence on QTKit removed**](https://www.delphiworlds.com/2020/05/its-time-to-get-excited/)

\- Adds new language features like [**custom managed records**](https://blog.marcocantu.com/blog/2020-may-custom-managed-records.html)

\- [**Greatly enhances Code Insight**](https://community.idera.com/developer-tools/b/blog/posts/new-in-delphi-10-4-redesigned-code-insight) in the IDE with implementation of the Language Server Protocol

\- Provides a [**unified memory model**](https://dalijap.blogspot.com/2020/03/unified-memory-management-coming-with.html) between VCL and FMX to greatly enhance the portability of code between the two user interface frameworks

\- And even the core system libraries have been improved with new standard functions and [**performance optimizations**](https://community.idera.com/developer-tools/b/blog/posts/coming-delphi-10-4-runtime-library-enhancements).

### **Connections**

I've been a Delphi developer for over twenty years but like many others, I haven't spent a lot of time connecting with others. For whatever reason, Delphi developers seem to be some of the least active developers online. Let's change that and connect today! Please send me a connection request on [**LinkedIn**](https://www.linkedin.com/in/darianm/) and you can follow my new **Ideas Awakened Inc** company on [**Twitter**](https://twitter.com/ideasawakened), [**Facebook**](https://www.facebook.com/ideasawakened/) and [**LinkedIn**](https://www.linkedin.com/company/ideasawakened).