---
layout: post
title: "TMonitor event stack fix by a true Delphi MVP"
date: 2020-06-02 12:00:00 +0000
last_modified_at: 2020-06-02 12:00:00 +0000
categories: [Delphi Programming]
tags: [MultiThreading, Kiriakos Vlahos, Anders Melander, David Heffernan, Uwe Raabe, Dmitry Arefiev, DelphiKB]
permalink: post/tmonitor-event-stack-fix-by-a-true-delphi-mvp
published: true
image: /assets/blog/Delphi-Programming/MVP/EmbarcaderoMVP_Dark.png
description: TMonitor event stack proposed fix by PyScripter, Kiriakos Vlahos
---
I recently blogged about [**Revisiting TThreadedQueue and TMonitor in Delphi**](https://www.ideasawakened.com/post/revisting-tthreadedqueue-and-tmonitor-in-delphi) and it spawned some discussions including a [**thread**](https://en.delphipraxis.net/topic/2824-revisiting-tthreadedqueue-and-tmonitor/) on the English-language Delphi-PRAXiS forums where a few users participated in the discussion led by **Kiriakos Vlahos** (online user name of [**pyscripter**](https://github.com/pyscripter)) who provided the suggested corrections to the underlying code. **Anders Melander** proposed changes, and others such as [**David Heffernan**](https://stackoverflow.com/users/505088/david-heffernan) and [**Uwe Raabe**](https://www.uweraabe.de/Blog/) contributed to the discussion as well. **Dmitry Arefiev** announced today that the corrections will be in the next version (**RAD Studio 10.4 Sydney Update 1**) with some modifications due to a new cross-platform AtomicCmpExchange128 method!

Kiriakos' response to that announcement deserves repeating here:

> This is worth a certain degree of celebration. I cannot remember Embarcadero/Inprise/Borland etc. being so responsive ever. It also demonstrates the power of the collective efforts of the community to make Delphi better.

I have been a long-time fan of Delphi and I'm very glad to see this sort of response from everyone involved.

If you want to test out the proposed changes, there is a [**MonitorWaitStackFix**](https://github.com/ideasawakened/DelphiKB/blob/master/Delphi%20Tests/Source/rtl/common/System.Generics.Collections/TThreadedQueue/StressTestPopItem/MonitorWaitStackFix.pas) unit in the related stress test project folder. With this unit included, the stress test successfully completed two 24-hour test runs using Win32 and Win64 builds with Delphi 10.4 Sydney. (Configured with: 1,000 worker threads; 20ms PopTimeOut; 86,400 max runtime seconds. Executed on: Windows 10 build 18363, Intel 7-7700K 4.2GHz, 4 cores, 8 logical processors, 64GB RAM)

If you want to check out more code from Kiriakos, check out his GitHub repositories which include the popular [pyscripter](https://github.com/pyscripter/pyscripter) Python IDE with 490 stars and the [python4delphi](https://github.com/pyscripter/python4delphi) components with 327 stars. Both are likely in the top 20 Delphi-related repositories on GitHub today.

I've nominated Kiriakos to be an [Embarcadero MVP](https://www.embarcadero.com/embarcadero-mvp-program) as I think he deserves the added recognition. You could do the same, follow him on GitHub, star his repositories, and stop by the forum to say _thank you_ for his efforts on this long-standing Delphi issue!

![Picture of Delphi MVPs in Brazil 2018](/assets/blog/Delphi-Programming/MVP/Embarcadero-MVPs-Brazil-2018.jpg)

With the power of 10.4 Sydney and a community like this, it is a fun time to be a Delphi Developer! We will never see the heydays of 90+% market domination of a single vendor solution like Microsoft Windows again, but we certainly have a capable cross-platform tool that can attack the vast majority of devices out there on a number of different supported platforms. Core improvements like this at the cross platform run time library level help all Delphi developers provide the best solutions possible.
