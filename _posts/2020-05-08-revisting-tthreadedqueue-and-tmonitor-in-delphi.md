---
layout: post
title: "Revisiting TThreadedQueue and TMonitor in Delphi"
date: 2020-05-08 12:00:00 +0000
last_modified_at: 2020-06-02 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Multithreading, iaLib, Mason Wheeler, Chris Rolliston, DelphiKB, Marco Cantu]
permalink: post/revisting-tthreadedqueue-and-tmonitor-in-delphi
published: true
image: /assets/blog/Mutlithreading/Delphi_StressTest_square.png
description: Name your threads to make debugging a multi-threaded application easier.
---
Unfortunately, both TThreadedQueue and TMonitor have a tainted history in Delphi. The original release of TThreadedQueue looked promising, with **Mason Wheeler** even classifying the new feature in Delphi XE as **Good** in his [**First look at Delphi XE**](http://tech.turbu-rpg.com/181/first-look-at-delphi-xe) blog post. However, the first few iterations were spoiled due to issues in the underlying TMonitor class as covered in many places, including on Chris Rolliston's [**blog**](https://delphihaven.wordpress.com/2011/05/25/tmonitor-redux).

There was also an active question on **StackOverflow** which covered some of the issues:

[**TThreadedQueue not capable of multiple consumers?**](https://stackoverflow.com/questions/4856306/tthreadedqueue-not-capable-of-multiple-consumers) By the time Update 4 came out for XE4, TMonitor was much better which led to a much more usable TThreadedQueue.

But there were further stumbles, as in the 10.2 Tokyo release the Grow method could corrupt the queue contents as documented in this Quality Portal issue: [**RSP-18521**](https://quality.embarcadero.com/browse/RSP-18521) This issue was corrected by 10.2.2.

There was some further discussion on [**Mason's blog**](http://tech.turbu-rpg.com/454/delayed-action-revisited) about delayed release of queued items for referenced-counted types. The suggestion by **Chris Rolliston** was "resumably all that needs doing on EMBT’s part is to assign FQueue[FQueueOffset] to Default(T) once AItem has been assigned"  In the Delphi 10.3.3 release, this does seem to be what's happening today, but I didn't find a Quality Portal issue reference to find the correction.

(A minor related side-note if your queue is processing many items per second and your service isn't rebooting very often, then be aware of the 32-bit counters overflowing as **TotalItemsPushed** and **TotalItemsPopped** are defined as [**Cardinal**](http://docwiki.embarcadero.com/Libraries/en/System.Cardinal). This is tracked in issue: [**RSP-25941**](https://quality.embarcadero.com/browse/RSP-25941).)

Unfortunately, the issues surrounding TThreadedQueue do not appear to be completely resolved as there are still a few open issues in the Quality Portal:

-   [**TThreadedQueue should wait INFINITE but doesn't. RSP-19993**](https://quality.embarcadero.com/browse/RSP-19993)
    
-   [**TThreadedQueue incorrect time during Push and Pop. RSP-21405**](https://quality.embarcadero.com/browse/RSP-21405)
    
-   [**TThreadedQueue unusable with many threads. RSP-23333**](https://quality.embarcadero.com/browse/RSP-23333)
    

I've spent a large part of the day running the test code provided in these issues. While running these tests, I finished off some tests of my own in order to exercise a single TThreadedQueue and see how well it works. I put this code in a [**GitHub Repository**](https://github.com/ideasawakened/DelphiKB) so anyone can try it out. Within this repo, the unit test project found [**here**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Tests/Source/rtl/common/System.Generics.Collections/TThreadedQueue/UnitTestQueue) runs on Delphi 10.3.3 without errors on Windows 10 with 6 logical processors. It varies the queue depth, the number of consumer threads, the number of producer threads, the task count and Push/Pop timeouts. I also ran the project on previous Delphi versions and while it works on XE4, it takes about three times as long to complete. (Using TThreadedQueue in XE4 and earlier is definitely questionable.)

After being able to run 1-40 consumer threads, 1-40 producer threads, with 1-100,000 tasks in the queue, I was feeling much better about TThreadedQueue in Delphi 10.3.3. Unfortunately, some of the test code from the above Quality Portal issues mentioned above finally started generating errors, so I had to keep going. (For future reference, testing on a VMWare Virtual Machine appears to suppress these errors significantly...it's much better to run the tests on a physical machine.)

I tweaked some of the example code from one of these issues into a new stress test project found [**here**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Tests/Source/rtl/common/System.Generics.Collections/TThreadedQueue/StressTestPopItem) and it dies fairly rapidly in 10.3.3. I added the Win64 platform and could still quickly fail the stress test. The stress test creates a bunch of worker threads and each thread has its own TThreadedQueue and it simply calls PopItem repeated on an empty queue in its Execute method. Once you get enough calls to PopItem, the method starts to unexpectedly fail with wrTimeOut after 0ms even though a PopTimeout was established on queue creation. (The queue fails to function properly after that point - the underlying [**TMonitor.Wait**](http://docwiki.embarcadero.com/Libraries/en/System.TMonitor.Wait) continuously fails.)

![Screenshot showing stress test of TThreadedQueue](/assets/blog/Mutlithreading/Delphi_StressTest_PopItem_TThreadedQueue_ScreenShot_100ms.jpg)

One may think to simply switch to the [**OmniThreadLibrary**](http://www.omnithreadlibrary.com/) and be done with this long-running affair with TThreadedQueue but the core problem is simple: as time goes on, more Runtime Library (RTL) code is leveraging TMonitor and the larger Parallel Programming Library (PPL) set of code within System.Threading, so the stability of the entire Delphi application is at stake - you cannot simply skip using TThreadedQueue and assume you are safe as the problem exposed here isn't directly with **TThreadedQueue**, rather the issue is with **TMonitor** and you cannot avoid using TMonitor as it's being leveraged by key pieces of the RTL. It already seems to be a rather major problem today - so is _Delphi Doomed_? I do not believe so. I have faith in the upcoming [**Delphi 10.4 Sydney**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.0) release that it will address some of these issues as **Marco Cantu** has stated on more than one occasion that the RTL team has applied improvements to the PPL and underlying technologies in 10.4. I'll now have two tests to run on 10.4 when it's released (and can obviously use it against all future releases.) In addition, over time I plan to add more more tests to this GitHub repo to further exercise the RTL and PPL codebase.

Today, I've only scratched the surface with these tests as it's a very narrow view into TThreadedQueue and the tests currently only run on the Win32/Win64 platforms. The RTL team has to extend this functionality over an ever-changing list of platforms and support a wide list of functionalities in the PPL and testing multi-threaded code is extremely difficult. The wide Delphi and C++ Builder community can assist with this task by contributing tests and digging deep into this platform functionality maze. Publish your tests in your own repo so others can run it on their various systems and/or create a Pull Request on this repo and I'll add your tests. The more we can hammer on this underlying codebase, the more we can help with the long-term stability of RAD Studio. The present (and future) is obviously full of multi-core systems and all the multi-threaded code that goes along with it. The old way of looking at Delphi is that it's core VCL feature set was the very best toolset available to work with the Windows API. We now need TMonitor+PPL to be just as rock-solid as they are central to the success of all future cross-platform development in Delphi. I'm simply picking on TThreadedQueue today as I was starting to implement a queue yesterday with multiple producer threads and I started flashing back some years to participating in the StackOverflow question linked above and I had to jump back into the TMonitor pool. Now, I need to get back to that original code, so where was I? But first, I could do one more tweak...