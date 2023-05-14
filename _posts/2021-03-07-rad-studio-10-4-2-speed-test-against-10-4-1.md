---
layout: post
title: "Speed test results building Spring4D in Delphi 10.4.2 versus 10.4.1"
date: 2021-03-07 12:00:00 +0000
last_modified_at: 2021-03-07 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [10.4 Sydney, Jim McKeeth, Allen Bauer, Andreas Hausladen, Andrew Holubovski, Roman Kassebaum, Dimitris Kounalakis, Uğur Buğra Özkaya]
permalink: post/rad-studio-10-4-2-speed-test-against-10-4-1
published: true
image: /assets/blog/RAD-Studio-10.4-Sydney/Delphi-Fast-build-Speed-square.png
description: "Blog post covers a speed test of the Spring4D Framework with 10.4.1 and 10.4.2 resulting in a large performance gains simply by upgrading to the latest version of Delphi."
---
**Jim McKeeth**'s new [**blog post**](https://blogs.embarcadero.com/delphi-10-4-2-compiler-speed-improvements/) covered a [**YouTube video**](https://youtu.be/lBSpv94FDE4) he made comparing building the well-known [**HeidiSQL**](https://github.com/HeidiSQL/HeidiSQL/) open source project in RAD Studio 10.4.1 and then he rebuilt that same project in 10.4.2 resulting in a very respectable 40% faster build times when using the Win32 compiler and 8% faster build when targeting Win64. One item impressive to me was that Jim ran the 10.4.1 test on a physical machine and the 10.4.2 test was executed on a virtual machine hosted by that machine, possibly understating the potential increases.

![Delphi 10.4.2 speed test results with the HeidiSQL project](/assets/blog/RAD-Studio-10.4-Sydney/Delphi-10.4.2-Compile-speed-increases.png)

I quickly covered these improvements in a recent [**blog post**](https://www.ideasawakened.com/post/rad-studio-10-4-2-integrates-over-30-ide-fixpack-changes). After seeing the impressive results from Jim, I completed my own set of tests on two identically configured virtual machines on the same physical host. I selected the [**Spring4D Framework**](https://bitbucket.org/sglienke/spring4d) as my test project to build (using the master branch, dated 6/19/2020.)

I used the Build.exe utility which ships with Spring4D to perform the builds after modifying it slightly so it displays the elapsed time of the build process once completed. Here are the results of building Win32 and Win64 debug configurations.

**Delphi 10.4.1 results**

-   Time to build: 64245 ms
    
-   Time to build: 63366 ms
    
-   Time to build: 63466 ms
    
-   Average build time: 63692.3 ms
    

**Delphi 10.4.2 results**

-   Time to build: 52460 ms
    
-   Time to build: 52713 ms
    
-   Time to build: 51995 ms
    
-   Average build time: 52389.3 ms
    

This is a pretty impressive 17.7% faster build time simply by upgrading the project to 10.4.2. I then noticed that the build process was also running tests, so I did another batch of builds and this time I deselected the tests and also executed separate Win32 and Win64 builds which yielded the following results:

**Delphi 10.4.1, Win32 build results**

-   Time to build: 15863 ms
    
-   Time to build: 14797 ms
    
-   Time to build: 14838 ms
    
-   Time to build: 15432 ms
    
-   Average build time: 15232.5 ms
    

**Delphi 10.4.2, Win32 build results**

-   Time to build: 14137 ms
    
-   Time to build: 14253 ms
    
-   Time to build: 14518 ms
    
-   Time to build: 14903 ms
    
-   Average build time: 14452.75 ms
    

**Delphi 10.4.1, Win64 build results**

-   Time to build: 47113 ms
    
-   Time to build: 47625 ms
    
-   Time to build: 48513 ms
    
-   Time to build: 47418 ms
    
-   Average build time: 47667.25 ms
    

**Delphi 10.4.2, Win64 build results**

-   Time to build: 36868 ms
    
-   Time to build: 37046 ms
    
-   Time to build: 36776 ms
    
-   Time to build: 37953 ms
    
-   Average build time: 37160.75 ms
    

Somewhat surprisingly, the greatest increase in speed was found with the Win64 builds at 22% faster build times while the Win32 builds were 5% faster. This is different than the HeidiSQL project test which resulted in a huge increase in Win32 builds and a much smaller increase in Win64 builds. Even though these two are very different projects, all builds with Delphi 10.4.2 were faster than the same builds with Delphi 10.4.1.

Delphi has long been known for being fast; however, it is once again fast out of the box. **Embarcadero has committed to breaking speed records with their latest release.**

![Major compiler speed improvements in Delphi 10.4.2](/assets/blog/RAD-Studio-10.4-Sydney/Delphi_Fast_Build_Speed_In_10.4.2.png)

Jim also was kind enough to reveal the full list of [**IDE Fix Pack**](https://www.idefixpack.de/blog/ide-tools/ide-fix-pack/) changes that were implemented in 10.4.2, which I will re-list here:

1.  FileSystem
    
2.  SearchUnitNameInNS
    
3.  GetUnitOf
    
4.  CacheControl
    
5.  FileNameStringFunctions
    
6.  KibitzIgnoreErrors
    
7.  RootTypeUnitList
    
8.  MapFile.fprintf
    
9.  Unit.RdName
    
10.  PrefetchToken
    
11.  StrLenCalls
    
12.  WarnLoadResString
    
13.  DbkGetFileIndex
    
14.  UnlinkImports
    
15.  ResetUnits
    
16.  KibitzCompilerImplUnitReset
    
17.  UnlinkDuringCompile
    
18.  UnitFreeAll
    
19.  UnitFindByAlias
    
20.  SymLookupScope
    
21.  ImportedSymbol
    
22.  NoUnitDiscardAfterCompile
    
23.  SourceOutdated
    
24.  MapFileBuffer
    
25.  BackgroundCompilerFileExists
    
26.  DrcFileBuffer
    
27.  Package.CleanupSpeed
    
28.  Optimization
    
29.  FindPackage
    
30.  x64.JumpOpt
    
31.  x64.SymTabHashTable
    
32.  ReleaseUnusedMemory
    
33.  FileNameStringFunctions
    
34.  Memory.Shrink
    

Note that there are always some improvements made which might actually decrease performance of your particular environment. One such improvement is the [**package cache**](https://community.embarcadero.com/blogs/entry/demandloaded-component-packages-in-delphi-2005-1833) which was implemented in Delphi 2005 by **Allen Bauer**. It seems that you can often gain some increased IDE performance by disabling this package cache with the undocumented "-**nocache"** command line parameter. Similarly, some of the implemented IDE Fix Pack improvements may have positive or negative effects on your specific results. However, **the improvements in 10.4.2 have proven to provide a very nice overall decrease in average build times** with both the HeidiSQL and Spring4D tests. After 26 years of continuous Delphi development, it is likely very difficult to get performance increases across the board for all types of projects, but it does appear that Embarcadero has managed to achieve a nice balance in 10.4.2.

**NOTE**: A big thank-you out to [**Andreas Hausladen**](https://github.com/ahausladen) for his IDE Fix Pack efforts over the years! (And to Embarcadero for integrating, extending and supporting the changes going forward.)


## Update: test results from others

**2021-03-08:** Comment received from **Andrew Holubovski** on LinkedIn who reports **38% speed increase for Win32 and 24% speed increase for Win64**.

[![Comment from Andrew Holubovski](/assets/blog/RAD-Studio-10.4-Sydney/Andrew_Holubovski_20210308.png)](https://www.linkedin.com/feed/update/urn:li:groupPost:1290947-6773752994241609728?commentUrn=urn%3Ali%3Acomment%3A%28groupPost%3A1290947-6773752994241609728%2C6774718559017803776%29)

**2021-03-03:** Twitter post from **Roman Kassebaum** states **10.4.2 is the fastest Delphi compiler I've ever seen!** On his related [**blog post**](https://t.co/HlTRJNYrgu?amp=1) he reports a project with 2.3 million lines of code took 2.5 minutes to compile in 10.3.3 now compiles in 1.5 minutes with 10.4.2.

[![Comment from Roman Kassebaum](/assets/blog/RAD-Studio-10.4-Sydney/roman_kassebaum_2021-03-03.png)](https://twitter.com/RomanKassebaum/status/1367191829268037635)

**2021-02-28:** Comment received from **Dimitris Kounalakis** on Facebook that **compile time is 50% less** for a big project.

[![Comment from Dimitris Kounalakis](/assets/blog/RAD-Studio-10.4-Sydney/Dimitris_Kounalakis_2021.02.28.png)](https://www.facebook.com/groups/delphidevelop/permalink/1106277023171223/?comment_id=1106281743170751)

## Update: 10.4.2 compared to 10.3.3

**2021-03-10:** Comment received from **Uğur Buğra Özkaya** [**on LinkedIn**](https://www.linkedin.com/feed/update/urn:li:groupPost:101829-6773752941988974592?commentUrn=urn%3Ali%3Acomment%3A%28groupPost%3A101829-6773752941988974592%2C6775304334415581184%29) about the availability of testing other versions like Tokyo or Rio, so I ran another batch of tests on Rio with all of the related updates (these results are from a fully patched version 10.3.3.) These builds were on an identically configured VM on the same host as the previous tests.

**Delphi 10.3.3, Win32 build results**

-   Time to build: 25003 ms
    
-   Time to build: 18894 ms
    
-   Time to build: 19026 ms
    
-   Time to build: 23441 ms
    
-   Average build time: 21591 ms
    

10.4.1 Win32 averaged 15232.5 ms so they had already gained very large increases in speed (29%) with the Sydney release before this big batch of IDE Fix Pack fixes in 10.4.2.

Compiling the same Win32 project in 10.3.3 versus 10.4.2 results in a **33% reduction in overall build time**.

**Delphi 10.3.3, Win64 build results**

-   Time to build: 51825 ms
    
-   Time to build: 51395 ms
    
-   Time to build: 51065 ms
    
-   Time to build: 52610 ms
    
-   Average build time: 51723.75 ms
    

10.4.1 Win64 averaged 47667.25 ms so the earlier Syndey releases resulted in about 8% decrease in Win64 build time before this big batch of IDE Fix Pack additions in 10.4.2.

Compiling the same Win64 project in 10.3.3 versus 10.4.2 results in a **28% reduction in overall build time**.