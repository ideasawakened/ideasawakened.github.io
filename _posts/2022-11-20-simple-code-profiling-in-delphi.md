---
layout: post
title: "Simple Code Profiling In Delphi"
date: 2022-07-30 12:00:00 +0000
last_modified_at: 2024-09-21 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [iaLib, AQtimePro, ProDelphi, Helmuth Adolph, GPProfile, ASMProfiler, Sampling Profiler, Eric Grange, Nexus Quality Suite]
permalink: post/simple-code-profiling-in-delphi
published: true
image: /assets/blog/Code-Profilers/iaLib-profiler.png
description: 'The blog post provide links to a few profilers available to Delphi and demonstrates a new simple to use open source profiling class.'
---
I imagine most Delphi developers have written code similar to the following to quickly test a block of code using the [**Now**](https://docwiki.embarcadero.com/Libraries/en/System.SysUtils.Now) function:

````pascal
procedure TDemoExecutionProfilerForm.DoSomeComplexCode;
var
  StartTime:TDateTime;
  DebugMessage:String;
begin
  StartTime := Now;
  Sleep(75);  //some time intensive code block
  DebugMessage := Format('DoSomeComplexCode took %dms', [MillisecondsBetween(Now, StartTime)]);
  OutputDebugString(PChar(DebugMessage));
end;
````

This type of code is quick and easy to implement and it basically works for testing the occasional block of code. Newer versions of Delphi introduced [**TStopWatch**](https://docwiki.embarcadero.com/Libraries/en/System.Diagnostics.TStopwatch) for high-performance timings that is typically now used instead of the 'Now' type code above. Alternatively, you can rely on a few full-blown code profiler tools for Delphi to examine your entire project. The most common Delphi profilers currently are (please let me know if I missed your favorite):

-   [**AQtimePro by SmartBear**](https://smartbear.com/product/aqtime-pro/overview/). This is the top of the line profiling tool available for Delphi and is priced accordingly.  NOTE: This product appears to be unsupported in 2024.
    
-   [**ProDelphi by Helmuth Adolph**](http://www.prodelphi.de/). This is another commercial profiling tool that has been around for many years (and looks like it.) Profilers aren't meant to look pretty, but this tool seems to work rather well.
    
-   [**GPProfile by Primoz Gabrijelcic**](https://github.com/ase379/gpprofile2017). There are multiple open source versions available of this instrumenting profiler with [**this link**](https://gp.17slon.com/gpprofile/index.htm) for the original site.
    
-   [**ASMProfiler by André Mussche**](https://github.com/dicaetano/asmprofiler). There are a few open source versions available of this sampling and instrumenting profiler with [**this link**](https://code.google.com/archive/p/asmprofiler/) for the Google Code version.
    
-   [**Sampling Profiler by Eric Grange**](https://www.delphitools.info/samplingprofiler/). A powerful and free sampling profiler which might be the most popular Delphi profiler in use today.
    
-   [**Nexus Quality Suite from NexusDB**](https://www.nexusdb.com/support/index.php?q=node/27156). This is a commercial product which is the retooling of the old Sleuth QA Suite from TurboPower.

- [**Intel VTune with map2pdb**](https://en.delphipraxis.net/topic/4853-map2pdb-profiling-with-vtune/)  **Anders Melander** created an open source tool to help you work with the powerful VTune application from Intel.  See the Delphi-Praxis thread for more info.
    

Besides the choice of commercial vs. open source, the main choice of which profiler to use boils down to using a "sampling" or "instrumenting" profiler, and you should visit the link above for Eric Grange's website for a nice breakdown between the two.

I was recently doing some performance analysis and revisited most of the tools above and I purchased **ProDelphi** as I have seen the product for many years but I have never tried it. To be honest, the website hasn't changed much in probably 15+ years so I never trusted it enough to make the purchase, even though it is relatively inexpensive. The purchase ended up being quick and efficient, and the product does work as expected.

![Profiling Code In Delphi](/assets/blog/Code-Profilers/Profiling-Code-In-Delphi.png)

After finding the area of code that needed some performance improvements, I implemented a slightly-better version of the simple "Now"-type code above to easily profile blocks of code without using an external tool. The replacement code ends up looking like the snippet below:

```pascal
procedure TDemoExecutionProfilerForm.DoSomeComplexCode;
begin
  TiaProfiler.StartTimer('DoSomeComplexCode');
  Sleep(75);  //some time intensive code block
  TiaProfiler.StopTimer('DoSomeComplexCode');
end;
````

If you sprinkle some of these profiling lines around the project, you can easily profile sections of code. Additionally, if you add a small block of code when your application terminates, you can save performance history to disk for comparisons over time. A simple example of saving performance results to a file is shown in the code snippet below:

````pascal
procedure TDemoExecutionProfilerForm.FormDestroy(Sender: TObject);
var
  StatsList:TStringList;
begin
  StatsList := TStringList.Create;
  try
    if TiaProfiler.ExportStats(StatsList, TiaExportResolution.Milliseconds) > 0 then
    begin
      StatsList.SaveToFile('PerformanceStats.csv');
    end;
  finally
    StatsList.Free;
  end;
end;
````

The performance stats exported for each named timer include Total Executions, Total Elapsed Time, Average Time, Min Time, and Max Time. The time resolution can be specified as Ticks or Milliseconds.

Besides the basic Stop/Stop timing methods, a few small utility methods included are **ClearTimers** and direct access to a **TimersAreEnabled** property to disable/enable timers on demand. You may want to use these to concentrate on a particular section of code by calling ClearTimers before a method is called and then disable any future timers afterwards to focus only on the timers related to a small section of code. In addition, the **Start, Stop, and ExportStats** calls are protected by a critical section for multi-threaded access.

Ideally, you would also surround all the profiling code added with a compiler define so that it is only enabled for certain builds. The simple example above has been modified so that the profiling code is conditionally executed in the following snippet:

````pascal
procedure TDemoExecutionProfilerForm.DoSomeComplexCode;
begin
  {$IFDEF PROFILER}TiaProfiler.StartTimer('DoSomeComplexCode');{$ENDIF}
  Sleep(75);  //some time intensive code block
  {$IFDEF PROFILER}TiaProfiler.StopTimer('DoSomeComplexCode');{$ENDIF}
end;
````

The profiling code is available on GitHub in the small [**iaLib**](https://github.com/ideasawakened/iaLib) shared code repository that has been used for other posts on this blog. A simple demo project is included to get started. It is a small step above the "Now" type debugging code at the start of the blog post but does not replace the need for one of the profiler tools mentioned above. If you want a commercial profiler, then I would recommend the ProDelphi tool for cost and features. If you want an open source tool, then the Sampling Profiler by Eric Grange is definitely one to consider.
