---
layout: post
title: "Name your threads, even the ones auto-created by Delphi"
date: 2020-04-15 12:00:00 +0000
last_modified_at: 2020-06-19 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Multithreading, iaLib]
permalink: post/name-your-threads-even-the-ones-auto-created-by-delphi
published: true
image: /assets//blog/Mutlithreading/Threads_Named_square.png
description: Name your threads to make debugging a multi-threaded application easier.
---
I haven't heard of too many Delphi developers that insist on naming all threads created in an application, but I think it is good practice. Call me overly pedantic if you must, but the practice certainly has come in handy more than a few times over the years.

Now, there is a limit - I typically don't dig into third party component code to name their threads. I simply haven't had the need to do this (yet.)

Contributing to the issue is the few threads created by Delphi on application startup. There's been a few posts over the years on what each of these threads do, but I'm not as interested as what they do but interested in tagging them as typically "not my problem." I have written a handy utility method that will help. You simply include the unit in your Windows project and the few threads created automatically by Delphi will be named as such.

It's surprisingly simple to do when using the [**Tool Help Library**](https://docs.microsoft.com/en-us/windows/win32/api/_toolhelp/) in the Windows API, which are nicely wrapped in the WinAPI.TlHelp32 unit. The full unit is below, and will also be added in the [**iaLib Github repository**](https://github.com/ideasawakened/iaLib).

````pascal
unit iaWin.NameDelphiThreads;

interface
uses
  WinAPI.Windows;

  procedure NameDelphiThreads(const pMainThreadId:THandle);

implementation
uses
  System.SysUtils,
  System.Classes,
  WinAPI.TlHelp32;


procedure NameDelphiThreads(const pMainThreadId:THandle);
var
  vSnapshot:THandle;
  vProcessId:THandle;
  vTE32:TThreadEntry32;
  i:Integer;
begin
  vProcessId := GetCurrentProcessId();

  vSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, vProcessId);
  if vSnapshot <> INVALID_HANDLE_VALUE then
  begin
    try
      vTE32.dwSize := SizeOf(vTE32); //If you do not initialize dwSize, Thread32First fails.
      if Thread32First(vSnapshot, vTE32) then
      begin
        i := 1;
        repeat
          if vTE32.th32OwnerProcessID = vProcessId then
          begin
            if vTE32.th32ThreadID = pMainThreadId then
            begin
              TThread.NameThreadForDebugging('DelphiCreated_Main', pMainThreadId);
            end
            else
            begin
              TThread.NameThreadForDebugging('DelphiCreated_' + AnsiString(IntToStr(i)), vTE32.th32ThreadID);
              Inc(i);
            end;
          end;
        until not Thread32Next(vSnapshot, vTE32);
      end;
    finally
      CloseHandle(vSnapshot);
    end;
  end;
end;

{$IFDEF DEBUG}
initialization
  NameDelphiThreads(GetCurrentThreadId);
{$ENDIF}

end.
````
Simply put, instead of seeing something like this image below in your debugger **Threads** window:

![RAD Studio IDE thread list, not named](/assets//blog/Mutlithreading/Threads_NotNamed.png)

You will now see something like the image below, with the Delphi-created threads clearly identified:

![RAD Studio IDE thread list, named threads](/assets//blog/Mutlithreading/Threads_Named.png)

Granted, it is not an earth-changing difference, but you should ask yourself "How many times have you wondered what Thread Id 11376 is for?" Now, you may not know all the details on what Delphi is doing with each background thread, but you will now be able to separate the system-managed threads with the application-managed threads perhaps saving you a few headaches down the road.

The example code heavily follows Microsoft's [**thread walking**](https://docs.microsoft.com/en-us/windows/win32/toolhelp/thread-walking) documentation. You simply create a [**snapshot**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-createtoolhelp32snapshot) of all threads for the [**CurrentProcessId**](https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-getcurrentprocessid) and use the [**Thread32First**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-thread32first) and [**Thread32Next**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-thread32next) methods to process the list.

To name your own TThread descendants, use the [**TThread.NameThreadForDebugging**](http://docwiki.embarcadero.com/Libraries/en/System.Classes.TThread.NameThreadForDebugging) method. Combine that practice with this unit's functionality, you'll have the Delphi-created threads named, all of your threads named, and then you can ponder on the remaining threads listed in the IDE and perhaps go on a fishing trip to determine their source. (See this [**StackOverflow question**](https://stackoverflow.com/questions/4855638/how-can-you-find-out-who-creates-all-your-threads-in-a-delphi-program) for assistance with that task.) The majority of the time, every item in the Threads list will now be named to make your debugging task just a little bit easier.

## Update, June 2020

-   Short thread on DelphiPraxis forums: [**https://en.delphipraxis.net/topic/2677-do-you-name-your-threads-for-debugging**](https://en.delphipraxis.net/topic/2677-do-you-name-your-threads-for-debugging)
    
-   Ensure you use Ascii characters for thread name: [**RSP-17452**](https://quality.embarcadero.com/browse/RSP-17452)
    
-   Named threads work great on Windows, but be aware of issues on other platforms. See iOS: [**RSP-29625**](https://quality.embarcadero.com/browse/RSP-29625), Linux: [**RSP-22083**](https://quality.embarcadero.com/browse/RSP-22083), Mac: [**RSP-26219**](https://quality.embarcadero.com/browse/RSP-26129)