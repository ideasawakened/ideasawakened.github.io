---
layout: post
title: "DLL Injection with Delphi"
date: 2019-11-08 12:00:00 +0000
last_modified_at: 2019-11-08 12:00:00 +0000
categories: [delphi programming, example code]
tags: [Delphi, DLL Injection, dxLib, Mahdi Safsafi, Sertac Akyuz, Mathias Rauen]
permalink: post/dll-injection-with-delphi
published: true
image: /assets/blog/Microsoft-Windows/DLL-Injection/Hook_MessageBox_API.png
description: A walk through of DLL Injection using the DelphiDetours package for Delphi and some custom injection code for injecting DLLs on Microsoft Windows.
---
I had recently spent some time playing around with the simple to use **DelphiDetours** package from **Mahdi Safsafi**

[**https://github.com/MahdiSafsafi/DDetours**](https://github.com/MahdiSafsafi/DDetours)

One missing feature is the ability to inject a DLL into an external process. This is something that I wanted to do for a project that I am currently working on, so I started doing some reading. To me, the topic always seemed to be a **_Black Art_**, and something left for the assembly language developers to use, but it turns out to be much more widely accessible and fairly easy to do.

I added a few routines to my **dxLib** open source project on GitHub to assist with the DLL injection task:

[**https://github.com/darianmiller/dxLib**](https://github.com/darianmiller/dxLib)

I also put a new repository online yesterday, initially containing Delphi projects to create an example custom DLL, a basic victim process, and an example DLL Injector. This repo can be found at:

[**https://github.com/darianmiller/dxInjectionDetours**](https://github.com/darianmiller/dxInjectionDetours)

The screen shot below demonstrates a successful detour of a simple **MessageBox** call utilizing the projects from the dxInjectionDetours repo:

![Sample Windows API method intercepted](/assets/blog/Microsoft-Windows/DLL-Injection/DLL-Injection-Example.png)

Sample Windows API method intercepted

## Define which ProcessID to target

First, I wanted to offer a list of running processes, so the user can simply select a DLL injection target from a list. There's a new **dxLib\_ProcessList** unit with a simple TdxProcessEntryList class which can be populated with a call to its SnapShotActiveProcesses method:

````pascal
//https://docs.microsoft.com/en-us/windows/win32/toolhelp/taking-a-snapshot-and-viewing-processes
function TdxProcessEntryList.SnapshotActiveProcesses(const pLookupFullPathName:Boolean=False):Boolean;
````

The list is generated using the Kernel32.dll API method: [**CreateToolhelp32Snapshot**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-createtoolhelp32snapshot) and related helper routines. It's a quick snapshot of active processes (along with heaps, modules and threads used...which I may extend later.) You can use the related [**Process32First**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-process32first) and [**Process32Next**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-process32next) API methods to build a list of [**PROCESSENTRY32**](https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/ns-tlhelp32-processentry32) items that contain interesting entries like **ProcessID**, **ParentProcessID**, and **ExeFile** name. One potential problem is the ExeFile provided is just the file name without the full path.

You actually do not need the full path name to inject a DLL, but I thought it was a nice feature to add to the demo so I added an optional parameter to utilize another Kernel32 API method **QueryFullProcessImageName** which returns the full path name of a given process. There are two versions of this method available, one [**ANSI**](https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-queryfullprocessimagenamea) and one [**Unicode**](https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-queryfullprocessimagenamew).

Since I'm using this in a loop, to cache the calls to GetModuleHandle/GetProcAddress, there was a utility class created (**TdxProcessNameToId**) and a simple method to return the full path name for a given ProcessID:

````pascal
//requires Vista or later
function TdxProcessNameToId.GetFileNameByProcessID(const pTargetProcessID:DWORD):string;
````

Note that this method calls OpenProcess with the PROCESS\_QUERY\_LIMITED\_INFORMATION access right, which provides a subset of information. This reduced access grants further calls to QueryFullProcessImageName (along with GetExitCodeProcess, GetPriorityClass, and IsProcessInJob.)

**Note**: OpenProcess will fail on some system processes.

Since some system processes will generate errors, the SnapshotActiveProcesses method simply skips over them. There typically no good reasons to be sniffing into these system processes anyway. (If you need that, then you should get out of User Land and get yourself into Kernel mode.)

We now have a simple routine to easily get a full list of active processes for a user to select from, which will provide a specific **ProcessID** to target.

## Create a custom DLL to inject

The interesting part of this task involves creating the DLL which contains our API hook, intercept, detour, or the terminology of your choice. Using the DelphiDetours package, it's really simple to do.

**Remember** to 'match your bitness' - create a 32-bit DLL to inject into a 32-bit process, and a 64-bit DLL for a 64-bit process.

There is a long history of DLL support in Delphi. Unfortunately, there's not a lot of documentation provided for some areas. For example, if you look up the DLLProc global variable, you likely won't find much. Fortunately, there is a succinct [**StackOverflow**](https://stackoverflow.com/a/10377476) response by **Sertac Akyuz** which explains basic usage:

> Unfortunately when begin is executed in your dll code, the OS has already called DllMain in your library. So when your DllProc := DllMain; statement executes it is already too late. The Delphi compiler does not allow user code to execute when the dll is attached to a process. The suggested workaround (if you can call that a workaround) is to call your own DllMain function yourself in a unit initalization section or in the library code

We set this DLLProc system variable and call it ourselves with DLL\_PROCESS\_ATTACH. This is the .DPR source for an example DLL:

````pascal
library dxDetours_InterceptAPI_MessageBox;

uses
  WinApi.Windows,
  dxDetours_InterceptAPI_MessageBox_CustomImplementation in '..\..\Source\dxDetours_InterceptAPI_MessageBox_CustomImplementation.pas';

{$R *.RES}

procedure DllEntry(pReason:DWORD);
begin
   case pReason of
     DLL_PROCESS_ATTACH: CreateIntercepts();
     DLL_PROCESS_DETACH: RemoveIntercepts();
     DLL_THREAD_ATTACH:;
     DLL_THREAD_DETACH:;
   end;
end;

begin
  DllProc:= @DllEntry;
  DllEntry(DLL_PROCESS_ATTACH);

end.
````

We then need to define CreateIntercepts() and RemoveIntercepts() in our custom implementation unit as in the example below. DelphiDetours does most of the heavy lifting. We just need to define which API calls to intercept and what to replace them with.

In this example, we're going to replace the two variations of MessageBox with our own custom implementation. For this very simple example, we will call the intercepted (original version) MessageBox with our custom message and caption whenever the target process calls MessageBox:

````pascal
unit dxDetours_InterceptAPI_MessageBox_CustomImplementation;

interface
uses
  Windows,
  DDetours;

  procedure CreateIntercepts();
  procedure RemoveIntercepts();

var
  //definitions of methods to be replaced - ensure exact match to target methods
  InterceptMessageBoxA: function(hWnd: hWnd; lpText, lpCaption: LPCSTR; uType: UINT): Integer; stdcall = nil;
  InterceptMessageBoxW: function(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall = nil;

implementation


function MyCustomMessageBoxA(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
begin
  Result := InterceptMessageBoxA(hWnd, 'My custom message', 'ANSI Version Hooked', MB_OK or MB_ICONEXCLAMATION);
end;


function MyCustomMessageBoxW(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
begin
  Result := InterceptMessageBoxW(hWnd, 'My custom message', 'UNICODE Version Hooked', MB_OK or MB_ICONEXCLAMATION);
end;


procedure CreateIntercepts();
begin
  BeginHooks;
    @InterceptMessageBoxA := InterceptCreate(@MessageBoxA, @MyCustomMessageBoxA);
    @InterceptMessageBoxW := InterceptCreate(@MessageBoxW, @MyCustomMessageBoxW);
  EndHooks;
end;


procedure RemoveIntercepts();
begin
  if Assigned(InterceptMessageBoxA) then
  begin
    BeginUnHooks;
    InterceptRemove(@InterceptMessageBoxA);
    InterceptRemove(@InterceptMessageBoxW);
    InterceptMessageBoxA := nil;
    InterceptMessageBoxW := nil;
    EndUnHooks;
  end;
end;

end.
````

Thanks to DelphiDetours, this isn't a **_Black Art_** at all (at least in our code...DelphiDetours is certainly full of magic.) The major caveat is to ensure that the API method definition exactly matches the original. It's best to utilize the Delphi-provided definitions whenever possible (like those found in the Windows.pas file.)

We now have a DLL ready to inject into a target process!

## Inject your DLL into an active process

Now the fun part - actually injecting this custom DLL into a target process. (Note that there are ways to prep a process to load one or more DLLs automatically on startup which could be detailed in a future blog post.)

Unfortunately, the DelphiDetours package doesn't provide the injection capability. I've added a new unit to dxLib so handle this task: **dxLib\_WinInjection**. This has a single utility method, **InjectDLL** as defined below:

````pascal
function InjectDLL(const pTargetProcessID:DWORD; const pSourceDLLFullPathName:string; const pSuppressOSError:Boolean=True):Boolean;
````

You just need the ProcessID and the filename (including full path) of the DLL to inject and it returns success or failure.

This method calls [**OpenProcess**](https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-openprocess) with extended access rights (PROCESS\_CREATE\_THREAD or PROCESS\_QUERY\_INFORMATION or PROCESS\_VM\_OPERATION or PROCESS\_VM\_WRITE or PROCESS\_VM\_READ) It then allocates enough memory (via [**VirtualAllocEx**](https://docs.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualallocex) which returns the memory location) to write the DLL full pathname (via [**WriteProcessMemory**](https://docs.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-writeprocessmemory)) to the process memory space. It then calls [**CreateRemoteThread**](https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createremotethread) with a function pointer to **LoadLibrary** ([**ANSI**](https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibrarya) or [**Unicode**](https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibraryw) variant, depending on your Delphi version) and the memory location of the DLL to be loaded. When this occurs, the interception(s) defined in the custom DLL get put into place.

Now, there is a bit of _**Black Art**_ in this code, as the remote thread starts with a LoadLibrary call in Kernel32.dll. This will only work because LoadLibrary exists in every process at the _same virtual memory address_ (otherwise we would have to look-up the location of LoadLibrary in the target process' memory space.) We start a new thread executing LoadLibrary with the DLL full filename. This nice [**blog post**](https://www.arvanaghi.com/blog/dll-injection-using-loadlibrary-in-C/) from **Brandon Arvanaghi** goes into more detail. As his post suggests, there is an alternative way of injecting a DLL and that is to write enough space to the external process to hold the entire contents of the DLL instead of just the filename. (Perhaps we'll attempt that task for fun in a future blog post.)

I confess to reading different pieces of sample code a few times before understanding what was going on. Most samples that I found were cryptic without any comments and it seemed odd to be writing the DLL filename to some random memory location - until it finally clicked. Perhaps one way to look at it is to compare it to a simple post-it note. We write the full pathname of the DLL to a post-it note and stick it inside the memory space of the target process. This note is only used by when calling the LoadLibrary routine in a new thread, and then it's discarded/ignored.

## What's next?

Next up is to inject a custom DLL into **every active process**. In the ancient versions of Windows, it was much easier (and a very important reason why viruses were much more rampant back then!) These days, this is going to take a system driver and it's beyond the scope of our simple InjectDLL call. DelphiDetours is a nice package, but it's also missing this functionality. However, there is hope as we can fall back to a famous Delphi classic tool: [**madCodeHook**](http://madshi.net/madCodeHookDescription.htm) from **Mathias Rauen**.

Mathias is no longer offering a freeware version of this package. It's also not available for sale without some sort of background check. Apparently, this powerful tool has been used in the past by malware authors. I've contacted the author directly and was provided a purchase link. (We also connected on LinkedIn. If you are interested in this sort of topic, feel free to reach out to me as well: [**https://www.linkedin.com/in/darianm/**](https://www.linkedin.com/in/darianm/)) I plan on purchasing madCodeHook soon and implementing a system wide hook using the driver-based approach. That fun will hopefully be documented in a future blog post. I wanted to get the current task working before tackling the much larger driver-based approach.

## Further Information

When looking at intercepting Windows APIs, the industry reference is the **Detours** package directly from **Microsoft**. This was available historically as a fairly expensive commercial package but is now available on GitHub under a fee MIT open source license.

[**https://github.com/microsoft/Detours**](https://github.com/microsoft/Detours)

There is also apparently some Injection code in the venerable JEDI Code Library for Delphi, which also may be looked at in a future blog post.

[**https://github.com/project-jedi/jcl**](https://github.com/project-jedi/jcl)

Finally, I used a few tools for composing this blog post. The first is a free source code beautifier available online called **Hilite.me** by **Alexander Kojevnikov**. Simply paste code into the _Source Code_ box, pick the _Language_, optionally pick the _Style_ (I selected **vs**) and click on the _Highlight!_ button. It will generate the HTML in another edit box, as well as display a preview. It's super-easy and works great.

[**http://hilite.me/**](http://hilite.me/)

For the short-animated demo below, I used a free tool called **ScreenToGif** by **Nicke Manarin.**

[**https://www.screentogif.com/**](https://www.screentogif.com/)

[**https://github.com/NickeManarin/ScreenToGif**](https://github.com/NickeManarin/ScreenToGif)

![DLL injection demo](/assets/blog/Microsoft-Windows/DLL-Injection/DLL-Injection-Example-Screen-Capture.gif)

DLL injection demo