---
layout: post
title: "How to enable DEP and ASLR to reduce the attack vector of your Delphi applications on Windows"
date: 2021-12-12 12:00:00 +0000
last_modified_at: 2021-12-12 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Marco Cantu, Hallvard Vassbotn, Security]
permalink: post/enabling-dep-and-aslr-to-reduce-the-attack-vector-of-your-delphi-applications-on-windows
published: true
image: /assets/blog/Delphi-Programming/DEP.png
description: "Help further secure your native Delphi applications with DEP and ASLR on Windows with a one-line addition to the project file."
---
**TLDR**; for 32-bit applications add this line to your DPR:

````
{$SETPEOPTFLAGS $140}
````

for 64-bit applications use:

````
{$SETPEOPTFLAGS $160}
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
````

or, if you do not have WinAPI.Windows in your uses list to support using constants, then simply use their values like:

````
{$SETPEOPTFLAGS $160}
{$SetPEFlags $20}
````

![Enabled DEP in Delphi use SETPEOPTFLAGS](/assets/blog/Delphi-Programming/DEP-in-Delphi.png)

By default, Delphi applications are fairly resilient against [**buffer overflows**](http://cwe.mitre.org/data/definitions/119.html) and there was only one well-known [**published CVE**](https://nvd.nist.gov/vuln/detail/CVE-2014-0993) related to Delphi XE6 and earlier, and it was an overflow in [**VCL.Graphics.TPicture.Bitmap**](https://docwiki.embarcadero.com/Libraries/en/Vcl.Graphics.TPicture.Bitmap) due to not checking the number of colors in the image which could be overrun when processing the color palette. This was [**quickly addressed**](https://www.coresecurity.com/core-labs/advisories/delphi-and-c-builder-vcl-library-heap-buffer-overflow) with a patch and **Marco Cantu** published a [**blog post**](https://blog.marcocantu.com/blog/2014_august_buffer_overflow_bitmap.html) on the subject and here is a link to the web archive version of the related [**CodeCentral article**](https://web.archive.org/web/20190320224858/http://support.embarcadero.com/article/44015). (There is also a [**non-official workaround**](https://github.com/helpsystems/Embarcadero-Workaround) if you no longer have the source of the application.) Just because Delphi applications are resilient to attack, it does not mean that they are protected against all exploits.

Data Execution Prevention ([**DEP**](https://en.wikipedia.org/wiki/Executable_space_protection)) and Address Space Layout Randomization ([**ASLR**](https://en.wikipedia.org/wiki/Address_space_layout_randomization)) are techniques that have been proven to mitigate a number of exploits that attack native applications. DEP helps to prevent code injection by marking memory regions as non-executable as protection against buffer overflows, and ASLR helps to protect against buffer overflows by randomizing the base memory load address of the executable and the heap and stack positions to prevent hackers from assuming consistent memory addresses for specific data elements. Note that [**DEP**](https://docs.microsoft.com/en-us/windows/win32/memory/data-execution-prevention) was introduced in Windows XP SP2 and ASLR was first supported in Windows Vista.

The following was taken from an old [**blog article**](https://community.embarcadero.com/blogs/entry/delphi-2007-supports-aslr-and-nx-33777) announcing that Delphi 2007 first supported ASLR and DEP (NX page protection) and is also covered on [**Hallvard's Blog**](https://hallvards.blogspot.com/2007/03/review-delphi-2007-for-win32-beta-part_06.html).

````
The CodeGear Delphi 2007 compiler supports ASLR via any of these three techniques:**
- add the command-line switch --dynamicbase when compiling with dcc32**  
- add the preprocessor command '{$DYNAMICBASE ON}' to the source code**
- manually OR in the bit in the header, with '{$SETPEOPTFLAGS $40}' in the source code**
    
The CodeGear Delphi 2007 compiler supports NX via this technique:**
- manually OR in the bit in the header, with '{$SETPEOPTFLAGS $100}' in the source code**

The easiest way to enable both ASLR and NX is to do this:**
- add '{$SETPEOPTFLAGS $140}' to the project source file (.dpr or .dpk)**
````

64-bit executables obviously have more ability to randomize the address space with ASLR and can be harder to attack than 32-bit executables. You can further increase the memory address space of ASLR by enabling [**HIGHENTROPYVA**](https://docs.microsoft.com/en-us/cpp/build/reference/highentropyva?view=msvc-170) which requires LargeAddressAware images:

````
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
````

See [**Embarcadero's wiki page**](https://docwiki.embarcadero.com/RADStudio/en/PE_%28portable_executable%29_header_flags_%28Delphi%29) and [**Microsoft docs page**](https://docs.microsoft.com/en-us/windows/win32/debug/pe-format) for more info on PE header flags.

The values for SETPEOPTFLAGS can be found in **winnt.h** which is installed along with the Windows SDK and the values should be OR'ed together:

````
// DllCharacteristics Entries
//      IMAGE_LIBRARY_PROCESS_INIT            0x0001     // Reserved.
//      IMAGE_LIBRARY_PROCESS_TERM            0x0002     // Reserved.
//      IMAGE_LIBRARY_THREAD_INIT             0x0004     // Reserved.
//      IMAGE_LIBRARY_THREAD_TERM             0x0008     // Reserved.
#define IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA    0x0020  // Image can handle a high entropy 64-bit virtual address space.
#define IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE 0x0040     // DLL can move.
#define IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY    0x0080     // Code Integrity Image
#define IMAGE_DLLCHARACTERISTICS_NX_COMPAT    0x0100     // Image is NX compatible
#define IMAGE_DLLCHARACTERISTICS_NO_ISOLATION 0x0200     // Image understands isolation and doesn't want it
#define IMAGE_DLLCHARACTERISTICS_NO_SEH       0x0400     // Image does not use SEH.  No SE handler may reside in this image
#define IMAGE_DLLCHARACTERISTICS_NO_BIND      0x0800     // Do not bind this image.
#define IMAGE_DLLCHARACTERISTICS_APPCONTAINER 0x1000     // Image should execute in an AppContainer
#define IMAGE_DLLCHARACTERISTICS_WDM_DRIVER   0x2000     // Driver uses WDM model
#define IMAGE_DLLCHARACTERISTICS_GUARD_CF     0x4000     // Image supports Control Flow Guard.
#define IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE     0x8000
````

According to Microsoft [**docs**](https://docs.microsoft.com/en-us/previous-versions/bb430720(v=msdn.10)), ASLR and DEP are only effective when [**used together**](https://msrc-blog.microsoft.com/2010/12/08/on-the-effectiveness-of-dep-and-aslr/). Also note that there are inherent weaknesses to the ASLR protection - if a single pointer location is leaked (with an exception trace for example) then the attacker can find the offset and compute the relocated address locations if they can reproduce the same leak locally.

These advanced security options are not enabled by default due to backwards compatibility issues. However, as security continually becomes more of an issue, Windows has new options available to apply these anti-exploit mechanisms regardless of how the executables are built. In later versions of Windows, look for the **Exploit protection** in a **System Settings** screen to examine the options available. (While you can configure Windows to enforce DEP for all programs running in Windows, some well-known software, such as [**SolarWinds**](https://support.solarwinds.com/SuccessCenter/s/article/Disable-Data-Execution-Prevention?language=en_US)**,** have problems when DEP is enabled.)

We should obviously try to create applications that are able to be [**certified as secure**](https://docs.microsoft.com/en-us/windows/win32/win_cert/certification-requirements-for-windows-desktop-apps). Delphi applications targeting Windows should be able to pass the current Microsoft certifications. If you have worked with Microsoft and completed the **Windows 10 Desktop App Certification** process for your Delphi created application, please let me know via one of the contact options listed below as I would be interested in any obstacles that you encountered. A quick search in the Quality Portal yielded one related open issue with only two votes: [**RSP-35641**](https://quality.embarcadero.com/browse/RSP-35641). I have not seen too many discussions on Microsoft certifications for Delphi applications over the years, but it seems like it should be much more common.

As shown in the screenshots above, you can easily see which processes have Data execution prevention enabled in Task Manager. (You will need to add a new column for "Data execution prevention" as this option is not displayed by default.)

I have utilized [**Microsoft EMET**](https://support.microsoft.com/en-us/topic/emet-mitigations-guidelines-b529d543-2a81-7b5a-d529-84b30e1ecee0) in the past to secure my service applications running on Windows Servers to satisfy third-party compliance audits. I have not enabled these specific PE header options directly in my Delphi projects before as I relied on EMET which has been [**replaced with enhanced functionality**](https://insights.sei.cmu.edu/blog/life-beyond-microsoft-emet/) built into the Windows operating system as **Windows Defender Exploit Guard**.
