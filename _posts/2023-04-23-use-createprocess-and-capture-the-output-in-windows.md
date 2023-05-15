---
layout: post
title: "Use CreateProcess and capture the output in Windows"
date: 2023-04-23 12:00:00 +0000
last_modified_at: 2023-04-23 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [iaLib, Microsoft Windows]
permalink: post/use-createprocess-and-capture-the-output-in-windows
published: true
image: /assets/blog/Microsoft-Windows/Named-Pipes/PeekNamedPipe-small.png
description: 'Use CreateProcess to call a child application and capture its console output'
---
It is highly likely that most Delphi programmers already have some code in the toolbox for launching applications on Windows. I know that I have written a few different utility functions myself over the years. I was thinking about an upcoming task that I wanted to do and it involved using CreateProcess and capturing the results of the console application while the process is running. The code I wrote 15+ years ago to do this looked awful. Some newer code had CreateProcess but did not have the capture option. So, like a typical programmer that did not want to go outside much over the weekend, I finished some changes on a new utility class to handle this task.

I had an open source unit already in my [**iaLib repository**](https://github.com/ideasawakened/iaLib) on GitHub that served as a base. It already has two one-liner helper routines called **ExecAndWait** and **StartProcess**. I just added a third routine called **ExecAndCaptureOutuput**.

First, I must say this simple task kicked my butt... for a very stupid reason. I am treating this blog post as part of my recovery/therapy session. I thought I had everything rewritten correctly but my tests kept failing. I inspected / twiddled with the code multiple times and it still did not work. I have written this a few times over the years and I compared my new code to some old code and I simply did not catch the problem until an embarassing amount of time was dedicated. I looked at other implementations in Delphi and in C, C++ and just about any online reference I could find and simply did not notice the problem. I asked ChatGPT to write the code for me and it totally failed. I asked it to debug my code and it did not catch any errors.

I was using a new implementation to capture the output and I assumed this new code was faulty and I could not focus on the simple problem. I rewrote the capture routines _a few times_ always with the same result.

Finally, I started re-reading the Microsoft documentation and I stumbled upon the now-obvious answer. Specifically, when specifying the STARTF\_USESTDHANDLES flag when calling CreateProcess "[**the handles must be inheritable and the function's bInheritHandles parameter must be set to TRUE**](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/ns-processthreadsapi-startupinfoa)**.**" Silly me - the default value for the value being used for the `bInheritHandles` parameter in this current implementation was set to `False`. (In the old implementations it was `True` and it took a little while for me to notice the difference.)

Now that capturing the Standard Output, I quickly added support capturing Standard Error. If you are capturing commands like `dir \*.xyz` you will want to see Standard Error as it will contain the relevant error message such as `File not found.`

## Use CreatePipe to capture the output of a child process

When your child process writes to the standard output/screen we want to grab it in the parent process for logging or to otherwise take custom action on the content. You can call [**CreatePipe**](https://learn.microsoft.com/en-us/windows/win32/api/namedpipeapi/nf-namedpipeapi-createpipe) to setup an [**anonymous pipe**](https://learn.microsoft.com/en-us/windows/win32/ipc/anonymous-pipes) which you read from one side and the child process writes to the other side with Windows managing the buffering in between.

````c
BOOL CreatePipe(
  [out]          PHANDLE               hReadPipe,
  [out]          PHANDLE               hWritePipe,
  [in, optional] LPSECURITY_ATTRIBUTES lpPipeAttributes,
  [in]           DWORD                 nSize
);
````

Pass in a handle for your parent process to use for reading and one for your child process to use for writing. These are "out" parameters and their values will be established after a successful call. You also need to create TSecurityAttributes variable and set the bInheritHandle property to True.

If that calls succeeds, you should then set the InheritHandle property of your read handle to zero with a call to [**SetHandleInformation**](https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-sethandleinformation) as we only want the child to inherit the write handle. The child process only gains access to a handle if it is inheritable. One of my various implementations used [**DuplicateHandle**](https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-duplicatehandle) to manage the inheritable property of these handles which is something many other implementations have used. I do not believe there is any benefit of DuplicateHandle over SetHandleInformation. If someone thinks I am doing it horribly wrong, please let me know of my errors. (One of the benefits of writing a blog post is that you get free code reviews and you are typically told when you are wrong. I see that as a positive.)

````c
BOOL SetHandleInformation(
  [in] HANDLE hObject,
  [in] DWORD  dwMask,
  [in] DWORD  dwFlags
);
````

## Customize StartupInfo and CreationFlags used in CreateProcess

CreateProcess uses a [**STARTUPINFOW**](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/ns-processthreadsapi-startupinfow) structure to configure the creation process which Delphi has pre-defined as a **TStartupInfo** record.

````c
BOOL CreateProcessW(
  [in, optional]      LPCWSTR               lpApplicationName,
  [in, out, optional] LPWSTR                lpCommandLine,
  [in, optional]      LPSECURITY_ATTRIBUTES lpProcessAttributes,
  [in, optional]      LPSECURITY_ATTRIBUTES lpThreadAttributes,
  [in]                BOOL                  bInheritHandles,
  [in]                DWORD                 dwCreationFlags,
  [in, optional]      LPVOID                lpEnvironment,
  [in, optional]      LPCWSTR               lpCurrentDirectory,
  [in]                LPSTARTUPINFOW        lpStartupInfo,
  [out]               LPPROCESS_INFORMATION lpProcessInformation
);
````

Once you have established your pipe, you assign its hWritePipe handle value to **StartupInfo.hStdOutput** for capturing Standard Output (and either create a second pipe for Standard Error or simply re-use the same handle to get all error output intermixed with the standard output. Either way, assign its hWritePipe handle to **StartupInfo.hStdError.**)

You need to inform Windows that you want the child process to use custom handles by setting the **STARTF\_USESTDHANDLES** value in **StartupInfo.dwFlags**. And, do not be like me and forget to also assign True to the bInheritHandles parameter in the CreateProcess call.

If you only want to capture one or two of the three standard handles (hStdInput, hStdOutput, hStdError) then set the other handles to their default values using [**GetStdHandle**](https://learn.microsoft.com/en-us/windows/console/getstdhandle) such as:

````pascal
StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
````

You may also want to set the **StartupInfo.wShowWindow** state to **SW\_HIDE** to hide the child window from displaying.

Finally, you will want to set the **CREATE\_NEW\_CONSOLE** flag in ProcessCreationFlags to prevent both processes from being attached to the same console (as there is no guarantee that the correct process will receive the input intended for it.)

As soon as CreateProcess completes, you should call CloseHandle on the hWritePipe handle(s) that you created or the pipe will not close properly when the child process exits.

## WaitForProcessCompletion becomes a little more complex

Typically, it is fairly simple to wait for a child process to complete as you simple use [**WaitForSingleObject**](https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitforsingleobject) with the handle of the child process along using an INFINITE timeout. We do not want to block on waiting for the process to end as it would be nice to get the output while the process is running so you either [**use threading**](https://stackoverflow.com/a/19070738) to capture the output in the background or you use a polling mechanism which I have done using the PeekNamedPipe Windows API function.

![PeekNamePipe in Delphi](/assets/blog/Microsoft-Windows/Named-Pipes/PeekNamedPipe.png)

You can set up a loop of WaitForSingleObject with a short timeout value. And during this loop use [**PeekNamedPipe**](https://learn.microsoft.com/en-us/windows/win32/api/namedpipeapi/nf-namedpipeapi-peeknamedpipe) to see if there is any data waiting to be read and if so use [**ReadFile**](https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile) to pull the data from the pipe. You will have three possible results: a successful read, an **ERROR\_BROKEN\_PIPE** (error #109) which occurs when the client process has exited, or an API failure. The PeekRead method is reproduced below:

````pascal
 function PeekRead(const hOurRead:THandle; const EventToCall:TOnCaptureProc):TReadPipeResult;
  var
    BytesAvailable:DWord;
    BytesRead:DWord;
    PipeData:TBytes;
  begin
    Result := TReadPipeResult.BrokenPipe;

    if PeekNamedPipe(hOurRead, nil, 0, nil, @BytesAvailable, nil) then
    begin
      if BytesAvailable = 0 then Exit(TReadPipeResult.Success);

      SetLength(PipeData, BytesAvailable);
      if ReadFile(hOurRead, PipeData[0], BytesAvailable, BytesRead, nil) then
      begin
        Result := TReadPipeResult.Success;

        if BytesRead < BytesAvailable then
        begin
          SetLength(PipeData, BytesRead);
        end;

        if Assigned(EventToCall) then
        begin
          EventToCall(CaptureEncoding.GetString(PipeData));
        end;
      end
      else
      begin
        Result := TReadPipeResult.APIFailure;
        CaptureSystemError('ReadFile after PeekNamedPipe failed');
      end;
    end
    else if GetLastError <> ERROR_BROKEN_PIPE then
    begin
      Result := TReadPipeResult.APIFailure;
      CaptureSystemError('PeekNamedPipe failed');
    end;
  end;
````

Put all this together, and you can have a one-line method to execute a command and capture its output. In this simplistic example, the output is added to a memo on the form:

````pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ExecAndCaptureOutuput('dir *.txt', OutputCapture, ErrorCapture);
end;

procedure TForm1.OutputCapture(const Capture:string);
begin
  Memo1.Lines.Add(Capture);
end;

procedure TForm1.ErrorCapture(const Capture:string);
begin
  Memo1.Lines.Add('Error: ' + Capture);
end;
````

I hope you find this utility class useful. Please let me know if you have any comments or suggestions. The majority of the code can be found on GitHub within [**iaRTL.ProcessExecutor.Windows.pas**](https://github.com/ideasawakened/iaLib/blob/master/Source/iaRTL.ProcessExecutor.Windows.pas)
