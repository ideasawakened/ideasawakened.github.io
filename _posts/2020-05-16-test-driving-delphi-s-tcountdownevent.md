---
layout: post
title: "Test driving Delphi's TCountdownEvent"
date: 2020-05-16 12:00:00 +0000
last_modified_at: 2020-05-16 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Mutlithreading, DelphiKB]
permalink: post/test-driving-delphi-s-tcountdownevent
published: true
image: /assets/blog/Mutlithreading/CountDownEvent-square.png
description: Introducing three GitHub projects testing out TCountdownEvent, one of the cross platform synchronization primitives supported by Delphi.
---
In this post, we take a look at [**TCountdownEvent**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent) from the [**System.SyncObjs.pas**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs) unit, first introduced back in Delphi XE. To be honest, I haven't used this class before. It's a shame too, as it seems like a very useful tool that can handle the common situation where one thread is waiting on a few others to finish their work. It will likely save me a lot of effort in the future! Let's hope after reading this article, you will feel the same.

![TCountdownEvent synchronization primitive](/assets/blog/Mutlithreading/CountDownEvent-Synchronization-Primitive.png)

A typical use case is that a single thread spawns a few activities (either with tasks in a ThreadPool or simply utilizing several TThreads directly) and then waits for those threads to finish before continuing. This normally complicated fork/join process is easily accomplished when utilizing a shared CDE between the participants. A CDE signals when the count reaches zero and is sometimes referred to as a _reverse semaphore_.

To reflect the actions in the example diagram above, you would create a CDE with an initial count of 1 for your main thread. Then, you would add one count to the CDE for each task created or child thread spawned. You then pass the CDE instance to the child task/thread so they can signal it when their work is done. (Note to mind your [**dangling pointers**](https://en.wikipedia.org/wiki/Dangling_pointer) and ensure the controller is freed only after the task threads.) Once the child tasks are operating, the main thread can optionally [**.Signal**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent.Signal) the CDE and then call the [**.WaitFor**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent.WaitFor) method which returns when the work is done. Of course, if that controlling thread has other work to do it can simply go about its business and periodically poll the CDE to see if the child threads are done by examining the [**.IsSet**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent.IsSet) property. TCountdownEvent provides a very simple coordination of parallel processing across multiple tasks and is not limited by the MAXIMUM\_WAIT\_OBJECTS (64) of the commonly used [**WaitForMultipleObjects**](https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitformultipleobjects) from the Windows API.

It sounds easy - but there might also be a few curiosities. One of the implementation details is that if you call [**.AddCount**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent.AddCount) when the [**.CurrentCount**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent.CurrentCount) is zero, an exception is raised. If the count is 0, the CDE is Signaled and needs to be [**.Reset**](http://docwiki.embarcadero.com/Libraries/en/System.SyncObjs.TCountdownEvent.Reset).

It took a little hands-on coding for me to reveal these (now obvious) implementation details. I first created a few example programs that kind of worked and played with the sample program that comes with Delphi until it finally clicked. What helped me the most was creating a UnitTest project for TCountdownEvent to figure out some of these details. This test project validates the various ways exceptions are created, which are listed below.

### How to raise an exception with TCountdownEvent:

1.  Use a negative count on Create
    
2.  Adding a Count which makes CurrentCount greater than MaxInt
    
3.  Adding a zero Count
    
4.  Adding a negative Count
    
5.  Resetting a negative Count
    
6.  Calling Add when Count is Zero
    
7.  Calling Signal when Count is Zero
    
8.  Calling Signal with a Count above CurrentCount
    

I was a little concerned when coming across each new exception, but once you put them all together, the rules make sense. I'm also not fully comfortable yet with the idea of Signaling more than one at a time, but it will probably be useful if you are working with percentage availability of certain resources.

### Three Test Drive Projects available on GitHub

A [**UnitTest project**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Tests/Source/rtl/common/System.SyncObjs/TCountdownEvent/UnitTest) currently supports the Win32 and Win64 platforms, but the great thing about Delphi these days is that it now supports just about everything so this project can be easily extended.

A second [**demo project**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Tests/Source/rtl/common/System.SyncObjs/TCountdownEvent/Demo.WaitThread) is a slightly expanded version of the Demo that comes shipped with Delphi. It splits the worker thread into a separate unit and helps to accentuate the actions being taken with minor tweaks. This demo has the main thread performing the countdown process while the worker thread waits.

A third [**fork-join project**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Tests/Source/rtl/common/System.SyncObjs/TCountdownEvent/Demo.VCLForkJoin) is a simple representation of the diagram above and consists of a standard VCL application with a memo and a button to activate a child controller thread. The spawned controller spins up three other worker threads and updates the GUI when the work is completed.

For this fork-join project, there's not a lot of code involved. Below is the ParallelTask unit which is a custom TThread descendant with a simplistic DoSomeWork routine. Note that the CDE is passed into the constructor.

````pascal
unit iaExample.CDE.ParallelTask;

interface
uses
  System.Classes,
  System.SyncObjs;

type

  TExampleParallelTask = class(TThread)
  private
    fCDE:TCountdownEvent;
  protected
    procedure Execute(); override;
    procedure DoSomeWork();
  public
    constructor Create(const pCDE:TCountdownEvent);
  end;

implementation
uses
  System.SysUtils;

constructor TExampleParallelTask.Create(const pCDE:TCountdownEvent);
begin
  fCDE := pCDE;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TExampleParallelTask.Execute();
begin
  NameThreadForDebugging('ExampleCDEWorkerThread');
  DoSomeWork();
  fCDE.Signal();
end;

procedure TExampleParallelTask.DoSomeWork();
begin
  Sleep(Random(2000));
end;

end.
````

Next is the controller which spawns the child threads to do the work. Note that the CDE is created with an initial count of 1 and the WorkerThreadCount is added when spinning up the child threads. Since we added 1 for the controller, we need to .Signal before calling .Waitfor. (You could easily change this example to create the CDE with a count of 0, call **.Reset(WorkerThreadThread)** instead of **.AddCount(WorkerThreadCount)**, and then you wouldn't need to call **.Signal** in this controller thread before the **.WaitFor**.)

````pascal
unit iaExample.CDE.ControllerThread;

interface
uses
  System.Classes,
  System.SyncObjs,
  Vcl.StdCtrls;

type

  TExampleControllerThread = class(TThread)
  private const
    WorkerThreadCount = 3;
  private
    fCDE:TCountdownEvent;
    fCallBackMemo:TMemo;
  protected
    procedure Execute(); override;
    procedure CallBack(const pText:string);
    procedure Fork();
    procedure Join();
  public
    constructor Create(const pCallbackTo:TMemo);
    destructor Destroy(); override;
  end;

implementation
uses
  System.SysUtils,
  iaExample.CDE.ParallelTask;

constructor TExampleControllerThread.Create(const pCallbackTo:TMemo);
begin
  fCDE := TCountdownEvent.Create(1);
  fCallBackMemo := pCallbackTo;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TExampleControllerThread.Destroy();
begin
  fCDE.Free();
  inherited;
end;

procedure TExampleControllerThread.Execute();
begin
  NameThreadForDebugging('ExampleCDEControllerThread');
  CallBack('Creating worker threads');

  Fork();
  CallBack('Controller waiting for workers to finish');
  Join();

  CallBack('Work completed');
end;

//spin up some child threads
procedure TExampleControllerThread.Fork();
begin
  fCDE.AddCount(WorkerThreadCount);

  for var i := 1 to WorkerThreadCount do
  begin
    TExampleParallelTask.Create(fCDE);
  end;
end;

//wait for the children to be done with their work
procedure TExampleControllerThread.Join();
begin
  fCDE.Signal();
  fCDE.WaitFor();
end;

procedure TExampleControllerThread.CallBack(const pText:string);
begin
  Synchronize(procedure()
              begin
                fCallBackMemo.Lines.Add(FormatDateTime('hh:nn:ss.zzzz', Now) + ' ' + pText);
              end);
end;

end.
````

Finally, this is the implementation section to the main form unit. We simply launch the controller thread when a button is clicked. The memo is updated with actions taken by the controller:

````pascal
uses
  iaExample.CDE.ControllerThread;

{$R *.dfm}

procedure TfrmMain.butPerformWorkClick(Sender: TObject);
begin
  TExampleControllerThread.Create(memLog);
end;
````

The TCountdownEvent class is a powerful tool in the ever-growing Delphi toolbox. It's been available for nearly a decade and I'm glad that I finally looked into it. My only excuse is that there is so much cool stuff that Delphi provides with each passing update, it's pretty difficult to keep up with all it. [**Delphi 10.4 Sydney**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.0) is about to be released and it's full of new and improved features - I hope some of those features won't take another decade for me to finally use!

Take this test drive yourself by cloning the above projects on [**GitHub**](https://github.com/ideasawakened/DelphiKB). Create a Pull Request for any corrections or additions that you may have. After working with Tasks, I've found that you can accomplish similar results by using chained tasks with a thread pool. Perhaps that will be a follow-up post. Another related task would be to extend this CDE to include a coordinated way to cancel the active work...