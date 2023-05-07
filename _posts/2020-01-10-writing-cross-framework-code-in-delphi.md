---
layout: post
title: "Writing Cross Framework code in Delphi"
date: 2020-01-10 12:00:00 +0000
last_modified_at: 2022-03-17 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Cross Platform, Uwe Raabe, Scott Hollows, iaLib]
permalink: post/writing-cross-framework-code-in-delphi
published: true
image: /assets/blog/Delphi-Programming/CrossPlatformCompilerDefine-Square.png
description: Working with Cross Framework code in Delphi, and introducing a Screen Cursor Stack for use with the VCL and FMX GUI frameworks.
---
Sharing Delphi code between your FMX and VCL applications is pretty easy for non-visual functionality, with the exception of [**zero-based strings**](https://stackoverflow.com/questions/19488010/how-to-work-with-0-based-strings-in-a-backwards-compatible-way-since-delphi-xe5) (and ARC on mobile platforms which is being [**rolled back soon**](https://twitter.com/marcocantu/status/1197509156493778945?ref_src=twsrc%5Etfw%7Ctwcamp%5Eembeddedtimeline%7Ctwterm%5Eprofile%3Adelphicz%7Ctwcon%5Etimelinechrome&ref_url=https%3A%2F%2Ftranslate.googleusercontent.com%2Ftranslate_c%3Fdepth%3D1%26rurl%3Dtranslate.google.com%26sl%3Dcs%26sp%3Dnmt4%26tl%3Den%26u%3Dhttps%3A%2F%2Fdelphi.cz%2Fpost%2FDelphi-103-Rio-Release-3.aspx%26xid%3D17259%2C15700022%2C15700186%2C15700190%2C15700256%2C15700259%2C15700262%2C15700265%2C15700271%2C15700283%26usg%3DALkJrhgYw1GRT7C5jBu8c7_DKJO4SxsNiA) to the traditional Delphi memory model.) However, it does get a little tricky when it involves dealing with GUI controls in a cross-framework manner.

One problem is there is no built-in way to detect if the application is based on the FMX or VCL framework. There has been some discussion on this, and one way suggested on a July 2017 [**forum post by Uwe Raabe**](https://www.delphipraxis.net/1376489-post6.html) (and repeated on a January 2019 [**StackOverflow response**](https://stackoverflow.com/questions/54093217/is-there-a-way-to-find-what-kind-of-framework-i-use-in-a-delphi-datamodule)) is to edit the **UserTools.proj** file and add some XML which will produce a compiler define that determines the framework type. This works for any project _on that machine_ but since it's editing a sparsely documented file in your AppData directory, and every member of your dev team needs to also maintain this custom file on their machine (which is typically not in version control), I do not think this is a good approach. (_Sparsely documented_ may be too kind - is there **any** documentation at all on UserTools.proj?)

In an October 2016 [**blog post**](https://scotthollows.com/2016/10/13/delphi-conditional-compile-vcl-firemonkey/) by **Scott Hollows**, there was some discussion for VCL/FMX conditional compilation and the suggested response was to check the FireMonkeyVersion constant in the FMX.Types unit:

````pascal
{$IF not declared(FireMonkeyVersion)}
  ShowMessage('VCL');
{$ELSE}
  ShowMessage('FMX');
{$IFEND}
````

This does appear to work - but _only_ if FMX.Types is included in your Uses clause. If you are trying to include specific units in your Interface section based on the application's target Framework, then this doesn't seem to be a solution. Perhaps I haven't had enough coffee this morning, but this simply does not work for me. (**Update**: You can rely on the special Defined block for this...see [**wiki**](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/IF_directive_(Delphi)) article.)

![Cross platform compiler defines in Delphi](/assets/blog/Delphi-Programming/CrossFrameworkCompilerDefine.png)

The best solution that I have found is to fill in the gap that Embarcadero has left and simply add a new [**conditional symbol**](http://docwiki.embarcadero.com/RADStudio/Rio/en/Conditional_compilation_(Delphi)) per project. It will follow along with your project in version control, and it makes cross-framework code easier to implement with little effort. There's a project variable that can be leveraged for this purpose: **$(FrameworkType)** You can add this to your project options (as in the image above) and then support code like:

````pascal
uses
  System.SysUtils,
  {$IFDEF FMX}
  FMX.Forms,
  {$ELSE}
  VCL.Forms,
  {$ENDIF}
  WinAPI.Windows;
````

You could simply stop there, but if you fail to add the compiler define to a project, then the code above will assume you are targeting the VCL framework. I prefer to not rely on the fact that I will remember to add the compiler define, so I have a special include file which validates the build:

````pascal
{$IFNDEF VCL}
  {$IFNDEF FMX}
    //No built-in way of knowing the difference between applications targeted for FMX or VCL
    //This project is relying on a specific workaround:
    //   Add a project conditional define: $(FrameworkType) which will result in VCL or FMX
    //   This is typically added to the project config: "All Configurations"->"All Platforms"->Conditional Defines

    {$Message Fatal 'VCL or FMX not defined, aborting.  This application contains cross-framework references - add $(FrameworkType) as a conditional define in this project''s configuration.'}

  {$ENDIF}
{$ENDIF}
````

When I am developing a unit that works with cross-framework code, I add this include file to the header of the unit to ensure that the required framework define is present.

_Note_: you could implement your own conditional symbol like FMX\_FRAMEWORK and VCL\_FRAMEWORK instead of relying on the $(FrameworkType) variable, especially if you are building iOS applications with Delphi XE4 as you will apparently need to support the [**FMI framework**](https://wiert.me/2013/11/28/delphi-dproj-files-frameworktype-and-formtype-via-embarcadero-discussion-forums/). I started using my own custom symbols until I stumbled across $(FrameworkType) and I switched to this built-in variable.

### Example Cross Framework Code

I've added a cross framework **ScreenCursorStack** to the fairly new [**iaLib repository**](https://github.com/ideasawakened/iaLib) on GitHub along with VCL and FMX demo projects to demonstrate how VCL and FMX projects can use identical code for manipulating the screen cursor:

````pascal
implementation
uses
  ExampleWorker,
  iaRTL.ScreenCursorStack;


//clicking Toggle and then Example work should handle nested requests
procedure TScreenCursorStackDemoForm.butToggleCursorClick(Sender: TObject);
begin
  if butToggleCursor.Tag = 0 then
  begin
    TiaScreenCursorStack.PushCursor(crHourGlass);
    butToggleCursor.Tag := 1;
  end
  else
  begin
    TiaScreenCursorStack.PopCursor;
    butToggleCursor.Tag := 0;
  end;
end;

procedure TScreenCursorStackDemoForm.butExampleWorkClick(Sender: TObject);
begin
  TiaScreenCursorStack.PushCursor(crHourGlass);
  try
    TExampleWorker.SimulateWork(2000);
  finally
    TiaScreenCursorStack.PopCursor;
  end;
  ShowMessage('Work completed!');
end;
````

I currently don't do a lot of cross framework coding, but this approach seems to be a way around the missing Framework compiler define. While writing this blog post today, I've put in a new feature request [**RSP-27405**](https://quality.embarcadero.com/browse/RSP-27405) to provide a built-in conditional symbol to determine the target framework. If you agree with the approach, please add your vote. If you don't, feel free to add a blog comment with a better suggestion as there should be a better way.


## Update

My feature request was implemented in *RAD Studio 11 Alexandria, Release 1*!  You can see the change documented in their [Release 1 wiki page](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/11_Alexandria_-_Release_1#Libraries_Improvements)

Here is the relevant snippet from that wiki:

````
To help sharing code among projects build with FMX and VCL, we added framework specific predefined symbols:
FRAMEWORK_VCL - this predefined variable is set to true if the project uses the VCL framework
FRAMEWORK_FMX - this predefined variable is set to true if the project uses the FireMonkey (FMX) framework
````
So for 11.1 and later, you can use their new `FRAMEWORK_VCL` and `FRAMEWORK_FMX` symbols.  Thank you Embarcadero!
