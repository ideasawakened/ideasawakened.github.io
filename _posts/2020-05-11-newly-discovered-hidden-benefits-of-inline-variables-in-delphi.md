---
layout: post
title: "Newly discovered hidden benefits of inline variables in Delphi"
date: 2020-05-08 12:00:00 +0000
last_modified_at: 2021-03-18 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Stefan Glienke, David Millington]
permalink: post/newly-discovered-hidden-benefits-of-inline-variables-in-delphi
published: true
image: /assets/blog/Delphi-Programming/Inline-Variables_Type-Inference.png
description: Local inline variables with local scope and type inference in Delphi 10.3 RIO programming adds multiple benefits, some not as obvious at first glance.
---
![Delphi code demonstrating inline variables](/assets/blog/Delphi-Programming/Inline-Variables_Type-Inference.png)

To be honest, I have always regarded the [**new inline variables in Delphi 10.3**](https://blog.marcocantu.com/blog/2018-october-inline-variables-delphi.html) feature as something that I would never use, except perhaps variable initialization as that is obviously a very cool thing which makes your code much more readable as in the example below, _but I obviously won't like changing!_

```pascal
procedure Test1; // multiple inline declarations (symbols declared when used)
begin
  var I: Integer := 22;
  var J: Integer;
  J := 22 + I;
  var K: Integer := I + J;
  ShowMessage (K.ToString);
end;
````

One of the core requirements of Pascal is the strict typing of your variables required at the top of all your methods and to set that aside simply for variable initialization gives me great pause. However, there's a related improvement in 10.3 that I had ignored: **type inference of inline variables** which includes a very nice hidden benefit. (It was hidden to me until today when it was revealed by **Stefan Glienke** in a [**forum post**](https://en.delphipraxis.net/topic/2508-language-updates-in-104/?page=2&tab=comments#comment-22297).)

Type inference is kinda cool, as you don't have to tell the compiler that your loop variable is an integer or another obvious type. And once you pair that feature with the limited scope of inline variables to the particular block of code in which they are declared, then you get the additional benefit of a compiler error (instead of a warning) if you reference your loop variable outside the block. So, I will eventually use inline variables for loops..._but I probably won't like changing!_

````pascal
for var Item in Collection do
begin
  //Item is usable here
end;
//References to the Item variable generates compiler error here
````

But what Stefan mentioned today blew my mind. He gave an additional reason to like inline variables: "**often enough you have to add units to a uses clause just to declare a variable of a type that is the return of a method of some other type being used - with type inference that becomes unnecessary.**" At first, I didn't quite understand what he just wrote. I then re-read it and understood what he wrote but told myself that he was obviously wrong. I then said, well, this is Stefan so I should probably give him the benefit of the doubt in this area, so I immediately loaded up Delphi 10.3.3. I then typed in some test code, being fully confident within myself that it would absolutely fail to compile. To my disbelief, I hit F9 and it ran without errors or warnings. I now sat there for a moment thinking about the consequences and then said **I will use this feature and I will like it!**

Now if you are pulling off your best Winnie the Pooh impression, tapping your head like I was, saying to yourself "_Think, Think, Think..._" let's go over a simple example.

Start a new VCL project and add a TMemo and a TButton to the form as you've undoubtedly have done countless of times. Now double-click the button to add an OnClick event with the intent to fill the memo with a list of files from a particular directory (just for simple example code.)

Then add **System.IOUtils** to the Uses clause in the implementation section of the unit to give your code access to the **TDirectory** class. Then type some code looking similar to this in the new Click event:

````pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  FileNameList:TStringDynArray;
  i:Integer;
begin
  FileNameList := TDirectory.GetFiles('C:\', '*.*');
  for i := Low(FileNameList) to High(FileNameList) do
  begin
    Memo1.Lines.Add(FileNameList[i]);
  end;
end;
````

Others may choose to code it like this:

````pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  FileNameList:TStringDynArray;
  FileName:string;
begin
  FileNameList := TDirectory.GetFiles('C:\', '*.*');
  for FileName in FileNameList do
  begin
    Memo1.Lines.Add(FileName);
  end;
end;
````

In any case, once you hit F9 to run you will get an error similar to:

> \[dcc32 Error\] Unit1.pas(30): E2003 Undeclared identifier: 'TStringDynArray'

This is definitely a common annoyance. To fix, you go back to the Uses clause and add **System.Types**. Now when you hit F9, your code should run just fine.

Now, what if you leveraged the benefits of [**Delphi 10.3 RIO**](https://www.embarcadero.com/products/rad-studio/whats-new-in-10-3-rio)? Continue with the same example VCL project and simply re-code your Click event to use inline variables, similar to this:

````pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  var FileNameList := TDirectory.GetFiles('C:\', '*.*');
  for var FileName in FileNameList do
  begin
    Memo1.Lines.Add(FileName);
  end;
end;
````

Now hit F9 and it will run as expected (_yes, it looks a little odd with the inline variables!_) But the somewhat hidden benefit is that you **no longer need System.Types in your uses clause**. Try it - remove System.Types and you'll find that it compiles and runs just fine when using the type inference feature of inline variables. I honestly didn't think it would work.

In my head, below is what I was actually typing and this does indeed fail to compile unless System.Types is in the Uses clause. (To me, it is a little counter-intuitive to leave out the type declaration of inline variables.)

````pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  var FileNameList:TStringDynArray := TDirectory.GetFiles('C:\', '*.*');
  for var FileName:string in FileNameList do
  begin
    Memo1.Lines.Add(FileName);
  end;
end;
````

Now ask yourself this simple question - how many times have you hit F9 and had to go back and find which unit to add just for one new variable that you just added to your code? I know it's happened more than a few times to me.

Now, *even more impactful* is that I have often reorganized code to lump types into a single unit mainly to reduce this very specific pain point. (Have you?) Therefore, inline variables not only reduce the immediate common annoyance as demonstrated above, they also help to reduce the desire to have larger units, which is likely a much more important incentive. Suffice to say, after giving it a little more thought, I've gone 180 degrees on inline variables usage in a one day based on a single comment that gave me pause. (_Thanks Stefan!_)

Of course, there's a definite problem with inline variables in Delphi 10.3.3 as unfortunately the IDE tooling doesn't properly support it. Code Insight and Error Insight both fail to properly recognize inline variables. But, no worries, [**Delphi 10.4 Sydney**](https://github.com/ideasawakened/DelphiKB/wiki/D27.SYDNEY.10.4.0.0) is right around the corner and it introduces an **Error Insight Which Shall Not Suck**™

![Error Insight - No Longer Sucks Gandalf](/assets/blog/Delphi-Programming/Gandalf-Error-Insight-No-Longer-Sucks.jpg)

One of the very first things I have done with every new install of Delphi for a number of years is to disable the Error Insight option. It was simply wrong so often that it was more of a distraction than a benefit. With the upcoming support of the [**Language Server Protocol**](https://microsoft.github.io/language-server-protocol/), allowing the back-end tools to notify the front-end code editor which code has errors and which doesn't, Error Insight will produce the exact same errors as the compiler does - simply meaning that it will work as expected 100% of the time....thus, it **Shall Not Suck**.

For more information, see *David Millington*'s recent [**blog post on the revamped Code Insight in Delphi 10.4**](https://community.idera.com/developer-tools/b/blog/posts/new-in-delphi-10-4-redesigned-code-insight). And, as they are saying, it's time to **Get Excited about Delphi 10.4**! Leveraging inline variables with an improved Code Insight, a much improved Error Insight, and some new internal insight to the benefits of inferred typing.

![RAD Studio 10.4 is coming soon](/assets/blog/Delphi-Programming/10.4-Coming-Soon-Get-Excited.png)

## UPDATE 1
May 15, 2020
For further reading on inline variables in **Delphi 10.3 RIO** check out these links:
- [**blog.grijjy.com: Inline Variables can increase performance**](https://blog.grijjy.com/2018/11/02/inline-variables-can-increase-performance/)
- [**langraf.dev: 5 Reasons to Use Inline Variables in Delphi**](https://landgraf.dev/en/5-reasons-to-use-inline-variables-in-delphi/)


## UPDATE 2
March 18, 2021

Unfortunately, there are too many issues with inline variables for me to use.  Some are listed here in my [style guide reference](https://github.com/radprogrammer/radteam/wiki/RADProgrammer-Style-Guide-Other-Guidance)
When (or IF) the tooling catches up, perhaps inline variables will be usable.

