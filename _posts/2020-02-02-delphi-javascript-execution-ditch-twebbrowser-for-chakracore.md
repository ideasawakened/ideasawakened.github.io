---
layout: post
title: "Delphi JavaScript execution - ditch TWebBrowser for ChakraCore"
date: 2020-02-02 12:00:00 +0000
last_modified_at: 2020-02-02 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [DelphiKB, ChakraCore, Javascript, TWebBrowser, Ondrej Kelle, Salvador Díaz Fau, Grijjy]
permalink: post/delphi-javascript-execution-ditch-twebbrowser-for-chakracor
published: true
image: /assets/blog/javascript/Chakra-Core/chakra-core-example-delphi-square.png
description: Using Microsoft ChakraCore binaries in Delphi projects
---
I assume a large portion of the Delphi developer community has leveraged TWebBrowser to some degree over the years - I know I have used it many times. So, when I started a new project and wanted to run a little JavaScript, I immediately thought of the venerable **TWebBrowser**. It's actually fairly simple to do, but you need to be careful about the async nature of the tool. Most examples on the web involve looping waiting for the ReadyState to change while calling Application.ProcessMessages inside the loop. Long story short: this is typically not a good approach as you are attempting to force an async tool to act in a synchronous manner.

To execute JavaScript with TWebBrowser you first need to get a document loaded, and the easiest way seems to be to navigate to "about:blank" After the .ReadyState is >= READYSTATE\_INTERACTIVE then you can continue with a simple example below:

````pascal
procedure TMyExample.ExecJS(const pJavaScript:string);
var
  Doc2:IHTMLDocument2;
  HTMLWindow:IHTMLWindow2;
begin
  Doc2 := fWB.Document as IHTMLDocument2;
  if Assigned(Doc2) then
  begin
    HTMLWindow := Doc2.parentWindow;
    HTMLWindow.execScript(pJavaScript, 'JavaScript');
  end
  else
  begin
    raise Exception.Create('Invalid browser state, cannot currently execute JavaScript');
  end;
end;
````

You may want to first have a web page loaded with some helper scripts populated in the <head> section. In that case, load your html doc first with something like:

````pascal
procedure TMyExample.LoadDoc(const pDocText:string);
var
  vDocStream:TStringStream;
begin
  vDocStream := TStringStream.Create(pDocText, TEncoding.UTF8);
  try
    vDocStream.Seek(0, TSeekOrigin.soBeginning);
    (fWB.Document as IPersistStreamInit).Load(TStreamAdapter.Create(vDocStream));
  finally
    vDocStream.Free();
  end;
  CurrentState := ..
end;
````

Once you have your helper scripts loaded, you can simply call them repeatedly with the .ExecJS method. This sort of simple operation seems to work rather well, but it's fairly slow when you start passing in lots of data back and forth.

One option is to create a custom ActiveX control and then load that into your document page and have Delphi and the ActiveX control communicate back and forth. That speeds things up, but the tooling starts getting more complicated. I'll point that Delphi probably has the best ActiveX support available but it's something rarely bragged about these days. ActiveX is very powerful with Delphi - if you have any interest, pick up the book "[**Delphi Programming with COM and ActiveX**](https://amzn.to/37RX2p8)" which is only $11.99 (side note - why is the same book on Amazon also listed as 'New' for [**$1,012.90**](https://amzn.to/2ucwTmg)?) There's another book "[**Essential Delphi 3 fast: Includes ActiveX Development**](https://amzn.to/3aZ5m8x)" that can be picked up used for as low as $1.99 - or as New from $159 if you have money to burn.

Ok, let's leave the past for now and look towards the future and that is high-speed JavaScript engines such as [**V8**](https://chromium.googlesource.com/v8/v8.git) (Google), [**SpiderMonkey**](https://hg.mozilla.org/) (Mozilla) and [**ChakraCore**](https://github.com/microsoft/ChakraCore) (Microsoft). It seems that Google has won the browser battle and the current future of Chakra is in doubt. But it's currently still actively being developed and so far, I haven't seen official announcements of its scheduled demise. For what its worth, it is apparently going to be continued "for non-Edge use in Windows." As far Microsoft Edge, it is [**officially out of preview**](https://blogs.windows.com/msedgedev/2020/01/15/upgrading-new-microsoft-edge-79-chromium/) and available for download as of a week ago with its rebirth using Chromium.

There are a few options out there for Delphi and Javascript interop: there is a V8 wrapper for Delphi on [**GitHub**](https://github.com/zolagiggszhou/v8delphiwrapper), but it hasn't been updated for a few years. There's an old [**Delphi-Javascript**](https://code.google.com/archive/p/delphi-javascript/) engine based on SpiderMonkey. There's even a full ECMAScript 5th Edition implementation in Object Pascal on GitHub called [**besen**](https://github.com/bero1985/besen). I will definitely dig into this more later as my first quick attempts weren't very successful. Instead, I went with a Delphi/FPC binding of the Microsoft ChakraCore library found on GitHub by **Ondrej Kelle**: [**chakracore-delphi**](https://github.com/tondrej/chakracore-delphi). He has a few articles on his [**blog**](https://fast-forward-tools.net/) to get you started, and also some sample projects in the repo. There are a few other options, such as the [**JSEngine**](https://www.winsoft.sk/jsengine.htm) product from WinSoft but I was in contact with them and it didn't work for my use. Last but not least, there's the venerable [**mORMot**](https://github.com/synopse/mORMot) framework with some SpiderMonkey bindings. I'm sure there's a solution there if you invest the time into the framework, which I still haven't done.

One other project which deserves mention is the [**CEF4Delphi**](https://github.com/salvadordf/CEF4Delphi) project being maintained by **Salvador Díaz Fau** who is doing an amazing job. If you are using TWebBrowser on Windows for browsing, you really should consider replacing it with CEF4Delphi. For this project, I wanted a single dll dependency for the ChakraCore library. I'm using CEF4Delphi in another project and it works great.

![What is ChakraCore?](/assets/blog/javascript/Chakra-Core/chakra-core.png)

The bare minimum on ChakraCore that you need to know is that it requires a single active runtime environment per thread. There can be multiple execution contexts active per thread, but they are each tied to a single runtime environment. You typically have one runtime and one context, but the tooling is there to expand as needed (if you require concurrent multi-threaded code execution for example.) If you are using this library only in the main thread of your app, you'll use a single runtime.

Ondrej has made this dead simple with a TChakraCoreRuntime and TChakraCoreContext classes. You first create a runtime, passing in any [**runtime attributes**](https://github.com/microsoft/ChakraCore/wiki/JsRuntimeAttributes) that you might need activated. You then create a context, passing in the associated runtime, for example:

````pascal
FRuntime := TChakraCoreRuntime.Create([ccroDisableEval]);
FContext := TChakraCoreContext.Create(FRuntime);
````

You can then execute script as needed, returning a JsValueRef result:

````pascal
 Result := FContext.RunScript(ScriptSource, ScriptName);
````

There are a slew of utility methods to work with JsValueRef objects such as **JsNumberToInt(Value:JsValueRef):Integer;**

I actually had a hard time getting started as it was really just too simple. Thankfully, Ondrej was kind enough to nudge me in the right direction! He has sample projects to get you started, including how to work with [**node.js modules**](https://tondrej.blogspot.com/2019/01/node-modules-with-delphi-and-free-pascal.html). It's an amazingly powerful tool.

And speed is much greater with this tool than the old TWebBrowser .ExecJS style approach. For small scripts with little data, you may not notice much difference. But, once your scripts get larger and your data gets more complex, you will see a massive improvement in speed by replacing TWebBrowser with ChakraCore. Clone the repo and you can be up and running in a few minutes - once you download the required [**binaries**](https://github.com/microsoft/ChakraCore/wiki/Getting-ChakraCore-binaries) from Microsoft of course. Drop the ChakraCore.dll file into your project's bin folder and you're good to go, and it's just a little under 5MB in size. I've included the Win32 version of the .dll in a [**simple example project on GitHub**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Demos/chakracore-delphi/SimpleExec).

![Example screenshot of using ChakraCore with Delphi](/assets/blog/javascript/Chakra-Core/chakra-core-example-delphi.png)

A big **_Thank-You_** out to Ondrej Kelle for the work on the chakracore-delphi project, for the quick online help, and for the connection request on [**LinkedIn**](https://www.linkedin.com/in/darianm). The Delphi community has been hit hard over the years, and there's been a seemingly continuous talk of its imminent death for the last two decades, but it seems to still be vibrant and strong today thanks to people like Ondrej!

### Alternative Route

Of course, as I was typing this blog post, I came across an article from the Grijjy folks about their solution: [**Duktape for Delphi**](https://blog.grijjy.com/2018/02/28/javascripting-with-duktape-for-delphi/). This minimizes the dependency down to a single half-megabyte DLL which apparently supports the complete ECMAScript 5.1 spec as well as parts of 2015/2016 versions.) There is simply an amazing amount of Delphi code out there once you start looking! I'll have to spend some time on this library soon...**so many Delphi toys, so little time!**