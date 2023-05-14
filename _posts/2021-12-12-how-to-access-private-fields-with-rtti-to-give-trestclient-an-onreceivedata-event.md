---
layout: post
title: "How to access private fields with RTTI to give TRESTClient an OnReceiveData event"
date: 2021-12-12 12:00:00 +0000
last_modified_at: 2021-12-12 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [RTTI, Robert Love]
permalink: post/how-to-access-private-fields-with-rtti-to-give-trestclient-an-onreceivedata-event
published: true
image: /assets/blog/Delphi-Programming/TRESTClient-OnProgress/RTTI-square.png
description: "Example on breaking encapsulation to access private fields in a class with RTTI in Delphi by adding an OnProgress event to TRESTClient."
---
I was speaking with a colleague the other day who was working on a project which relied on the popular Delphi component trio: [**TRESTClient**](https://docwiki.embarcadero.com/Libraries/en/REST.Client.TRESTClient), [**TRESTRequest**](https://docwiki.embarcadero.com/Libraries/en/REST.Client.TRESTRequest) and [**TRESTResponse**](https://docwiki.embarcadero.com/Libraries/en/REST.Client.TRESTResponse). He wanted to add a progress event but there is currently no obvious way of providing that sort of feedback with the components. Internally, I immediately discounted that statement as these are obviously communication components and communication components **always** have some sort of 'OnProgress' type event...right?

After inspecting the components myself, it seems that I have to eat my words and accept the fact that there is, in my humble opinion, a design flaw in these components. Not only do they not expose an OnProgress type event, the components are built in a way to shield shield you from accessing the internal HTTPClient transport - so you cannot add support for the missing event yourself.

You typically create a TRESTClient, assign it a TRESTRequest and assign the TRESTResponse to the request. You can accomplish this completely code free in the IDE by dropping a few components on the form. By combining [**Live Bindings**](https://docwiki.embarcadero.com/RADStudio/Sydney/en/LiveBindings_in_RAD_Studio) in RAD Studio with their free [**REST Debugger Tool**](https://www.embarcadero.com/free-tools/rest-debugger) you can become a **no-code** or **low code guru** with Delphi and easily implement the many different APIs that are available today.

That sounds great, and it does work fairly well in most scenarios with small file transfers. But if your download takes more than a few seconds then you might want to show a progress bar and that is where these components fail - and where we will take the opportunity to introduce a workaround.

![Break encapsulation with RTTI in Delphi](/assets/blog/Delphi-Programming/TRESTClient-OnProgress/Break-Encapsulation-with-RTTI.png)

## Access Private Fields with RTTI

For many years, there were a few tricks to easily access private fields in another class in Delphi. It was [**decided**](https://blog.marcocantu.com/blog/2016-june-closing-class-helpers-loophole.html) for the 10.1 Berlin release back in 2016 to close out this access and fix the implementation bug. Thankfully, by using [**RTTI**](https://docwiki.embarcadero.com/RADStudio/en/Working_with_RTTI) you can still access the internals of other classes as needed. (As long as the class has been compiled with [**extended RTTI**](https://docwiki.embarcadero.com/RADStudio/en/RTTI_directive_(Delphi))...which is not available in all RTL classes,)

If you look at the [**available events for TRESTClient**](https://docwiki.embarcadero.com/Libraries/en/REST.Client.TRESTClient_Events) you will only see a few events listed: OnAuthEvent, OnHTTPProtocolError, OnNeedClientCertificate, and OnValidateCertificate. The first reaction is to look for an exposed underlying transport channel via a public property and unfortunately, but there is nothing available. Since we do not have an event to use, nor do we have a way to access the underlying transport, we will have to crack into the class.

If you examine the code for TCustomRESTClient in the REST.Client.pas unit, you will find a private field named **FHttpClient** with the type of [**TRESTHTTP**](https://docwiki.embarcadero.com/Libraries/en/REST.HttpClient.TRESTHTTP). Unfortunately, there are even [**fewer events**](https://docwiki.embarcadero.com/Libraries/en/REST.HttpClient.TRESTHTTP_Events) made public for the TRESTHTTP class: OnAuthEvent, OnNeedClientCertificate, OnValidateCertificate and once again, no public [**properties**](https://docwiki.embarcadero.com/Libraries/en/REST.HttpClient.TRESTHTTP_Properties) available for accessing the underlying transport.. So we need to dig deeper into the code of TRESTHTTP in the REST.HttpClient.pas unit. Here, we will find another private field named **FHTTPClient** with the type of [**System.Net.HTTPClient.THTTPClient**](https://docwiki.embarcadero.com/Libraries/en/System.Net.HttpClient.THTTPClient).

By examining THTTPClient, we now see in the [**event list**](https://docwiki.embarcadero.com/Libraries/en/System.Net.HttpClient.THTTPClient_Events) the likely progress event that we are after: **OnReceiveData**. Unfortunately, there are no public [**properties**](https://docwiki.embarcadero.com/Libraries/en/System.Net.HttpClient.THTTPClient_Properties) available to access the underlying transport mechanism to set this event, so we will once again have to crack into this underlying class.

What we learned is that we need to break into the REST.Client.TCustomRESTClient class and access the FHttpClient private field of type REST.HttpClient.TRESTHTTP. We then need to break into this TRESTHTTP class and access the FHTTPClient private field of type System.Net.HTTPClient.THTTPClient so we can set the OnReceiveData event and hope it works. Simple, right? It is with RTTI.

The first thing you will do is add **System.RTTI** to your Uses clause. You can then create a special **TRTTIContext** record to access all the public types in your application. You can use the RTTI reflection methods provided to create instances of a type, invoke its methods, or access its fields. The process is pretty simple - you first call [**GetType**](https://docwiki.embarcadero.com/Libraries/en/System.Rtti.TRttiContext.GetType) and pass in the desired class type and then use that returned instance to dig deeper with calls to [**GetField**](https://docwiki.embarcadero.com/Libraries/en/System.Rtti.TRttiType.GetField) with the desired field name paired with [**GetValue**](https://docwiki.embarcadero.com/Libraries/en/System.Rtti.TRttiField.GetValue) on the field reference passing in the particular object instance you are working with to get the current value of the field as a [**TValue**](https://docwiki.embarcadero.com/Libraries/en/System.Rtti.TValue) which is another special record that acts like a Variant that can store different data types.

That was a long sentence...here's a more concrete code example that should hopefully make it clearer. Start a new Application in Delphi and drop a **TRESTClient** component onto a form (which will automatically be named as RESTClient1) along with a **TButton**. Add **System.RTTI** to the uses clause and in the **OnClick** event of the button, write code similar to what is shown below:

````pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  MyRTTIContext:TRTTIContext;
  ExtractedRestHTTP:TValue;
begin

  MyRTTIContext := TRTTIContext.Create;
  try
    ExtractedRestHTTP := MyRTTIContext.GetType(TRESTClient).GetField('FHttpClient').GetValue(RESTClient1);
  finally
    MyRTTIContext.Free;
  end;

end;
````

What this does is to grant us access to the private FHttpClient field from the RESTClient1 instance of the TRESTClient class. Within a few seconds of work, you have successfully cracked the class and accessed a private field!

Of course, in this particular example case we aren't done as we have to go two classes deep to get to the desired OnReceiveData event. So let us expand on this simple code a make it more usable by dropping some more components on the form: TRESTRequest, TRESTResponse,TMemo and a TProgressbar.

We started this process by wanting a Progress event so we also need to create a new procedure to show the download progress. We will crack into the underlying THTTPClient instance and assign this custom procedure to be used as the OnReceiveData event.

The entire unit is shown below. Once you click on the button, the request will be made and the JSON result will be appended to the memo. In this particular example we will be looking up the current Bitcoin price using [**Cryptonator's API**](https://www.cryptonator.com/api).

````pascal
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, Vcl.StdCtrls,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    Memo1: TMemo;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure MyOnProgressEvent(const Sender:TObject; AContentLength:Int64; AReadCount:Int64; var AAbort:Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  System.Rtti,
  REST.HttpClient,
  System.Net.HttpClient;

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  MyRTTIContext:TRTTIContext;
  RestHTTPClientVal:TValue;
  HTTPClientVal:TValue;
  theClient:System.Net.HttpClient.THTTPClient;
begin

  //hook up our OnProgress event by access private fields via RTTI
  MyRTTIContext := TRTTIContext.Create;
  try
    RestHTTPClientVal := MyRTTIContext.GetType(TRESTClient).GetField('FHttpClient').GetValue(RESTClient1);
    HTTPClientVal := MyRTTIContext.GetType(TRESTHTTP).GetField('FHTTPClient').GetValue(RestHTTPClientVal.AsObject);

    theClient := HTTPClientVal.AsType<THTTPClient>;
    theClient.OnReceiveData := MyOnProgressEvent;

  finally
    MyRTTIContext.Free;
  end;

end;


procedure TForm1.Button1Click(Sender:TObject);
begin
  RESTClient1.BaseURL := 'https://api.cryptonator.com/api/ticker';
  RESTRequest1.Resource := 'btc-usd';
  ProgressBar1.Position := 0;

  RESTRequest1.Execute;

  Memo1.Lines.Add('Current Price:');
  Memo1.Lines.Add(RESTResponse1.Content);
end;



procedure TForm1.MyOnProgressEvent(const Sender:TObject; AContentLength:Int64; AReadCount:Int64; var AAbort:Boolean);
begin
  if AContentLength > 0 then
  begin
    ProgressBar1.Position := (AReadCount * 100) div AContentLength;
  end;
end;

end.
````
When you run this sample code you should see something similar to the following screen shot. In this example, two OnProgress events were quickly fired and the current price details of Bitcoin was returned in JSON format. This particular transaction was so fast that a progress bar is not very useful but if your received data size is much larger, then perhaps you might want to add an OnProgress event of your own.

![Example screen shot with progressbar for TRESTClient](/assets/blog/Delphi-Programming/TRESTClient-OnProgress/RTTI-OnProgress-TRestClient-Example.png)

The astute readers will note that TRTTIContext is a record so why are calling Create and Free? You should review [**Robert Love's great answer**](http://robstechcorner.blogspot.com/2009/09/trtticontextcreate-trtticontextfree.html) to that particular question.

## Summary

As demonstrated, you can use RTTI to obtain access to private fields in another class which allows you to break the normal encapsulation rules. We all know that if you break rules in life then you can certainly be expected to pay a price for your actions at some point in the future. If you are accessing the internals of a class, you should not be surprised when you have to deal with breaking code changes in the future as implementation details can change without notice as long as the public interface does not change