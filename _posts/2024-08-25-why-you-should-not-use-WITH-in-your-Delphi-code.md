---
layout: post
title: "Why you should not use WITH in your Delphi code (but there is this one cool hack...)"
date: 2024-08-25 20:00:00 +0000
last_modified_at: 2024-08-26 14:00:00 +0000
categories: [Delphi Programming, Best Practices]
tags: [Ali Dehbansiahkarbon, Alister Christie, Andreas Rejbrand, Chris Miller, Craig Stuntz, Dalija Prasnikar, David Corneliums, David Dombrowsky, David Heffernan, Fabricio Araujo, Glenn Dufke, Hallvard Vassbotn, Malcome Groves, Marc Durdin, Marco Cantu, Martin James, Mason Wheeler, Nick Hodges, Olaf Monien, Robert Gilland, Stefan Glienke, Thomas Mueller, Toon Krijthe]
permalink: post/why-you-should-not-use-WITH-in-your-Delphi-code
published: true
image: /assets/blog/Delphi-Programming/Stop-Using-With-Statements-In-Delphi-Square.jpg
description: 'Avoid using WITH statements in Delphi for clearer, more maintainable, and less error-prone code. But you can use WITH along with Record and Class Helpers to access private methods.'
---

# Delphi Programming Best Practices - Avoid WITH Statements

There have been many discussions over the years about using **WITH** statements in your Pascal code. We just had another short with-related discussion on the Delphi Programmers Telegram server the other day and I decided to write a blog post so I could simply include a link to this post and hopefully rarely discuss it in detail again. 

Some people simply never want to give up the clean looking blocks of code that **with** seems to provide.  I will provide example code explaining the hidden dangers of continuing with this bad practice. 

## WITH Statement Documentation

First, here is some documentation taken from the official [DocWiki Page](https://docwiki.embarcadero.com/RADStudio/en/Declarations_and_Statements_(Delphi)#With_Statements) from Embarcadero.

A **with** statement is a shorthand for referencing the fields of a record or the fields, properties, and methods of an object. You can reference one or more variables in your **with** statement such as
````pascal
with obj1, ..., objN do statement
````
where *obj* is an expression yielding a reference to a record, object instance, class instance, interface or class type (metaclass) instance, and *statement* is any simple or structured statement. Within the *statement*, you can refer to fields, properties, and methods of obj using their identifiers alone, that is, without qualifiers.

Each variable reference or method name in a **with** statement is interpreted, if possible, as a member of the specified object or record. If there is another variable or method of the same name that you want to access from the **with** statement, you need to prepend it with a qualifier.


## Example Usage
The **with** statement allows you to access child elements without having to specify the element's name each time.

For example, we will define two simple records for tracking Company and Contact info:
````pascal
  TCompany = Record
    CompanyID:Integer;
    CompanyName:String;
  End;

  TContact = Record
    ContactID:Integer;
    FirstName:String;
    LastName:String;
    Twitter:String;
    LinkedIn:String;
  end;
````
You can use a **with** statement to streamline references to child fields, such as in the following code:

````pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  MyContact:TContact;
  MyCompany:TCompany;
begin
  MyContact := Default(TContact);
  MyCompany := Default(TCompany);

  with MyContact, MyCompany do
  begin
    ContactID := 1234;
    FirstName := 'Darian';
    LastName := 'Miller';
    Twitter := 'https://www.twitter.com/ideasawakened';
    LinkedIn := 'https://www.linkedin.com/in/darianm/';

    CompanyID := 5678;
    CompanyName := 'Ideas Awakened, Inc';
  end;
  
  ShowMessage(MyContact.Twitter);  //will display: https://www.twitter.com/ideasawakened
end;
````

Now the code above is currently **functionality equivalent** to the code below, which is one big reason why developers like to use the **with** statement as the code below does look a little more involved:

````pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  MyContact:TContact;
  MyCompany:TCompany;
begin
  MyContact := Default(TContact);
  MyCompany := Default(TCompany);

  MyContact.ContactID := 1234;
  MyContact.FirstName := 'Darian';
  MyContact.LastName := 'Miller';
  MyContact.Twitter := 'https://www.twitter.com/ideasawakened';
  MyContact.LinkedIn := 'https://www.linkedin.com/in/darianm/';

  MyCompany.CompanyID := 5678;
  MyCompany.CompanyName := 'Ideas Awakened, Inc';

  ShowMessage(MyContact.Twitter);  //will display: https://www.twitter.com/ideasawakened
end;

````

## Example Problem
Say the code above has been in production for over a few days, weeks, or even a few decades...and then someone requests that you start tracking the Twitter link for TCompany records.  You can easily make a one-line change to the record definition like this:
````pascal
  TCompany = Record
    CompanyID:Integer;
    CompanyName:String;
    Twitter:String;  //added for ticket IA-240821
  End;
````
After that one-line change, build the code above and you will find zero warnings/errors so you can simply rerun the project...what will be displayed by the ShowMessage statement?  In this case, it should be relatively easy to figure out that there will now be a blank message as the TCompany reference was last in scope within the **with** block and the compiler now sees a Twitter field in your TCompany record and it will use that first.  

This may seem like an overly contrived example (which it is), but you should consider a larger program with many thousands (or millions) of lines of code with hundreds (or thousands) of Records and Objects defined.  The compiler did not warn you that it will now treat that **with** reference differently - so you better have the tests in place to discover the problem before your customers do!  This is a clear example of the **maintenance challenges** that **with** statements introduce into your code.

## Actual Example Of WITH Fragility

A great example from **Marc Durdin**'s [Blog: Delphi's ongoing problem with "with"](https://marc.durdin.net/2011/11/delphis-ongoing-problem-with-with-2/) Even though Marc still seems to favor using **with**, he gives a perfect example of how it can come back later to generate havoc in your code.  In this case, it was upgrading the code to a newer version of Delphi which caused **with** statements to silently change previous behavior.

His code in a million-line+ project included the following snippet:
````pascal
with GetTranspBorderSize(BorderStyle) do
  R := Rect(Left, Top, Width-Right, Height-Bottom);
````
The routine GetTranspBorderSize returned a TRect and the code referenced the Top+Bottom+Left+Right properties of the TRect result but also the Width+Height properties of the current component. A newer version of Delphi introduced new Width+Height functions to TRect so his code now failed to work as expected because the compiler picked the Width+Height of the TRect result instead of the Width+Height of the component (as last-defined in scope wins.) I wonder how many similar bugs were created across the world after developers upgraded their Delphi version because they used **with**.

**Code is nearly guaranteed to change over time** and this exact problem can easily arise from future changes in the RTL, third party libraries, or even in your own code.  Using **with** is like introducing a time-bomb set to blow up randomly at some point in the future.  This is the number one reason why you should stop using **with** statements.  No one wants Embarcadero to ever stop improving  existing RTL code.  Your third party library vendors will not stop changing their code either.  Hopefully, your organization will not stop adding/improving your existing codebase.  All those code changes over time combine into one big pot of potential scoping conflicts and future bugs.  Start using namespace qualifiers where you can and get more precise in the code that you maintain.  You simply cannot hope to become more precise if you continue to use **with** statements.

## Nested WITHs Are Evil

Do we need cover the problems with nested **with** statements?  Simply assume all problems of **with** increase exponentially based on the level of nesting.

````pascal
with A, B, C do
begin
  Cost := 10; 
end;
````

## With Related Bugs
There are also compiler issues surrounding the **with** statement, including [RSP-37186: With statement doesn't give priority to local variables in its code block](https://quality.embarcadero.com/browse/RSP-37186) as seen below:

````pascal
{$Apptype console}
var
  test : record
    message : string;
  end;
 
begin
  test.message := 'Hello world!';
 
  with test do
   begin
     // This will output: Hello world!
     writeln (message);    
     var message := 'Goodbye!';
     // This will output:  Hello world! (but last-defined in scope wins, right?)
     writeln (message); //add a breakpoint here
     //Check the current value (Goodbye!) The Compiler does not match the Debugger
   end;
end.
````

A recent related RTL issue in RAD Studio 11 Alexandria: [RSP-38694: Using "with" in VCL causes "use after free" bugs](https://quality.embarcadero.com/browse/RSP-38694)  This quickly received 43 votes to fix as the following simple code failed after some RTL changes:

````pascal
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(64, 64);
    Buffer.Canvas.StretchDraw(Rect(0, 0, 64, 64), Bitmap); // fails here
    Bitmap.SetSize(64, 64);
    Bitmap.Canvas.Draw(0, 0, Buffer); 
  finally
    Buffer.Free;
  end;
````
That is because StretchDraw calls TBitmap.Draw, which was implemented like so:
````pascal
procedure TBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
  // ...
begin
  with Rect, FImage do // here: FImage is cached into CPU register
  // ...
      Canvas.RequiredState(csAllValid); // here: FImage is deleted, but CPU register still points to the deleted object
  // ...
        StretchBlt(ACanvas.FHandle, Left, Top, Right - Left, Bottom - Top,
          Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth, // here: FImage.FDIB references already deleted object
          FDIB.dsbm.bmHeight, ACanvas.CopyMode);
  // ...
end;
````
Of course, **with** is just as evil inside the RTL as outside of it!  EurekaLog also covered this with-related bug in their blog post [EurekaLog erases my bitmap? (even VCL has bugs)](https://blog.eurekalog.com/2022/07/eurekalog-erases-my-bitmap.html)  This was also covered in [StackOverflow: Why should I not use "with" in Delphi](https://stackoverflow.com/a/76976183/35696)


## WITH Problems Are Not Idential To Problems With USES Clauses
Some developers equate the **with** problem as the exact same scoping problems with **uses** statements (as a lame excuse to keep using **with**.)  If you re-order the items within your uses clause, you can certainly open yourself up to bugs (as last-defined in scope wins.) Any changes to public members in used units can also open yourself to similar scoping-related bugs.  But, you can protect yourself against these problems by using namespace identifiers when accesing code in different units. (`SomeUnit.SomeMethod`)  Using **with** statements is the _intentional exclusion_ of using more precise scoping within the with-block of code.  It also greatly obscures the "last-defined in scope wins" assumption when you are reading and understanding the code in the unit.  All too often you can actually see the item you assume you are referencing in that block of code within the same unit, but the implementation the compiler sees is different based on the altered scope resolution.  Your eyes can easily lie to you when reading **with** blocks of code.

A simple example of uses-clause scope issues involves the call to `DeleteFile(FileName)`  How many times have you used that routine and received a compiler error similar to: `[dcc32 Error] Unit1.pas(47): E2010 Incompatible types: 'PWideChar' and 'string'`?  For form units, the autocreated code by Delphi lists `Winapi.Windows` before `System.SysUtils` so DeleteFile typically works as expected (and calls the SysUtils version.)  But sometimes when you are creating a non-visual unit and your call to Delete throws that compiler error because SysUtils can sometimes be listed before Windows.  The obvious solution is to be more precise in your coding and use `SysUtils.DeleteFile(FileName);` or even better, use the fully scoped method: `System.SysUtils.DeleteFile(FileName);` 

Scope issues related to **uses** clauses can be greatly minimized but the damage from **with** is self-inflicted and **unavoidable** making **with** issues much worse than **uses** clause issues.

## Use Variables To Replace With
In a [Delphi-PRAXiS thread: Inline Variables Coming in 10.3](https://en.delphipraxis.net/topic/138-inline-variables-coming-in-103/?do=findComment&comment=1454) it was suggested by **Kryvich** to use inline variables to replace **with** statements with the following example:
````pascal

//instead of this:
procedure TMyGreatEditor.Cut;
  with ControlRange as IHTMLControlRange do
  begin
    select;
    execCommand('Cut', False, EmptyParam)
  end;
end;

//use inline variables instead and your future WITH-related problems go away:
procedure TMyGreatEditor.Cut;
begin
  var cr := ControlRange as IHTMLControlRange;
  cr.select;
  cr.execCommand('Cut', False, EmptyParam)
end;

//I suggest that you use simple variables instead:
procedure TMyGreatEditor.Cut;
var
  cr:IHTMLControlRange
begin
  cr := ControlRange as IHTMLControlRange;
  cr.select;
  cr.execCommand('Cut', False, EmptyParam)
end;
````

**Marco Cantu** also suggested inline variables: [Blog: Delphi With Statements and Local Variables](https://blogs.embarcadero.com/delphi-with-statements-and-local-variables/)

**Note**: I wish I could support switching to inline variables, but the tooling still has not caught up.  There are just too many problems with the IDE and inline variables. (Issues with formatting, refactoring, debugging, codeinsight...)  It is definitely getting better, and I believe they are a great addition to the language, but I currently stay away from most inline variable usage. See my earlier post [Newly discovered hidden benefits of inline variables in Delphi](https://ideasawakened.com/post/newly-discovered-hidden-benefits-of-inline-variables-in-delphi)


## Careful Refactoring Required

You need to be careful when replacing your existing **with** statements, as you can see in the Quality Portal issue [RSP-34700: Resolving a with statement has introduced a bug](https://quality.embarcadero.com/browse/RSP-34700) Undoing the **with** entanglements properly can be very tricky and error prone.  This points to the underlying problems of using **with**:  you think that you are adding simplicity to your code when you are potentially making it much more complex over time.  

The original code looked like this:
````pascal
  with FToolBar do
  begin
    if not (csLoading in ComponentState) then
      HandleNeeded; // Changing visibility requires the toolbar to have a handle
    if Perform(TB_GETBUTTON, Index, Button) <> 0 then
      Perform(TB_HIDEBUTTON, Button.idCommand, MakeLong(Ord(not Self.Visible), 0));
    { Force a resize to occur }
    if AutoSize then
      AdjustSize;
  end;
````
And the newly refactored code looked like:
````pascal
  if not (csLoading in FToolBar.ComponentState) then
    FToolBar.HandleNeeded; // Changing visibility requires the toolbar to have a handle
  if FToolBar.Perform(TB_GETBUTTON, Index, Button) <> 0 then
    FToolBar.Perform(TB_HIDEBUTTON, Button.idCommand, MakeLong(Ord(not Visible), 0));
  if Visible then
  begin
    var R: TRect;
    // If the toolbutton has just been made visible, then ensure it didn't
    // miss a scale operation by checking intended size
    if FToolBar.Perform(TB_GETITEMRECT, Index, R) <> 0 then
      BoundsRect := R;
  end;
  // Force a resize to occur
  if AutoSize then
    AdjustSize;
````
Note that *AutoSize* and *AdjustSize* refers to **FToolbar** in the first case, but to **Self** in the second one. Even the RTL developers fall prey to common errors like this.   Welcome to a another in a long line of with-related bugs!  


## There Are Exceptions To Every Rule

I will admit - assigning simple constants to fields of a record can be pretty innocent - but only as long as you do not use nested with statements.  So this simple code *could* be an exception:
````pascal
with ExampleProduct do
begin
  ID := 133;
  SKU := 'AB230';
  Price := 50;
end;
````
**However**, good code typically lasts a very long time. As soon this code does anything but assign constants to fields, you step into the minefield of with-related problems.  It is best to simply avoid future headaches and stay away from **with**.  

General rule: if a reference can be explicitly scoped - *do so*.


## Hack Private Methods Using WITH, Even After Delphi 10.1 Berlin
There is really one reason to use **with** statements today and that is their ability to hack into private methods with the use of a Class or Record Helper.  Re-read that last sentence and it does not seem to fit into the rest of this article... we are supposed to be making code more re-usable, more readable, more maintainable and now we are talking about hacking into private methods.  Well, yes.  Sometimes you need to do what you need to do...but at least attempt fixing the code first before using this special hack.

Embarcadero limited the overpowered Class Helpers back in the 10.1 Berlin release (see [Marco Cantu's Blog Post: Closing the Class Helpers Private Access Loophole](https://blog.marcocantu.com/blog/2016-june-closing-class-helpers-loophole.html)) Class Helpers were commonly utilized to break encapsulation, as explained by Marco in his blog post:
>For quite some time, there was a bug in the Delphi compiler that ended up allowing class helper methods to access private fields of the class they helped, regardless of the unit in which the class was declared. This “hack” basically broke OOP encapsulation rules. To enforce visibility semantics, class and record helpers in most recent versions of Object Pascal compilers (starting with 10.1 Berlin) cannot access private members of the classes or records that they extend. Notice that protected members, on the other hand, are accessible to class helpers, exactly like they are to derived classes.

Embarcadero received quite a bit of blowback over that decision - as you can tell from the number of comments on his blog post, and others:
- [RSP-14711: Helper - E2361 Cannot access private symbol](https://quality.embarcadero.com/browse/RSP-14711)
- [RSP-14347: Can't access to private fields in helpers](https://quality.embarcadero.com/browse/RSP-14347)
- [RSP-15273: Add possibility to access to private properties and methods in natural way like it was for class helpers](https://quality.embarcadero.com/browse/RSP-15273)
- [StackOverflow: How to access private methods without helpers?](https://stackoverflow.com/questions/36716363/how-to-access-private-methods-without-helpers/36717896#36717896)
- [Use inline assembler to access private members](https://lyna-hateblo-jp.translate.goog/entry/20160420/1461081751?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=en&_x_tr_pto=wapp)

However, there remains a way to continue this special Class Helper access by leveraging the **with** statement.  NOTE:  There is no guarantee that Embarcadero will not close down this bug in an upcoming version of Delphi.  Another way is to use RTTI, as seen in this blog post: [How to access private fields with RTTI to give TRESTClient an OnReceiveData event](https://ideasawakened.com/post/how-to-access-private-fields-with-rtti-to-give-trestclient-an-onreceivedata-event)

Here is a very simple example of using **with** and a Class Helper... say you have a `TMyGreatClass` and it has a **private method** called `SomethingSpecial` that you want to call:  
````pascal
unit MyGreatClassUnit;

interface

type

  TMyGreatClass = class
  private
    procedure SomethingSpecial;
  end;

implementation
uses
  Dialogs;

procedure TMyGreatClass.SomethingSpecial;
begin
  ShowMessage('Special');
end;
````


Create a Class Helper and define a new method that relies on the **with** statement as seen below:

````pascal
uses
  MyGreatClassUnit;

interface

type
  TMyGreatHelper = class helper for TMyGreatClass
    procedure CallSomethingSpecial;
  end;

procedure TestIt;

implementation

procedure TMyGreatHelper.CallSomethingSpecial;
begin
  //Self.SomethingSpecial;   {This no longer works as of 10.1 Berlin}

  with Self do SomethingSpecial;  {This works!  Sorry Berlin, your wall was broken down}
end;

procedure TestIt;
var
  x:TMyGreatClass;
begin
  x := TMyGreatClass.Create;
  try
    x.CallSomethingSpecial;  
  finally
    x.Free;
  end;
end.
````
Should you actually use this power available to you?  Only on Tuesday nights when the moon is full perhaps.

I have seen this referenced in a few places, but **Toon Krijthe** may have been the first as he did provide example code a StackOverflow Answer back in 2017: [How to access private methods without helpers?](https://stackoverflow.com/a/42936955/35696) and again: [How to access a private field from a class helper in Delphi 10.1 Berlin?](https://stackoverflow.com/a/42936824/35696)


## Additional Links
If you need more motivation, here are comments from other Delphi developers regarding the **with** statement:

- Even the [Delphi Basics](https://www.delphibasics.co.uk/RTL.php?Name=with) website warns you on using **with**:
> be warned that it can surprisingly make your code more difficult to read, especially when nesting with clauses. More disturbingly, it can create maintenance problems, where a code change can mean that the wrong target for the 'child' field referenced.


- The venerable [Delphi in a Nutshell](https://www.oreilly.com/library/view/delphi-in-a/1565926595/re393.html) book from O'Reilly warns:
> Be careful using the with statement. Indiscriminate use of the with statement obscures the meaning of code and makes it harder to identify the object references that are the targets of methods and properties. Changes to the referenced record, object, class, or interface can cause an identifier to be interpreted in a different scope. If you are fortunate, the change will cause a syntax error; if you are not, the change will not be noticed until your program performs incorrectly.

- **Nick Hodges** clearly stated in an interview about Delphi with [The Register](https://www.theregister.com/2009/02/04/verity_stob_delphi_syntax/?page=2)
> The with keyword. The most hideous, dangerous, blow-your-own-feet-off feature in the language

- Nick also answered a question on [StackOverflow: Any Resources/Tutorials on using nested "With" statements in Delphi?](https://stackoverflow.com/questions/8889911/any-resources-tutorials-on-using-nested-with-statements-in-delphi)

````
The best advice I can give you is:
Do not use with ever.

If you feel like using 'with':
go and lie down until the feeling passes.

If you feel like using a nested with:
pound your hand with a hammer until the desire goes away.

'with' is just a bug waiting to happen. 
Altering classes used with it can alter the meaning of your code. 
It creates imprecise semantics, and that is always bad.

Saving keystrokes is never a good reason to use 'with'. 
A few more keystrokes now will save you a lot of pain later.

'With' should be shunned.
````

- **Martin James** replied to that same answer with the comment:
>I have been bitten by 'with' once - never again. In a complex constructor, it appeared that my code was unable to simply increment an integer. After hours of fiddling and embarrassing myself by posting my problem on a group, 10 or so posters replied 'with!'. Sure enough, some obscure property had the same name as my integer..

- **Fabricio Araujo** also commented:
> And yes, avoid with like the plague... ;-)

- And he also answered the same StackOverflow question with:
> Delphi's with is problematic - it's "imprecise semantics" really can bite you on the rear. Nested with... OMG... Is a disaster waiting to happen. Never needed the nested version on 13 years of Delphi. And I'll avoid for the next years.

- **Mason Wheeler** answered another related question on [StackOverflow: Debugging problems with ‘WITH’ statement in Delphi 2006](https://stackoverflow.com/a/321716/35696) with: 
> The with statement is syntactic NutraSweet: It tastes a lot like syntactic sugar, but leaves a bad aftertaste and turns out to actually be harmful in the long term. It's best to just not use it.

- **David Dombrowsky** answered that same question with: 
> I'm fully convinced that the dreaded "with" clause was included just so book writers didn't have to have all 
those ugly TWinComponent strings in their example code. In real life, outside of code snippets and textbooks, there is almost no good reason to use "with". The primary reason is it breaks the debugger. It would be very impractical for the debugger to evaluate entire clauses when finding the value of a variable, so its not a bug, its just unsupported in all delphi debuggers I know about. If you're like me, stuck maintaining hundreds of thousands of lines from a programmer that basically copied everything out of the textbook, it makes debugging a living hell. Bottom line, DON'T USE WITH-CLAUSES... EVER!

- **Alister Christie** has ancient video [YouTube Video: Delphi Programming Tutorial #19 - The With Statement](https://youtu.be/HcpmXcYdF00) where he provides some example code showing some problems of using **with** statements.

- **David Corneliums** has a blog post [Delphi Debates: With, Goto, & Label--and Exit](https://corneliusconcepts.tech/delphi-debates-with-goto-label) (but he is not completely "anti-with"):
> Generally, the with statement is strongly frowned upon by experienced programmers in the Delphi community, and I agree--with rare exceptions. ...while I recommend against using with on general principles, I would say that IF you're the only programmer maintaining the code and IF the section of code is very small and IF you never use more than one object in a with and IF you never, ever nest them, then in this very limited use case, they are handy.

- **Hallvard Vassbotn** had a blog post years ago on this topic [Hallvard's Blog: The with-statement considered harmful](https://hallvards.blogspot.com/2004/08/with-statement-considered-harmful.html) where has offers a succinct response:
> Delphi's with-statements can be harmful to the readability, maintainability and robustness of your code.

- **Thomas Mueller** stated on a [Delphi-PRAXiS forum message: With's the deal with "With"?](https://en.delphipraxis.net/topic/1614-withs-the-deal-with-with/?do=findComment&comment=12755)
> My recommendation is simple: Don't use the WITH keyword. There are not just the problems you mention. It can also be difficult to determine the scope of an identifier within a WITH block.

- **David Heffernan** also commented on that forum message:
> Debugging was never the issue. That was just annoying. Refactoring was never the issue either. That also is annoying but one of many broken aspects of the refactoring. The issue is the silent bugs that arise when you add a field to a type and that one hides local variables.

 - **Stefan Glienke** also weighed in on that same forum message, using the TRect issue mentioned above:
 > It's not about proper naming - its about newly added members creeping into the with scope - as happened when TRect got Height and Width properties!  It was perfectly clear that Top, Left, Right, Bottom referred inside the with belonged to the "withed" rect variable and Height and Width to something outside - now those got added to TRect and BOOM.

- **Dalija Prasnikar** further commented on that message:
>'with' is relic of another time and another coding practices. And, yes you could shoot yourself in the foot even back then, but now it is extremely easy to do so. Not using 'with' is the best advice one could offer. It is almost like using goto and absolute. You may have some very limited use case where using it is justified, but you will not find such constructs being used at large.

- And she later adds:
>You can shoot yourself in the foot with 'with' even in single line of code. Again, main problem is that code may break due to far away changes you may not even be aware of. 'with' makes code fragile and prone to breakage more than it would be without it. Any code can be broken one way or another. Point is minimizing accidental and subtle breakage as much as possible.

- **Glenn Dufke** commented on [RSP-42059: Language Feature: Enhancement to 'with'](https://quality.embarcadero.com/browse/RSP-42059) states:
> The with-do construct is one of the features which only rarely have a benefit, when looking from a codegen perspective and generally shouldn't be used.  It makes code maintenance and refactoring harder, let alone debugging.

- **Ian Barker** also commented on that same Quality Portal ticket:
> We had a whole debate about this among MVPs and the 'with' keyword was practically universally detested.  The general consensus was that it makes the code more succinct but also has the side effect of making it error prone with often difficult to locate bugs and can also make the code less readable when overused or used without great care. Personally, I remove 'with' keywords from most legacy code I work on.

- **Robert Gilland** also commented on that same ticket with:
> Any time I update Delphi code, my second task is to remove all with statements. With statements hide values during debugging, this is not acceptable.

- **Olaf Monien** commented on another Quality Portal related ticket [RSP-36984: Safer with statement](https://quality.embarcadero.com/browse/RSP-36984):
> The majority consensus (probably not at 95% but anyway) is that the advantages of "with" (shorter, possibly easier to read code) are outweighed by its disadvantages (hardly debuggable, unclear references, resulting in maintenance issues), thus its usage is not allowed in most teams.

But Olaf also seems to favor using the **with** statement if the debugger issues were addressed.


- **Marco Cantu** offered multiple comments: [Blog Post: If With is Considered Harmful, What About Double With?](https://blog.marcocantu.com/blog/with_harmful.html) which discusses **double with** statements.  

- **Chris Miller** commented on Marco's post:
> The double with is an abomination.  The minute amount of time that you save by not having to type the object name to prefix the variable is more than used up by the time you'll need to read that code 6 months after writing it.  

- **Marco Cantu** also covered the topic again [Blog Post: Delphi With Statements and Local Variables](https://blogs.embarcadero.com/delphi-with-statements-and-local-variables/) He provides an example and states:
> As any Delphi developer knows, the with statements have been long in the Object Pascal language since the early Pascal days, but they have also been considered harmful by many, myself included.

- **Malcome Groves** via a web archive link [Blog Post: Writing Solid Delphi Code](https://web.archive.org/web/20060716021322/http://www.malcolmgroves.com/stories/2004/04/05/writingSolidDelphiCode.html) perfectly states:
> Anything that causes you to pause, even momentarily, when reading your code to wonder what scope some operation is going to be executed in, is a bad thing. This feature reduces the readability of your code, increases the number of cycles your brain needs to understand what's going on, and all for benefits that could be achieved other ways!

- **Craig Stuntz** answer on [StackOverflow: Is Delphi "with" keyword a bad practice?](https://stackoverflow.com/a/515933/35696) to a with-related question and provided a good example:
> The biggest danger of with, outside of pathological conditions like "with A, B, C, D" is that your code can silently change meaning with no notice to you.
````pascal
with TFoo.Create
try
  Bar := Baz;
  DoSomething();
finally
  Free;
end;
````
> You write this code knowing that Bar is a property of TFoo, and Baz is a property of the type containing the method which has this code.  Now, two years later, some well-meaning developer comes in adds a Baz property to TFoo. Your code has silently changed meaning. The compiler won't complain, but the code is now broken.


- **Andreas Rejbrand** commented on another question on StackOverflow recently about **with** issues: [Delphi ambiguous call of functions](https://stackoverflow.com/questions/78901164/delphi-ambiguous-call-of-functions):
> this is a great example of why you should never use with statements, and especially not multiple ones. 
 
- I was a little more direct a decade ago on a comment I made [StackOverflow: Using two With statements](https://stackoverflow.com/a/14765968/35696):
> If you are utilizing nested With statements, punch yourself in the face to save time.

If I missed your public post on using **with**, let me know and I can update the post if you provide me a link.  If you have a public post suggesting the continued usage of **with**, then keep it to yourself. 😉

From Telegram:
- **Ali Dehbansiahkarbon** states
>Remember: 'with' is a double-edged sword, while it might seem convenient at first, it can also introduce hidden bloody dangers, ready to trap the unwary developer!

## LLM-Generated Poetry About WITH

I posted this one recently on Telegram during another discussion about the **with** statement:
````
In Delphi’s realm where coders tread,
A WITH statement fills some with dread.
The seasoned ones, they nod and say,
“A shortcut taken, a price to pay.”

The Noob, with eager eyes and grin,
Sees WITH as magic, a win-win.
No need to type, no need to trace,
Just WITH and poof, you're saving space!

But in the shadows, whispers stir,
The lazy path might just deter.
For WITH can cloud the code’s intent,
Obscuring where the data went.

A line, a block, a nested mess,
And now you’re lost, a wild guess.
The pros will warn, with wisdom’s might,
That clarity outshines the fight.

So Delphi coders, hear the plea,
Use WITH with care, let logic be.
For though it’s tempting, sleek and fast,
A coder’s skill should ever last.

In every line, make meaning clear,
Let no shortcuts bring you fear.
For in the end, the true code art,
Is writing smart, and not just WITH your heart.
````

Here is another from ChatGPT:
````
Oh, novice coder, heed this rhyme,
In Delphi's land, a warning chime,
You’ve found the WITH and think it's neat,
A shortcut small, a tiny cheat.

But lurking deep within your code,
A monster sleeps, a hidden load,
As variables lose their way,
In shadowed scopes, they go astray.

The WITH it seems so quick, so clear,
But hides the things that should be near,
As bugs arise, you scratch your head,
“Why’s this broken?” you are led.

Oh noob, if you'd just take the time,
To write it out, to make it fine,
Your future self would thank you well,
No longer trapped in WITH’s dark spell.

So ditch the WITH, avoid the plight,
Keep your code clean, shining bright,
For clarity will be your friend,
And hidden bugs will meet their end.
````

![Stop using WITH in your Delphi code](/assets/blog/Delphi-Programming/Stop-Using-With-Statements-In-Delphi.jpg)


## The Future Of With In Delphi
There is zero chance of **with** being removed from the language.  I do not think we should waste any time discussing that option.  However, there can be a new compiler warning introduced for code that leverages **with** statements.  This would be my preference.  If you agree, then I ask you to add your support by commenting on this Quality Portal ticket requesting a new compiler warning for **with** statement usage:  [RSS-1634: Add a compiler warning for WITH statement usage](https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1634)  (Even a simple "+1" comment would be appreciated.)


## Summary
Some examples were provided to show some obvious and not-so-obvious reasons to avoid **with** statements, multiple quotes were provided from various developers offering a wide consensus against using **with**, and even some poetry... if you still insist on using **with** in your code, then we need to talk about something else and I will simply wish you luck on discovering your next with-related bug before your customers do!


## Updates
- Some comments are coming in on various sites about this article.  A response for the purists who believe Niklaus Wirth implemented **with** statements and it is somehow disrepectful to suggest that Pascal has shortcomings... think about this statement directly from Wirth in "Recollections about the development of Pascal":
>I have been encouraged to state my assessment of the merits and weaknesses of Pascal, of the mistaken decisions in its design, and of its prospects and place in the future. I prefer not to do so explicitly, and instead to refer the reader to my own successive designs, the languages Modula-2 and Oberon. Had I named them Pascal-2 and Pascal-3 instead, the questions might not have been asked, because the evolutionary line of these languages would have been evident.

Now look at Pascal-3 (Oberon) and what did he do?  He removed the WITH, LOOP, and EXIT statements.  Which Niklaus Wirth will you argue with?


### Some Comments From Linked In

- **Tilen Kordis** [posted](https://www.linkedin.com/feed/update/urn:li:activity:7234866143469981697/) 

````
What is one of Delphi statements that makes perfect sense the first time you use it and absolutely NO SENSE the first time you need to debug it?  

WITH Statement!

with LongNamedDelphiVisualControl do
begin
 Top := 1;
 Left := 1;
 Caption := 'In Top Left corner.';
end;

The Delphi documentation is full of examples using WITH and as a new Delphi developer it makes sense to use it, it shortens some code examples and makes some code easier to read. 

Well, that ends the first time you need to debug or refactor it. Then you quickly notice the shortened and readable code could be full of mysteries and it makes sense to try to avoid it in your code. 

The headache that comes with using WITH is not worth it!
````

** Michalis Kamburelis** responded to Tilen's post:
> We have a section in "Coding Conventions" of Castle Game Engine that explicitly says "Do not use with" for this reason:) https://castle-engine.io/coding_conventions#no_with

From his documentation:
> Never use with keyword. Using with makes the code very difficult to read, as some of the symbols inside the "with A do begin …​ end" clause are bound to A, and some are not, and it’s completely invisible to the human reader which symbols are which. And it’s impossible to determine it, without intimately knowing the complete API of class/record A.

** Eugene Kasnerik** [responded](https://www.linkedin.com/feed/update/urn:li:groupPost:1290947-7233568690158329856?commentUrn=urn%3Ali%3Acomment%3A%28groupPost%3A1290947-7233568690158329856%2C7233708262624313344%29&dashCommentUrn=urn%3Ali%3Afsd_comment%3A%287233708262624313344%2Curn%3Ali%3AgroupPost%3A1290947-7233568690158329856%29) to one of my posts:
> I thought this point was obvious for decades. But after reading comments I'm not sure about the planet :) 

** Gordan Paunovic** [responded](https://www.linkedin.com/feed/update/urn:li:groupPost:101829-7233568711738015746?commentUrn=urn%3Ali%3Acomment%3A%28groupPost%3A101829-7233568711738015746%2C7233755585156907011%29&dashCommentUrn=urn%3Ali%3Afsd_comment%3A%287233755585156907011%2Curn%3Ali%3AgroupPost%3A101829-7233568711738015746%29)
> One of the few things which should be removed from Delphi is WITH. Thanks for sharing.


### Some Comments From Facebook

**Brian Muegge**:
> No matter how many times you warn them, there's always somebody who has to touch the stove to see if it's still hot.

**Phillip Woon**:
> You're preaching to the choir

**Bruce McGee**:
> It will never not trigger me

And Bruce's response to someone that defended using **with**
> I've made a good living fixing code written by people who say things like this.

**Marco Eberhardt**:
> if you heavy use WITH then you will get unexpected errors in your code its just a matter if time

**Rick Wheeler**:
> you can use inline “var” declaration to solve this issue. Get rid of nasty with statements.

**Sergio González**:
> No need to read any blog... I stopped using 'with' after struggling with a bug for days, only to find out it was caused by that kind of evil thing! I don't remember now what the bug was, but I do remember swearing by my life never to use 'with' again!
