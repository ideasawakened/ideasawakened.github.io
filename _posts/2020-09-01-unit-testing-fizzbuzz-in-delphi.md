---
layout: post
title: "Unit Testing FizzBuzz in Delphi"
date: 2020-09-01 12:00:00 +0000
last_modified_at: 2020-09-01 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Job Search, TDD, DelphiKB]
permalink: post/unit-testing-fizzbuzz-in-delphi
published: true
image: /assets/blog/Delphi-Programming/FizzBuzz-Unit-Testing-small.png
description: Preparing for software developer interview questions with a look at a FizzBuzz solution coded in Delphi.
---
In the [**last article**](https://www.ideasawakened.com/post/developer-interview-questions-fizzbuzz-in-delphi), a simple solution to the common FizzBuzz software developer interview screening test was discussed. For the majority of employers, the simple solution provided will likely be acceptable as it will be very similar to the other submissions received. However, don't you want to be a little bit different in the interview process? Besides, there's a healthy percentage of developer teams that insist on TDD, and if you try to join one of those teams by submitting non-testable code during the interview process, then you are going to be at a competitive disadvantage.

## Introducing the first Unit Tests

I ran across a definition of Legacy Code somewhere and it has stuck with me: Legacy Code is defined simply as **code without tests**. (And perhaps one of the most important properties of Legacy Code is that you typically resist making any changes for fear of breaking something.) You can assume that every time that you start to write the code first instead of the test first, you are extending your legacy codebase. Perhaps future blog articles will address some of the issues of an ever-growing legacy codebase, but we'll skip those long debates for the moment.

The solution provided in the last article was simply not easily testable. The responsibilities were intermixed as the game logic was intimately tied to the console output. The only way to test the results of our application is to parse the output displayed on the screen.

Unit Testing seems like a pretty straight-forward subject, but there are variations and nuances emphasized by different programming teams. For example, you may run into a **Must-Mock** team which insist on coding everything to an interface and mocking every aspect of your system during testing to enforce as much isolation as possible. You may also run into a team that uses DUnit only for building **Integration Tests** without a single [**test double**](https://www.martinfowler.com/bliki/TestDouble.html) in sight.

For this article, we'll take our previous application and extend it with DUnit to provide a simple test suite to validate the FizzBuzz output. First, we'll need to separate the console output from the value generation. (You should note that thinking about the tests first, or even better by starting to write the tests up front, this desirable separation should have been obvious.) Let's create a new unit and move the game logic into it:

````pascal
unit FizzBuzz.GameLogic;

interface

function CalcFizzBuzz(const pFizz, pBuzz, pValue:Integer):string;


implementation

uses
  System.SysUtils;


function CalcFizzBuzz(const pFizz, pBuzz, pValue:Integer):string;

  function IsMultiple(const x, y:Integer):Boolean;
  begin
    Result := (x mod y = 0);
  end;


begin
  Result := '';

  if IsMultiple(pValue, pFizz) then
  begin
    Result := 'Fizz';
  end;
  if IsMultiple(pValue, pBuzz) then
  begin
    Result := Result + 'Buzz';
  end;
  if Result = '' then
  begin
    Result := IntToStr(pValue);
  end;
end;


end.
````

The loop is initially moved into the DPR during this refactor:

````pascal
program FizzBuzz;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FizzBuzz.GameLogic in '..\Source\FizzBuzz.GameLogic.pas';

var
  i:Integer;
begin
  try
    for i := 1 to 100 do
    begin
      WriteLn(CalcFizzBuzz(3,5,i));
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
````

If you run this, you'll get the expected outcome if you manually inspect the console window. But now let's add some tests to automate this validation process.

Select the **Project**\->**Add New Project...** menu option in the Delphi IDE (or right click the default **ProjectGroup1** in the Project Explorer and select the same **Add New Project...** menu item.) You should be presented with a **New Items** dialog. Click on the **Other**\->**Unit Test** category and ensure **Test Project** is selected and then select **OK**.

This should start the **Test Project Wizard** dialog. Let's use the defaults for now and simply click on the **Finish** button. You can then add a new unit to the project which we will use for our suite of tests. In the current scenario, we have a simple procedure to test so we'll create the test unit ourselves and not rely on the DUnit expert to generate the test suite unit.

Below is what the first test suite code might look like, and all the tests currently pass.

````pascal
unit TestFizzBuzz;

interface

uses
  TestFramework;

type

  TTestFizzBuzz = class(TTestCase)
  published
    procedure CheckFizz;
    procedure CheckBuzz;
    procedure CheckFizzBuzz;
    procedure CheckNonFizzBuzz;
  end;

const
  FizzOutput = 'Fizz';
  BuzzOutput = 'Buzz';
  FizzVal = 3;
  BuzzVal = 5;


implementation
uses
 FizzBuzz.GameLogic;

procedure TTestFizzBuzz.CheckFizz;
begin
  CheckEqualsString(FizzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal));
  CheckEqualsString(FizzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*2));
  CheckEqualsString(FizzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*3));
  CheckEqualsString(FizzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*101));
end;

procedure TTestFizzBuzz.CheckBuzz;
begin
  CheckEqualsString(BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, BuzzVal));
  CheckEqualsString(BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, BuzzVal*2));
  CheckEqualsString(BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, BuzzVal*4));
  CheckEqualsString(BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, BuzzVal*101));
end;

procedure TTestFizzBuzz.CheckFizzBuzz;
begin
  CheckEqualsString(FizzOutput+BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*BuzzVal));
  CheckEqualsString(FizzOutput+BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*BuzzVal*2));
  CheckEqualsString(FizzOutput+BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*BuzzVal*3));
  CheckEqualsString(FizzOutput+BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*BuzzVal*4));
  CheckEqualsString(FizzOutput+BuzzOutput, CalcFizzBuzz(FizzVal, BuzzVal, FizzVal*BuzzVal*5));
end;

procedure TTestFizzBuzz.CheckNonFizzBuzz;
begin
  CheckEqualsString('1', CalcFizzBuzz(FizzVal, BuzzVal, 1));
  CheckEqualsString('2', CalcFizzBuzz(FizzVal, BuzzVal, 2));
  CheckEqualsString('4', CalcFizzBuzz(FizzVal, BuzzVal, 4));
  CheckEqualsString('7', CalcFizzBuzz(FizzVal, BuzzVal, 7));
end;

initialization
  RegisterTest(TTestFizzBuzz.Suite);

end.
````

For a quick code screening test, adding some unit tests like this to the FizzBuzz project could help distinguish you from the other applicants. (But if you started with the tests first you would not have to refactor later.) Let's not affix our 'Must-Mock' badges on our shirts just yet and over-engineer the solution, but let us see if we can quickly improve on the code above.

## Rethinking FizzBuzz

Creating a simple procedure is a common solution to many Delphi tasks. As we shown above, these simple procedures can certainly be testable - and if you are just going to get started with Unit Testing, this may be a valid first step. However, the DUnit tooling is built around testing classes, so you will inherit some minor benefits by simply switching to class based development.

Let's refactor the CalcFizzBuzz method into a class. This takes your code embedded into a single procedure and allows it to be more extensible in the future.

````pascal
unit FizzBuzz.GameLogic;

interface

type

  TFizzBuzzGame = class
  public const
    FizzText = 'Fizz';
    BuzzText = 'Buzz';
  private const
    defFizzValue = 3;
    defBuzzValue = 5;
  private
    fFizz:Integer;
    fBuzz:Integer;
  protected
    function IsMultiple(const x, y:Integer):Boolean;
  public
    constructor Create();

    function CalcFizzBuzz(const pValue:Integer):string;

    property Fizz:Integer read fFizz write fFizz;
    property Buzz:Integer read fBuzz write fBuzz;
  end;


implementation

uses
  System.SysUtils;


constructor TFizzBuzzGame.Create();
begin
  inherited;
  fFizz := defFizzValue;
  fBuzz := defBuzzValue;
end;


function TFizzBuzzGame.IsMultiple(const x, y:Integer):Boolean;
begin
  Result := (x mod y = 0);
end;


function TFizzBuzzGame.CalcFizzBuzz(const pValue:Integer):string;
begin
  Result := '';

  if IsMultiple(pValue, Fizz) then
  begin
    Result := FizzText;
  end;
  if IsMultiple(pValue, Buzz) then
  begin
    Result := Result + BuzzText;
  end;
  if Result = '' then
  begin
    Result := IntToStr(pValue);
  end;
end;


end.
````

As you can see, it's nearly identical code to the original simple procedure but it's laid out in a way that makes it easier to work with. It seems pretty minor in this small bit of code, but sometimes small improvements go a long way.

This new class forces some minor changes to the DPR file to utilize this new class. Perhaps we could extract the loop into a GameSession in a future modification. And perhaps we could extract most of this code out of the DPR... it all depends on how much effort and time you have to apply to this task.

````pascal
program FizzBuzz;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FizzBuzz.GameLogic in '..\Source\FizzBuzz.GameLogic.pas';

var
  i:Integer;
  vGame:TFizzBuzzGame;
begin
  try
    vGame := TFizzBuzzGame.Create();
    try
      for i := 1 to 100 do
      begin
        WriteLn(vGame.CalcFizzBuzz(i));
      end;
    finally
      vGame.Free();
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
````

The test unit is updated as well:

````pascal
unit TestFizzBuzz;

interface

uses
  TestFramework,
  FizzBuzz.GameLogic;

type

  TestTFizzBuzzGame = class(TTestCase)
  private const
    TestFizzVal = 3;
    TestBuzzVal = 5;
  strict private
    fGame:TFizzBuzzGame;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckFizz;
    procedure CheckBuzz;
    procedure CheckFizzBuzz;
    procedure CheckNonFizzBuzz;
    procedure CheckFizzText;
    procedure CheckBuzzText;
  end;


implementation


procedure TestTFizzBuzzGame.SetUp;
begin
  fGame := TFizzBuzzGame.Create;
  fGame.Fizz := TestFizzVal;
  fGame.Buzz := TestBuzzVal;
end;


procedure TestTFizzBuzzGame.TearDown;
begin
  fGame.Free;
  fGame := nil;
end;


procedure TestTFizzBuzzGame.CheckFizz;
begin
  CheckEqualsString(fGame.FizzText, fGame.CalcFizzBuzz(TestFizzVal));
  CheckEqualsString(fGame.FizzText, fGame.CalcFizzBuzz(TestFizzVal * 2));
  CheckEqualsString(fGame.FizzText, fGame.CalcFizzBuzz(TestFizzVal * 3));
  CheckEqualsString(fGame.FizzText, fGame.CalcFizzBuzz(TestFizzVal * 101));
end;


procedure TestTFizzBuzzGame.CheckBuzz;
begin
  CheckEqualsString(fGame.BuzzText, fGame.CalcFizzBuzz(TestBuzzVal));
  CheckEqualsString(fGame.BuzzText, fGame.CalcFizzBuzz(TestBuzzVal * 2));
  CheckEqualsString(fGame.BuzzText, fGame.CalcFizzBuzz(TestBuzzVal * 4));
  CheckEqualsString(fGame.BuzzText, fGame.CalcFizzBuzz(TestBuzzVal * 101));
end;


procedure TestTFizzBuzzGame.CheckFizzBuzz;
begin
  CheckEqualsString(fGame.FizzText + fGame.BuzzText, fGame.CalcFizzBuzz(TestFizzVal * TestBuzzVal));
  CheckEqualsString(fGame.FizzText + fGame.BuzzText, fGame.CalcFizzBuzz(TestFizzVal * TestBuzzVal * 2));
  CheckEqualsString(fGame.FizzText + fGame.BuzzText, fGame.CalcFizzBuzz(TestFizzVal * TestBuzzVal * 3));
  CheckEqualsString(fGame.FizzText + fGame.BuzzText, fGame.CalcFizzBuzz(TestFizzVal * TestBuzzVal * 4));
  CheckEqualsString(fGame.FizzText + fGame.BuzzText, fGame.CalcFizzBuzz(TestFizzVal * TestBuzzVal * 5));
end;


procedure TestTFizzBuzzGame.CheckNonFizzBuzz;
begin
  CheckEqualsString('1', fGame.CalcFizzBuzz(1));
  CheckEqualsString('2', fGame.CalcFizzBuzz(2));
  CheckEqualsString('4', fGame.CalcFizzBuzz(4));
  CheckEqualsString('7', fGame.CalcFizzBuzz(7));
end;


procedure TestTFizzBuzzGame.CheckFizzText;
begin
  CheckEqualsString('Fizz', fGame.FizzText);
end;


procedure TestTFizzBuzzGame.CheckBuzzText;
begin
  CheckEqualsString('Buzz', fGame.BuzzText);
end;


initialization

RegisterTest(TestTFizzBuzzGame.Suite);

end.
````

Running the tests and seeing all green is good! We know we haven't screwed up the basic operation of our FizzBuzz solution.

![FizzBuzz Unit Tests - All Tests Pass](/assets/blog/Delphi-Programming/FizzBuzz-Unit-Testing-ScreenShot.png "FizzBuzz Unit Tests - All Tests Pass")

## Summary

If you were only given a short amount of time to whip out a FizzBuzz solution in a job interview, you will probably end up with a little time to refactor. As you can see above, it doesn't take much effort to change from a simple procedure to a class and this process helps to clean up some of the code and allow for easier modifications. And if you provide testable code up front, you are nearly guaranteed to be ahead of other applicants (besides helping to ensure that your little code test actually works as intended!) In addition, some employers may ask for small modifications to your FizzBuzz solution in order to see how quickly and easily you can handle change requests. For example, they may ask for all values divisble by 2 to return "Flop". If you have the test infrastructure already in place, it helps to avoid some silly mistakes when you are making quick changes in a potentially stressful situation like a job interview.

There's plenty more we can do with this silly FizzBuzz code. Perhaps more articles will be posted in the near future based on feedback.

## DUnitX

You shouldn't mention unit testing in Delphi without a reference to [**DUnitX**](https://www.finalbuilder.com/resources/blogs/introducing-dunitx). This is currently the gold standard in unit testing within the Delphi community. It's available in the later versions of Delphi, or simply grab it from [**GitHub**](https://github.com/VSoftTechnologies/DUnitX). It's simply much more powerful than the built in DUnit framework as it's built for Generics, Extended RTTI, attribute based testing and more.