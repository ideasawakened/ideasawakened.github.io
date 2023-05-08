---
layout: post
title: "Developer interview questions: FizzBuzz in Delphi"
date: 2020-08-31 12:00:00 +0000
last_modified_at: 2020-09-01 12:00:00 +0000
categories: [Delphi Programming, Example Code]
tags: [Job Search, Martin Fowler, DelphiKB]
permalink: post/developer-interview-questions-fizzbuzz-in-delphi
published: true
image: /assets/blog/Delphi-Programming/FizzBuzz-Delphi-small.png
description: Preparing for software developer interview questions with a look at a FizzBuzz solution coded in Delphi.
---
A lot of time, effort, and money is invested into hiring developers so businesses typically go out of their way to ensure they have a qualified candidate. As I've experienced a few times over the years, hiring the wrong developer is often much worse than not hiring one at all.

If you are a software developer looking for work, you should be well prepared for the interview process. In this blog post, we will discuss one possible interview screening question: **coding a FizzBuzz solution.** If there are many candidates available for a single job opening, a simple coding test like this is often utilized to filter out the lower-end candidates.

![Running FizzBuzz example code in Delphi](/assets/blog/Delphi-Programming/Coding-FizzBuzz-In-Delphi.png)

## What is the FizzBuzz game?

Fizz Buzz is a word game for children to help them improve their math skills. Participants take turns counting upwards by incrementing the previous participants number. When their number happens to be divisible by three, they must state "FIZZ" as their response. If their number happens to be divisible by five, they must state "BUZZ" as their response. If their number is divisible by both three and five, then they must state "FIZZBUZZ"; otherwise they simply speak their number. If you take too long, or get it wrong, you are eliminated from the game. The last one remaining wins the game.

## What is the FizzBuzz coding test?

Developers are asked to quickly produce code that would mimic the output of the Fizz Buzz math game. The rules are typically stated similar to the following:

> Output every number in the range of 1 to 100 and if the number is divisible by 3, output Fizz. If the number is divisible by 5, output Buzz. If the number is divisible by 3 and 5, output FizzBuzz. Otherwise, output the actual number.

You are typically given 5 minutes or less to complete this simple coding task.

## Coding the FizzBuzz solution in Delphi

Note: the solution somewhat depends on the particular version of Delphi in use and how the output is to be implemented. For this example, we'll take the likely most utilized approach and that is to create a Win32 console application that simply writes the values to the console. (The code within the DPR file will simply call a procedure found in a separate unit.)

The first solution would be to follow their instructions and generate a procedure something like this:

````pascal
// First instinct - code exactly as requested.
procedure FizzBuzzExample1;
var
  i:Integer;
begin
  for i := 1 to 100 do
  begin
    if i mod 3 = 0 then
    begin
      WriteLn('Fizz');
    end
    else if i mod 5 = 0 then
    begin
      WriteLn('Buzz');
    end
    else if (i mod 3 = 0) and (i mod 5 = 0) then
    begin
      WriteLn('FizzBuzz');
    end
    else
    begin
      WriteLn(IntToStr(i));
    end;
  end;
end;
````

While it seems to satisfy each of the specific requests, once you run the code you should find that it never outputs FizzBuzz. Don't be the job applicant who turns in non-tested code!

One correct solution is to simply test for FizzBuzz first. The code below satisfies a first-pass solution to the FizzBuzz coding test in Delphi:

````pascal
procedure FizzBuzzExample2;
var
  i:Integer;
begin
  for i := 1 to 100 do
  begin
    if (i mod 3 = 0) and (i mod 5 = 0) then
    begin
      WriteLn('FizzBuzz');
    end
    else if i mod 3 = 0 then
    begin
      WriteLn('Fizz');
    end
    else if i mod 5 = 0 then
    begin
      WriteLn('Buzz');
    end
    else
    begin
      WriteLn(IntToStr(i));
    end;
  end;
end;
````

As a follow-up, they may ask to make the code extendable by allowing the user to specify the specific numbers to use for Fizz, Buzz, and to possibly refine the number range. (Or you may want to show a little initiative and provide this in your initial solution!) One possible extended solution follows:

````pascal
procedure FizzBuzzExample3(const pFizz, pBuzz:Integer; const pRangeStart, pRangeEnd:Integer);
var
  i:Integer;
begin
  for i := pRangeStart to pRangeEnd do
  begin
    if (i mod pFizz = 0) and (i mod pBuzz = 0) then
    begin
      WriteLn('FizzBuzz');
    end
    else if i mod pFizz = 0 then
    begin
      WriteLn('Fizz');
    end
    else if i mod pBuzz = 0 then
    begin
      WriteLn('Buzz');
    end
    else
    begin
      WriteLn(IntToStr(i));
    end;
  end;
end;
````

As part of the interview process, they may try to refactor the code in real time with you to see how you respond. For example, they may ask "How do we reduce the math operations?" What follows is a slightly refactored version which reduces the number of checks made and slightly simplifies the logic.

````pascal
procedure FizzBuzzExample4(const pFizz, pBuzz:Integer; const pRangeStart, pRangeEnd:Integer);

  function IsMultiple(const x, y:Integer):Boolean;
  begin
    Result := (x mod y = 0);
  end;


var
  i:Integer;
  vOutput:string;
begin
  for i := pRangeStart to pRangeEnd do
  begin
    vOutput := '';

    if IsMultiple(i, pFizz) then
    begin
      vOutput := 'Fizz';
    end;
    if IsMultiple(i, pBuzz) then
    begin
      vOutput := vOutput + 'Buzz';
    end;
    if vOutput = '' then
    begin
      vOutput := IntToStr(i);
    end;

    WriteLn(vOutput);
  end;
end;
````

Another option is to ask you to write a simplified version using a case statement which could be something like the following code:

````pascal
procedure FizzBuzzExample_UseCase;
var
  i:Integer;
begin
  for i := 1 to 100 do
  begin
    case i mod 15 of
      3, 6, 9, 12:
        WriteLn('Fizz');
      5, 10:
        WriteLn('Buzz');
      0:
        WriteLn('FizzBuzz');
      else
        WriteLn(IntToStr(i));
    end;
  end;
end;
````

Perhaps you will be asked to use specific classes like TStringList or TDictionary within your solution. It's also likely you may be asked to write automated tests for your FizzBuzz project. The bottom line is that you should be prepared to provide a FizzBuzz type solution when you are interviewing for a developer position. Of course, this isn't limited to Delphi developers, as nearly all software developers are asked for provide a FizzBuzz type solution in their targeted language.

## The value of FizzBuzz

There are many ways to solve FizzBuzz, so there is no perfect answer. The employer is simply looking to see how comfortable you are at solving a simple problem in a short amount of time. Some employers may like a fully extendable version up front, while some may see it as over-engineered for such a simple assignment. To help with that potential discussion of your code, review **Martin Fowler**'s article on [**YAGNI**](https://www.martinfowler.com/bliki/Yagni.html) for discussion points.

![YAGNI cartoon](/assets/blog/Delphi-Programming/FizzBuzz-YAGNI.png)

Another value of this assignment is for the employer to see what sort of questions you will ask before you start to code. I personally asked dozens of applicants over the years to generate some sample code in response to a coding test like FizzBuzz and very few ever asked any qualifying questions before coding. In some cases, the directions were intentionally a little vague just to see how the candidate responded. It's encouraging to get a question, just about any question, from a job applicant on a test like FizzBuzz before they start to write code. I would suggest something like "Could the number range be customizable in the future?" to prod the interviewer a little to see how extensible your code is expected to be. (To be clear: I would only ask a question like that in person, or in live chat; I would not generate a simple question via email and expect a response via email before completing a simple 5-minute task like this.)

## Summary

The odds are that you will not be asked to write a FizzBuzz solution in Delphi during your next job interview. However, the odds are that you will be asked to provide some sort of quick code solution during your interview process. Be prepared to show off your skills. In any case, ensure that your code actually works! Run it more than once to be sure. Definitely consider writing the tests first if you have the time, but be careful not to over-engineer something quick like this as you may turn a simple first step in the overall process into a quick fail. If you feel obligated to show off a little, I suggest that you provide a quick-n-dirty version and a second 'upgraded' version. (Perhaps you provide the quick version with a note that it took a few minutes and provide your more extendable version which took a few extra minutes.) Whatever you turn in, be prepared to defend and possibly refactor your code.

## Delphi FizzBuzz on GitHub

I added a simple [**Delphi project to GitHub**](https://github.com/ideasawakened/DelphiKB/tree/master/Delphi%20Demos/FizzBuzz) with the FizzBuzz code in this article. If you would like to create your own highly engineered solution to FizzBuzz, send me a Pull Request and I'll add it to the repo. Kudos out to the most over-engineered FizzBuzz solution!

## Update: 
See the [**next article**](https://www.ideasawakened.com/post/unit-testing-fizzbuzz-in-delphi) on extending FizzBuzz with unit tests.