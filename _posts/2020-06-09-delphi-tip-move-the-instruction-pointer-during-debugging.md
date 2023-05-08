---
layout: post
title: "Delphi Tip: Move the instruction pointer during debugging"
date: 2020-06-09 12:00:00 +0000
last_modified_at: 2020-06-09 12:00:00 +0000
categories: [Delphi Programming, IDE]
tags: [Debugging]
permalink: post/delphi-tip-move-the-instruction-pointer-during-debugging
published: true
image: /assets/blog/Debugging/Instruction-Pointer/Instruction-Pointer-IDE-Gutter.png
description: Quick tip on using the "Set Next Statement" debugger option in the Delphi IDE. This is also available by dragging within the editor gutter.
---
## What is an instruction pointer?

This **execution pointer** holds the address of the next line of instruction. It seems to be defined in the Intel x86 world as _instruction pointer_, but may be better known as the [**program counter**](https://en.wikipedia.org/wiki/Program_counter). While you are debugging in Delphi, the IDE includes an arrow glyph in the editor gutter and colorizes the source code line that will be executed next. In the image below, line 404 is the next line to be executed and is colored using the Delphi 10.4 Sydney default colors. (In the _Options_ menu you can customize the **Execution Point** background and foreground colors.)

![Debugging game of life code](/assets/blog/Debugging/Instruction-Pointer/Game-Of-Life-ExecutionPoint.png)

It's likely that the majority of Delphi developers are quite accustomed to using hotkeys to control execution flow during a debugging session, including **F8** to Step Over a line, **F7** to Trace Into a line, and **F4** to Run to Cursor. But there are other options available, including a very powerful one that some developers may not utilize and that is to right-click a source line in the editor, select the Debug menu, and then select the "**Set Next Statement**" menu item. This can also be easily accomplished using your mouse by dragging the instruction pointer within the editor gutter. In the example above, you can left-click inside the editor gutter (within the area including the blue arrow and the line number) and drag the instruction pointer up or down to the desired source code line. For example, you could restart the for loop by dragging the pointer up to line 402.

This feature is a fantastic way to save time during a debugging session. If you are debugging a fairly complex situation and you seem to have arrived to a specific area of concern after a lot of effort, you can repeat sections of code without having to start the whole debugging process again. (Of course, this assumes repeating the execution of code doesn't alter the expected outcome.)

Perhaps you are trying to diagnose some problems during the generation of a specific report and you have some code that creates and populates an output file with data, generates a specific report from the output file, sends the report via email and FTP, and finally deletes the temporary files created. You could put a breakpoint on the start of the report generation routine and step through its code. If you need to keep debugging that report generation routine, you could reset the instruction pointer to restart the report generation routine without having to start a new debugging session and without wasting the time to generate the data file.

![Multiple Breakpoints](/assets/blog/Debugging/Instruction-Pointer/Example-ExecutionPoint.png)

You can also use this feature to easily skip sections of code. In the previous example, once you have figured out the bug in the report generation routine you can put a breakpoint to stop execution before delivering the report via email and FTP. Once that breakpoint is hit, simply move the instruction pointer beyond the delivery method and let the code continue to delete the temporary output files.

![Move the instruction pointer meme image](/assets/blog/Debugging/Instruction-Pointer/Move-Execution-Point-Meme.png)

So instead of just relying on stepping through code with the commonly used F4, F7, and F8 keys you can easily repeat or skip code by simply moving the instruction pointer. Note that this can easily get you into trouble if you move the pointer to a different method. Delphi will allow you to move the pointer, but you'll likely get some immediate access violations. You have been duly warned!

## Summary

Most developers are pretty creative people - except when it comes to how they use their tools. Getting some developers to change existing behavior is typically a challenge - with the likely exception when there are major time savings involved. This is one of those times when a change in behavior should be obvious. Moving the instruction pointer around should be as comfortable in your debugging session as hitting F8. For those developers already using this feature daily, I'm sure you've said "no kidding" to yourself a few times already (if you even made it this far.) After all, this feature has been around since Delphi 2010 so it's not ground-breaking information and it's also a common action taken by many developers in various programming languages. However, is it as easy as dragging the instruction pointer around inside the editor window? It's that easy in Delphi!

