---
layout: post
title: "Prevent concurrent execution of a batch file in Microsoft Windows"
date: 2023-05-27 12:00:00 +0000
last_modified_at: 2023-05-27 12:00:00 +0000
categories: [Tooling, Microsoft Windows]
tags: [Microsoft Windows]
permalink: post/prevent-concurrent-execution-batch-file-implementation-how-to
published: true
image: /assets/blog/Microsoft-Windows/singleton-batch-file-execution/Bing_AI_Generated_LockFile_small.jpg
description: "Blog post which introduces a simple technique to enforce single user access to run a batch file on Microsoft Windows"
---

Batch files are a convenient way to automate repetitive tasks on Windows systems. However, in certain scenarios, it is crucial to ensure that a batch file can only be executed one at a time to avoid conflicts or data corruption. In this blog post, we will explore a technique to enforce single execution of a batch file using a locking mechanism.

## Example Batch File
To begin, open a text editor and create a new file. Save it with a ".bat" extension, giving it a descriptive name that reflects its purpose. For this example, let's use "singleuser.bat".  I have also posted a batch file on [GitHub Gists](https://gist.github.com/darianmiller/4df2423d57a83c903a3a410ce1e978bd) to download, if desired.

To enforce single execution, we need to implement a locking mechanism within the batch file. We can achieve this by obtaining a lock to the batch file itself. Here's an example implementation:

```` shell
@echo off
:: Workaround for expansion issue
call :getLock %1 %2 %3
exit /b

:getLock
:: Open this batch file in append mode, preventing others from doing so
call :singleuser-main %1 %2 %3 9>>"%~f0"
exit /b

:singleuser-main
:: Body of your script goes here. Only one process at a time can execute. 
:: The lock will be released upon return from this routine, or when the batch file terminates for any reason
echo %*
pause
````

The first call statement is there to avoid a bug with %~f0 when the script is executed with quotes around the script name.  (For more info and some interesting [comments](https://github.com/microsoft/terminal/issues/15212#issuecomment-1516185592), see this [GitHub Issue](https://github.com/microsoft/terminal/issues/15212))
This work-around simply ensures that the `"%~f0"` syntax correctly expands to the double-quoted, fully qualified path of the batch file.

## :getLock
- The `9>>` is the magic that starts a new instance at the `singleuser-main` label while redirecting output from the special file handle #9 to the batch file itself, opening the batch file in Append Mode. This will fail if the file is already open.  This actually does not redirect anything to the file as there is no output coming from #9 (this is just a trick to establish a write lock on the batch file.)
- `%~f0` expands to the fully qualified path of the current batch file
- `%1 %2 %3` are example command-line paramters.  You can forward zero, one or more parameters to these subroutines as required.

## Redirection Notes
- The Windows command processor interprets a single digit followed by a redirection symbol as a request to redirect to that specific file handle, with 0, 1, and 2 being used as Standard Input, Standard Output, and Standard Error. In PowerShell, this concept was extended to use 3 to 6 for Warning, Verbose, Debug, and Information output types. (Since 7 through 9 are currently undefined and unused, we specify 9 here.)
- Dating back many years, you could always echo text to files like this: `echo TestMe>myfile.txt` which generates a file called `myfile.txt` that has one line of text in it with the content of `TestMe`.
- With this new redirection trick being introduced with Windows NT, they had to come up with a way to tell the difference between a single digit representing a file handle redirection or simple textual output, so they introduced a caret prefix. For those few unlucky people that wanted to continue to echo a single digit to a text file they now had to prefix their single digit with a caret, such as `echo ^1>myfile.txt` which writes the single digit `1` to `myfile.txt`. If you do not include a caret, then the single digit is assumed to be a file handle. I wonder how many people since then have successfully executed the command `echo something>somefile.txt` and are unfortunate enough to stumble upon this quirk when "something" just happens to be a single numeric digit. They could be writing numeric results with `echo 1000>somefile.txt` and `echo 10>somefile.txt` working as expected and then notice that `echo 9>somefile.txt` fails. 
- Here is a test: what does the `somefile.txt` file contain after you run the following command? `echo 1>somefile.txt`
- Redirection example, this command `DIR myfilenametofind.txt 1>filelist.txt 2>error.txt` creates a file called `filelist.txt` for the output from the DIR command and another file called `error.txt` for any error messages (such as *File Not Found*.)

## :singleuser-main

This is where you can place the body of your script and only one instance will be allowed at a time.  In this example, we execute a simple `pause` command to help demonstrate the effect of single execution. Replace this with your own script logic.  We also use `echo %*` which is a short-cut to display the values of all command line parameters.

## Validate
To run the batch file, simply double-click on it in Explorer, and it will execute within a console window.  If you attempt to run a second instance, you will receive the error `Another program is currently using this file.`

Try editing the file in Notepad and saving changes while the batch file is running. Notepad will also give you an error: `The process cannot access the file because it is being used by another process.`

## Summary
Using the tips outlined in this blog post, you can create batch files that enforce single execution to help automate tasks reliably on Microsoft Windows.  One potential use is to enforce single-user execution with background services or scheduled tasks which execute a specific batch file that could also be executed manually by interactive users.

![Bing AI-generated lockfile art](/assets/blog/Microsoft-Windows/singleton-batch-file-execution/Bing_AI_Generated_LockFile.jpg)

## AI Generated Image
AI image generation is becoming quite popular and I used [Bing](https://www.bing.com/create) (powered by DALL-E) to create the image for this post.  