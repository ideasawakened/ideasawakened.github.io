---
layout: post
title: "Reset your Windows password without any third party tools"
date: 2020-02-17 12:00:00 +0000
last_modified_at: 2020-02-17 12:00:00 +0000
categories: [Tooling]
tags: [Microsoft Windows]
permalink: post/reset-your-windows-password-without-any-third-party-tools
published: true
image: /assets/blog/Microsoft-Windows/Password-Recovery/Utilman-Trick-Square.png
description: Easily hack your own system to recover lost Windows passwords.
---
There are some tasks that come up infrequently, but when they occur you want a reliable solution. Losing your Windows password on an old virtual machine, laptop, or desktop happens on rare occasions and one temptation is to use a hacker tool to regain access. This isn't needed - as you can break the password yourself in a short amount of time without installing any third-party tool.

I have completed this task a few times over the years, and just did it again today on an old laptop that hasn't been booted in a few years. This time, I'll keep some notes for future reference.

This hack is known to work well with Windows 7, and I will try it with Windows 10 and do an update later how that went. I assume it works just as well on the new versions of Windows.

There are a few different variants of this **Utilman.exe trick** available, but they all rely on getting system access to your Windows installation files.  Pick one of this options and then you can start the "Use Utilman.exe hack" step below.

## Option 1 - Windows Install DVD

If you have a Windows 7 installation disc, this allows you to start the installation process, but instead of proceeding with the install process, use the Repair option to gain access to the Recovery Tools provided to help fix problems starting Windows. Simply insert the installer disk, reboot and when you reach the "Install Windows" screen, select the "**Repair your computer**" option. You will be presented with a list of available drives, with the system drive selected. Click on Next and then you'll have the option to select **Command Prompt** recovery tool. Once you are at a command prompt, see below for the actions to take.

_Note_: you may have to tweak your BIOS to allow booting from the DVD drive.

The first Windows 7 installer screen where you can set your language and keyboard options and click **Next:**

![Windows 7 Setup Screen](/assets/blog/Microsoft-Windows/Password-Recovery/Windows-7-DVD-Setup-Screen.png)

Second screen prompts you to Install now, but instead click on the **Repair your computer** option:

![Repair your computer selection](/assets/blog/Microsoft-Windows/Password-Recovery/Select-Repair-Your-Computer-Option.png)

The next screen is presented after a short system inspection. Most users can simply click on the **Next** button:

![System Recovery Options screen](/assets/blog/Microsoft-Windows/Password-Recovery/System-Recovery-Options.png)

The next screen is the **Choose a recovery tool** option where you should click on the **Command Prompt** option:

![Choose a recovery tool screen](/assets/blog/Microsoft-Windows/Password-Recovery/Choose-A-Recovery-Tool.png)

At the command prompt, you'll copy the cmd.exe to Utilman.exe, making a backup first as shown in the image below. You can then restart to proceed to the next step to **use the Utilman.exe hack**.

_note:_ This example assumes your drive is C:, but it may be D: or another drive depending on how your system is configured. If C: doesn't work, try D: and then other drive letters.

![Utilman.exe Password Recovery Trick](/assets/blog/Microsoft-Windows/Password-Recovery/Utilman.exe-Password-Recovery-Trick.png)

_Additional tip:_ To restart the system from the command prompt, use the shutdown command:

````bash
shutdown -r -t 0
````

## Option 2 - hard drive access

A direct option taken by many is to simply remove the hard drive from your system and add it as a slave drive to another working system. From that second system, you should be able to access the files on this disk. Once you have access to your system files, make a copy of utilman.exe and copy cmd.exe to utilman.exe in `C:\Windows\System32.`

## Option 3 - force recovery tools option on boot

A slightly more radical approach if you don't have the Windows DVD handy is to interrupt the boot process by turning off your system while it is booting. On the next reboot, this should give you an option to "**Launch Startup Repair**" which you should select.

![Windows error recovery screen](/assets/blog/Microsoft-Windows/Password-Recovery/Windows-Error-Recovery.png)

Windows will then start a search for problems and should eventually present you the option of "**Startup Repair cannot repair this computer automatically**"

_Note: y_ou may be first prompted with Startup Repair options, which you'll need to click on **Cancel.** Then Windows will start analyzing your system for repair options.

![Startup Repair Options.png screen](/assets/blog/Microsoft-Windows/Password-Recovery/Startup-Repair-Options.png)

Once Windows figures out that there isn't really anything wrong with your system (this may take a few minutes), then you'll be prompted with the message below:

![Startup Repair cannot repair this computer automatically screen](/assets/blog/Microsoft-Windows/Password-Recovery/Startup-Repair-Cannot-Repair-This-Computer-Automatically.png)

When this screen is available, click the drop-down on "**View problem details**" This will reveal a text box with some info on the OS Version along with two privacy notices links. Click the offline privacy notice link to bring up Notepad.

![Click on offline privacy statement screen](/assets/blog/Microsoft-Windows/Password-Recovery/Click-on-offline-privacy-statement.png)

Now that you Notepad running, you can use the File->Open command to bring up a file dialog. From there it's simple to access the needed system files - just change the current directory to **C:\\Windows\\System32** (for most users it will be C:\\) and click on Open to refresh the file list:

![Notepad](/assets/blog/Microsoft-Windows/Password-Recovery/Notepad-File-Open.png)

Once you are in the System32 directory, change the file name filter to **Utilman\*.exe** and click on **Open** again to refresh the list. Find the **Utilman** file entry and right click it and select the Rename option and type in **Utilman.bak**

![Rename utilman](/assets/blog/Microsoft-Windows/Password-Recovery/Rename-utilman.png)

Then change the file name filter to **cmd\*.exe** and click on **Open** to refresh. Then find the **cmd** file entry and right click it and select the Rename menu and rename it to Utilman. (or copy and paste it to a new file and then rename the new file to utilman.) Then close out the screens and let the system reboot to **use the Utilman.exe hack**.

_Note_: the file extensions are hidden by default in Explorer.

## Use the Utilman.exe hack

After you have copied cmd.exe over the Utilman.exe file in one of the three ways detailed above and have rebooted the system, when you have reached the logon screen, click on the **Utility Manager** icon in the lower left hand corner of the screen (or use **Windows Key + U** hotkey combination.) Since you replaced this utility with the Windows Command Shell, you will find yourself at the command prompt where you can do all the needed user changes:

![Screen showing the Utliman trick worked](/assets/blog/Microsoft-Windows/Password-Recovery/Utilman.exe-trick-worked.png)

## Edit user account settings

The easiest approach is to simply reset the password of the desired user account to be blank. This will allow you to logon without a password and then reset it using the normal tools. If your username is **Bob**, then the command would be:

````shell
net user Bob \*
````

Then hit enter twice on the password prompts.

_Additional tip:_ If you don't remember your Windows username, you can list all the usernames at a command prompt with this simple net command:

````shell
net user
````

Another approach is to create a new user with Administrator rights, in this example we are adding a user call **MyTempAdmin**:

````shell
net user MyTempAdmin /add
net localgroup administators MyTempAdmin /add
````

Once you have tweaked your user accounts, simply type **Exit** and hit enter to close out the command prompt. Then click on the screen to refresh the list of users as needed. You should be able to logon to your user account, or to the new admin account that you just created! You have now hacked into your own system.

## Important to clean up changes made

Ensure you clean up after the actions taken above:

-   If you created a new temporary admin account, then leverage it fix your account and then after you have rebooted successfully into to your account, delete this temp admin account.
    
-   If you reset your account password to blank, ensure you set a new password.
    
-   Also remember to clean up the Utilman.exe hack completed above. Reverse the change made by copying Utilman.exe.bak to Utilman.exe in the Windows\\System32 folder. (If you renamed cmd.exe, first copy utilman.exe back to cmd.exe.) At a command prompt:
    

````shell
cd /d C:\Windows\System32
copy /y utilman.exe cmd.exe
copy /y utilman.exe.bak utilman.exe
del utilman.exe.bak
````

If you created a temp administrator account, you can also easily delete the account from the command prompt:

````shell
net user MyTempAdmin /delete
````

_Additional tip:_ Most people use the user settings applet in Control Panel instead of the net user commands. A quick way to open this user settings applet is to start->run (or **Windows Key + R**) and type:

````shell
lusrmgr.msc
````

## Summary

If you have physical access to a Windows machine, then you can typically gain full access to the system using the simple methods described above. There's no need to rely on hacker tools or downloads from questionable websites. It's a simple approach that has been utilized by many people over the years and it came in handy again today for me when trying to logon to an old laptop.