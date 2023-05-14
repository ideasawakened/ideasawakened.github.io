---
layout: post
title: "Quick fix for for RAD Studio 11 license failure during installation"
date: 2021-09-09 12:00:00 +0000
last_modified_at: 2021-09-14 12:00:00 +0000
categories: [Delphi Programming, New Releases]
tags: [11 Alexandria, Marco Cantu, Uwe Raabe, Dave Nottage, Licensing]
permalink: post/quick-fix-for-for-rad-studio-11-license-failure-during-installation
published: true
image: /assets/blog/RAD-Studio-11-Alexandria/RAD-Studio-11-License-Manager-Fix-square.png
description: 'Some installations of RAD Studio 11 are failing due to a license error.  Use the "Update..." option in the License Manager to fix.'
---
## License Fix
[**RAD Studio 11**](https://www.embarcadero.com/products/rad-studio/whats-new-in-11-alexandria) was just released today! For some users, you may have trouble getting RAD Studio installed due to license failures. One way of solving this issue was [**posted today on Twitter**](https://twitter.com/marcocantu/status/1436032961808326660) by **Marco Cantu**.

His directions were simple:

> To anyone trying to install RAD Studio 11, there is a hiccup on license refresh: In the Product Registration window please select the Advanced button, and in the License manager select your license and use the Update button. Alexandria gets added, continue with installation.

Below is a screen shot of the License Manager screen. Simply click on the **Update...** option.

![Update fix for RAD Studio license problem screenshot](/assets/blog/RAD-Studio-11-Alexandria/RAD-Studio-11-License-Manager-Fix.png)

_Note: I am using a named network license, so the 'Update' option is disabled for me in the image above, but it should be enabled for most users._

The install is currently failing for me when using a network named license (via the 'Import' option above on a new machine.) I'll update this blog post whenever I get my copy of RAD Studio 11 installed. Send me a note on Twitter or LinkedIn if you get a solution before I do!

## Update 1
A simple work-around for some is to simply Delete and add back the existing license in License Manager. This is known to have worked for some users:

```
In the Product Registration window please select the Advanced button
- The Embarcadero License Manager will Launch
- In the License Manager please select your license from the list
- Delete license
- Click on Register, register with your serial number
- Click Done and this will continue the installation
```

_You may want to manually remove the C:\\ProgramData\\Embarcadero folder as well._

## Update 2
Thanks to a suggestion from **Uwe Raabe**, I finally did get my network license to work correctly. I had to re-host the license on the ELC server by using the _manual method_. On the **License Hosting** screen in your ELC server instance, instead of using the "Host Licenses" button, use the "Web based License Hosting" option as pointed out in the image below. After clicking this link, you will need to logon using the credentials as originally provided in your network license certificate email _and not your normal EDN credentials_. (Note: you may want to change the protocol to HTTPS as it is using HTTP by default.) Once logged on, you will be presented with a list of the license certificates that have been issed to your account. Click the 'Host' link on your desired certificate and confirm the next two pages and download your license .zip file. (Note: follow the readme in the .zip and copy the file to the _conf_ folder of your ELC server install. I also extracted the contents of the .zip to the _conf_ folder...in my case, the full path for this conf folder is: **C:\\Program Files\\Embarcadero\\ELC5.33\\LicenseCenter\\conf**)

Using the License Manager, I imported the .slip file from this manually downloaded .zip file on my workstation with success. (The .slip file from the .zip created from "Host Licenses" button just failed to work for me.)

![ELC Network License Hosting screenshot](/assets/blog/RAD-Studio-11-Alexandria/Network-License-Failing-ELC.png)

## Update #3
For isolated or offline machines you will need to obtain a new activation file from [https://license.embarcadero.com/srs6/activation.do](https://license.embarcadero.com/srs6/activation.do)

````
Go to: C:\ProgramData\Embarcadero
Remove license files “.slip and .txt files” do not delete the whole folder
Place new file you have downloaded
launch product and it should open up all the way
````

## Update #4
For users having a problem with mobile platforms not being activated for users who have "Delphi Professional plus Mobile" licenses, this is what reportedly worked for one user, **Dave Nottage**:

````
Go to: C:\ProgramData\Embarcadero
Please note c:\ProgramData is a hidden folder, you need to go to folder options under the view tab, and check on show hidden files and folders.
Remove license files “.slip and .txt files” do not delete the whole folder
Launch product and register your serial number as directed

if RAD Studio 11 is already installed, also need to:
  Tools|Manage Platforms
  Ensure that the relevant platforms are selected
  Click Apply
  Wait for confirmation
  Restart Delphi
````