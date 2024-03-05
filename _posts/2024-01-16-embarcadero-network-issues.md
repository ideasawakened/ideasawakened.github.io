---
layout: post
title: "Recent system outages on Embarcadero websites"
date: 2024-01-16 12:00:00 +0000
last_modified_at: 2024-01-22 23:50:00 +0000
categories: [Community]
tags: [Embarcadero, Ian Barker, Jeroen Wiert Pluimers, Marco Cantu]
permalink: post/embarcadero-network-issues
published: true
image: /assets/blog/Embarcadero/ConnectionFailure-small.png
description: 'Quick post covering the recent issues connecting to Embarcadero websites'
---

There has been persistent connectivity issues recently to Embarcadero websites including their [DocWiki](https://docwiki.embarcadero.com), [Quality Portal](https://quality.embarcadero.com/) and others.  The issue is being actively discussed in multiple channels including on Telegram and Facebook.  There are apparently some underlying infrastructure issues that they have been chasing and you should look for an official announcement soon.

**Ian Barker** posted a quick update on Telegram this morning
[![Update on outage](/assets/blog/Embarcadero/IanBarker-Update.png)](https://t.me/delphidevelopers/35711)


For future reference, there is a status page that you can refer to verify the uptime of their web services. This was put online by the veritable **Jeroen Wiert Pluimers** as posted on Twitter back in 2022.  

[![Uptime Monitoring of Embarcadero Sites](/assets/blog/Embarcadero/Jeroen-Twitter-Post.png)](https://twitter.com/jpluimers/status/1495669278519046148)


Look for an official announcement on Embarcadero's [blog site](https://blogs.embarcadero.com) or check our Telegram group for any further updates.

![Connection Failure](/assets/blog/Embarcadero/Connection-Failure.png)

## UPDATE 1
`January 17, 2024`

**Ian Barker** released a blog post with some details about the outage:
* [Some Embarcadero Servers Are Experiencing A Hardware Outage](https://blogs.embarcadero.com/we-are-experiencing-a-hardware-outage/)

## Update 2
`January 20, 2024`

**Ian Barker** announced on Slack that Embarcadero has put some packages up on [my.embarcadero.com](https://my.embarcadero.com) so that people with an update subscription can download them and install then is the GetIt "install local packages" function.

## Update 3
`January 21, 2024`

**Uwe Raabe** posted some [instructions](https://en.delphipraxis.net/topic/10835-fyi-several-embarcadero-services-are-currently-unavailable/?do=findComment&comment=86688) on the Delphi-PRAXiS forums on how to reinstall help from the ISO while the network outage continues:

"If you did not install from the ISO in the first place or have already changed to online mode before, open a command prompt in the Delphi bin folder and execute 

`getitcmd c=useoffline`

You can as well change the *SeviceKind* setting to *Offline* in the registry in *HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\23.0\CatalogRepository* (for Delphi 12). There you can specify the path to the gof file from the ISO under *ServicePath*.

Up to here, all this is to tell the *Feature Manager* where to look for the installation files. 

Then start the IDE and select *Tools - Manage Features*. There you can select *Help* and install it."

## Update 4
`January 22, 2024`

I noticed on StackOverflow today that a question was asked on January 7th: [GetIt failed to load](https://stackoverflow.com/questions/77772473/unable-to-load-url-https-getit-12-embarcadero-com-when-trying-to-load-getit-in) so the troubles possibly started about a week before the major outage began.

I added a new status page to track uptime of Delphi related services without all the old servers listed on Jeroen's page (and also so I would get notified when an outage is detected.)  It's a free service provided by **Uptime Robot**: [https://stats.uptimerobot.com/zJjvZI8vEZ](https://stats.uptimerobot.com/zJjvZI8vEZ)

Embarcadero staff might be getting a little stressed about this outage by now.  Those responsible for server management are in a different group so the front-line Embarcadero staff are bearing the blunt of the communication from customers around the globe while the IT staff is insulated.  The week+ long outage in late 2022 didn't result in major improvements...I wonder if the week+ long outage in early 2024 will?  
- Reference ticket for 2022 [RSP-39878](https://quality.embarcadero.com/browse/RSP-39878)
- Reference ticket for 2024 [RSP-44128](https://quality.embarcadero.com/browse/RSP-44128)


## Update 5
`January 22, 2024`

**Ian Barker** posted a [reply](https://en.delphipraxis.net/topic/10835-fyi-several-embarcadero-services-are-currently-unavailable/?do=findComment&comment=86729) on the Delphi PRAXiS forums that the event is definitely hardware related and involves more than one server.  "it was a cascade of events and *multiple* affected servers and hardware."  


## Update 6
`January 23, 2024`

The [DocWiki](https://docwiki.embarcadero.com/) is back online as of 16:40 UTC.  GetIt still seems to be having troubles.

## Update 7
`January 25, 2024`

The [official blog post](https://blogs.embarcadero.com/we-are-experiencing-a-hardware-outage/) was updated this morning. 

"The Embarcadero IT team is still working on restoring and migrating some of the systems that have been affected by this hardware outage and we’d like to provide you an update:

- The DocWiki site has been restored and should now be working fine, see https://docwiki.embarcadero.com/RADStudio/Athens/en/What%27s_New 
- We have also added two downloads for Local GetIt packages on my.embarcadero.com, with a small number of GetIt packages, including IDE extension, core components, and FMXLinux. This is the same we did in the past, see https://blogs.embarcadero.com/embarcadero-getit-packages-download-for-rad-studio-11-2/ .
- The Quality Portal site, which was only partially affected, has been back to regular work for a few days. 
- The GetIt site (also used for the product installation) is still affected by the outage. The team is not only working on restoring it, but also on migrating it to a new infrastructure.
- The ISO image for installing RAD Studio 12.0 is available at https://altd.embarcadero.com/download/radstudio/12.0/RADStudio_12_0_4915718.iso "


## Update 8
`January 26, 2024`

DocWiki is down again and Quality Portal is online but with the message
````
Attention: Embarcadero is migrating its customer bug and feature request 
reporting portal to a new system. This site will remain accessible as 
a read only repository. We'll shortly provide information on how to 
access and report bugs in the new portal.
````
So it looks like we're getting some new servers online soon!  **Huzzah!**

DocWiki was reported down at `2024-01-26 15:48` UTC


## Update 9
`January 27, 2024`

A new blog post is available from Embarcadero - [Quality Portal Migration](https://blogs.embarcadero.com/embarcadero-quality-portal-migration/)

Key information:

````
 The plan is to have a new public bug reporting and feature request
 portal for RAD Studio using the Atlassian JSM (Jira Service Manager)
 front end, working in conjunction with the JIRA system used by R&D. 
 The new portal will continue to serve the purpose of allowing customers 
 to log issues and make requests, even if it will offer a different 
 user experience. In addition, it will have the advantage of being 
 a standard, stable, and fully integrated system.

 For the time being, the existing Quality Portal system will remain 
 accessible read only. The data on the old system will remain visible, 
 even after the new portal will be in place.

 This migration has been planned for a few months and the expected 
 timeline is now. Quality Portal will shortly become read only. 
 By next week, we’ll have the new portal in place and we’ll have 
 a new blog post explaining how to use it and providing 
 additional information.
````

Also, the DocWiki site is back online earlier this morning (so they seem to be working over the weekend to attack this ongoing problem.)

---

## Update 10
`January 30, 2024`

Some systems were down for a short while this morning, presumedly being migrated to a new backend.  DocWiki remains online and it feels faster than ever.  (It has the 'new server feel')  However, GetIt remains unusable.

---
## Update 11
`February 4, 2024`

DocWiki has been down all morning so they are hopefully doing some server shuffling today.  The optimist side of me hopes to see an announcement later today, or perhaps on Monday.



**Note:** You can edit the layout of the Welcome Page and remove the GetIt panels ("New in GetIt" and "Promoted in GetIt") to prevent the excess load on their servers and the lag on your system during this outage.
![GetIt Server Outage](/assets/blog/Embarcadero/GetIt-Outage.png)

---
## Update 12
`February 7, 2024`

Good news, and more good news... [GetIt is now online](https://blogs.embarcadero.com/getit-update-additional-rad-studio-12-getit-packages-are-now-available/)!
(At least for RAD Studio 12 users.)

- And the patch for RAD Studio 12 is [now available in GetIt](https://blogs.embarcadero.com/rad-studio-12-athens-patch-1-available/)
- This patch will be merged with 12.0 to replace the original install with a new 'inline release'
- The patch will 'soon' be available for manual download in https://my.embarcadero.com 
- Their hardware outage blog post was updated today to include this note:
````
Starting today, the majority of the GetIt add-on packages that were available on 
the previous system have been restored and made available. There is still 
ongoing work to provide some missing packages, which we are aiming to
complete by this week.
````

The bad news:
- Quality Portal remains in read-only mode  (their [Quality Portal Migration](https://blogs.embarcadero.com/embarcadero-quality-portal-migration/) blog post on January 27th stated `"By next week, we’ll have the new portal in place and we’ll have a new blog post explaining how to use it and providing additional information."`)

- GetIt is only available for 12.0.  Older versions, including Community Edition coming online later (so a new Community Edition user cannot get a version installed until more progress is made.)


## Update 13
`February 9, 2024`

A [new blog post](https://blogs.embarcadero.com/getit-update-rad-studio-12-getit-online-installation-is-now-available/) from Embarcadero states that a new inline release is available.  The trial installation and regular product installation are now based on this new inline release with version number **29.0.51511.6924**

````
Customers on the original RTM version 
of 12.0 (build version 29.0.50491.5718)
can continue using it, can install 
Patch 1, and can use GetIt packages:

If they used the online installation, 
they cannot add new platforms or 
features to the product. To do so, 
customers will have to reinstall the 
product using the online build (6924) 
or use the offline (ISO) installer 
for the RTM build (5718).

If they used the offline (ISO) installer,
this allows adding platforms and features.  

The new build (6924) is functionally 
equivalent to the RTM build (5718)
with Patch 1 installed. There is no 
reason to move from one to the other 
unless you need to install additional 
target platforms.
````

## Update 14
`February 10, 2024`

The Community Edition has not been available for new users since this outage began 27 days ago.  Embarcadero has recently updated the download page with the notification `Download temporarily unavailable, we apologize for the inconvenience. If you are a student or teacher and need to download the Community Edition for a scheduled class, please Contact Sales.`

I suppose it is always *better late, than never...* However, I assume that likely means there are a few more days (at least) to go before this outage event is over otherwise they would not have bothered to update this page.


## Update 15
`February 20, 2024`

It seems that as of this afternoon, the `getit-olympus.embarcadero.com` server is back online!  The new status page is proudly announcing:  **All systems operational**

![All Systems Operational](/assets/blog/Embarcadero/All_Systems_Operational.png)

Once confirmed, this means the only item left is the migration of Quality Portal to a new system.  The current system ticket system remains in Read-Only mode.  **Marco Cantu** recently left a comment the [Quality Portal migration blog page](https://blogs.embarcadero.com/embarcadero-quality-portal-migration/) on Feb 16th: **"For critical bug reports, please contact Embarcadero support"**   The target migration completion was as late as February 3rd, so that project is a few weeks behind schedule.

## Update 16
`February 28, 2024`

Correction to Update 15 - there is one more big item left regarding the server outage and that is the GetIt server for 10.4 which is still offline. If you are still using that release (versions 10.4, 10.4.1, and 10.4.2) then GetIt still fails to load.  (Perhaps it's a good time to upgrade?)  After verifying today with Embarcadero, there is currently no target date for that to be back online.

Quality Portal is still in read-only mode and no further updates on its replacement has yet been released.

## Update 17
`March 4, 2024`

I have not been able to logon to Quality Portal for the last two days and I believe I saw a reference from another developer that could not logon on March 1 so it has likely been down for the last three days.  The server status page shows that it is online as the logon page does display but you receive the logon error: `Sorry, an error occurred trying to log you in - please try again.` and you simply cannot access the content.

Quality Portal has been in "Read Only mode" since January 26th and now is simply unusable and no updates on their [Quality Portal Migration blog page](https://blogs.embarcadero.com/embarcadero-quality-portal-migration/) since a comment made by Marco on Feb 16th that suggested accessing Support for any bugs.

Maybe the IT Operations people at Idera (who runs these servers) actually believe the rumor that "Delphi is Dead."  Or maybe they have all their servers busy mining Bitcoin as its price keeps going up...(which is currently at the max of $68,230 which is even more depressing than this server fiasco as I had money in Bitcoin many years ago that I should have left invested.  I cannot help but do the math at the amount of money it would be worth today.)
