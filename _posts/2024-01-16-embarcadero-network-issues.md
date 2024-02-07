---
layout: post
title: "Recent system outages on Embarcadero websites"
date: 2024-01-16 12:00:00 +0000
last_modified_at: 2024-01-22 23:50:00 +0000
categories: [Community]
tags: [Embarcadero, Ian Barker, Jeroen Wiert Pluimers]
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

- And the patch for RAD Studio 12 is [now available](https://blogs.embarcadero.com/rad-studio-12-athens-patch-1-available/)
- This patch will be merged with 12.0 to replace the original install with a new 'inline release'

The bad news:
- Quality Portal remains in read-only mode
- GetIt is only available for 12.0  (Older versions, including Community Edition coming online later)



