---
layout: post
title: "Recent system outages on Embarcadero websites"
date: 2024-01-16 12:00:00 +0000
last_modified_at: 2024-01-17 12:00:00 +0000
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


For future reference, there is a status page that you can refer to verify the uptime of their web services: [https://stats.uptimerobot.com/3yP3quwNW](https://stats.uptimerobot.com/3yP3quwNW)  This was put online by the veritable **Jeroen Wiert Pluimers** as posted on Twitter back in 2022.  

[![Uptime Monitoring of Embarcadero Sites](/assets/blog/Embarcadero/Jeroen-Twitter-Post.png)](https://twitter.com/jpluimers/status/1495669278519046148)


Look for an official announcement on Embarcadero's [blog site](https://blogs.embarcadero.com) or check our Telegram group for any further updates.

![Connection Failure](/assets/blog/Embarcadero/Connection-Failure.png)

## UPDATE 1
`January 15, 2024`

**Ian Barker** posted an update on [Discord](https://discord.com/channels/623794270255579146/623835476037730304/1196456421215187006): "There is currently an outage affecting several key services including the GetIt package manager, DocWiki, and several other key systems.

The outage is caused by a central piece of hardware failing (some kind of controller). It is being worked on with a replacement on the way. No ETA but everyone who could possibly be involved at the highest levels are dealing with it and get it all back online ASAP."

## UPDATE 2
`January 17, 2024`

**Ian Barker** released a blog post with some details about the outage:
* [Some Embarcadero Servers Are Experiencing A Hardware Outage](https://blogs.embarcadero.com/we-are-experiencing-a-hardware-outage/)

## Update 3
`January 20, 2024`

**Ian Barker** announced on Slack that Embarcadero has put some packages up on [my.embarcadero.com](https://my.embarcadero.com) so that people with an update subscription can download them and install then is the GetIt "install local packages" function.

## Update 4
`January 21, 2024`

**Uwe Raabe** posted some [instructions](https://en.delphipraxis.net/topic/10835-fyi-several-embarcadero-services-are-currently-unavailable/?do=findComment&comment=86688) on the Delphi-PRAXiS forums on how to reinstall help from the ISO while the network outage continues:

"If you did not install from the ISO in the first place or have already changed to online mode before, open a command prompt in the Delphi bin folder and execute 

`getitcmd c=useoffline`

You can as well change the *SeviceKind* setting to *Offline* in the registry in *HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\23.0\CatalogRepository* (for Delphi 12). There you can specify the path to the gof file from the ISO under *ServicePath*.

Up to here, all this is to tell the *Feature Manager* where to look for the installation files. 

Then start the IDE and select *Tools - Manage Features*. There you can select *Help* and install it."


---
## Current status
`January 21, 2024 18:40 UTC`

Based on a manual check, it appears that the main servers are all online except [DocWiki](https://docwiki.embarcadero.com/) and GetIt.


![GetIt Server Outage](/assets/blog/Embarcadero/GetIt-Outage.png)