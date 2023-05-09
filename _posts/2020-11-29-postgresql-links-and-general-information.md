---
layout: post
title: "PostgreSQL links and general information"
date: 2020-11-29 12:00:00 +0000
last_modified_at: 2020-11-29 12:00:00 +0000
categories: [Tooling, Database]
tags: [PostgreSQL, FireDAC, DevArt, Microolap, ZeosLib, Synopse]
permalink: post/postgresql-links-and-general-information
published: true
image: /assets/blog/Database/PostgreSQL/PostgreSQL-Logo.png
description: PostgreSQL distributions, support, and other information.
---
I have used PostgreSQL sporadically over the years, but only for small projects with short lifespans. Another such project is being started and this time I thought I would gather some information together on PostgreSQL and post it on the blog for future reference.

![PostgreSQL Logo](/assets/blog/Database/PostgreSQL/PostgreSQL-Logo.png)

## General Info

[**PostgreSQL**](https://www.postgresql.org/) is an extremely popular open source object-relational database system and it claims to be "**The world's most advanced open source database**." It has been around for over three decades and is still in active development. In fact, with the emphasis on cloud migrations going on and the wide support of PostgreSQL on all the major cloud platforms, this might be the zenith of PostgreSQL usage. It is typically rated as being in the Top Five of most popular database management systems in use with the other four being Oracle, Microsoft SQL Server, and MySQL (SQLite is the most widely utilized embedded RDMS in the world.)

The latest version 13.1 was [**released**](https://www.postgresql.org/docs/release/) on November 12, 2020. The database is available on multiple platforms, including Microsoft Windows (for Windows XP and later) with a [**separate FAQ**](https://wiki.postgresql.org/wiki/Running_%26_Installing_PostgreSQL_On_Native_Windows) available just for this platform.

## How Do You Pronounce PostgreSQL?

I have learned today that I have been mis-pronouncing it for years. There is an [**audio file available**](https://www.postgresql.org/files/postgresql.mp3) that demonstrates the proper English pronunciation. (Postgres-Q-L)

## Which Distribution To Use?

There are a few different variations available, some are open source, some are commercially licensed.

-   [**Download**](https://www.postgresql.org/download/) the latest version directly from the main open source project website (postgresql.org.) You can select from Linux, macOS, Windows, Solaris, BSD. For Microsoft Windows you can select from an interactive installer created by EDB, or simply download the [**zip archive**](https://www.enterprisedb.com/download-postgresql-binaries) of the binaries from the EDB website.
    
-   [**EDB**](https://www.enterprisedb.com/) offers a slightly customized installer for the open source version on their [**download page**](https://www.enterprisedb.com/downloads/postgresql). They also offer an EDB Postgres Advanced Server, various support options, and a variety of tools for managing PostgreSQL.
    
-   [**2ndQuadrant**](https://www.2ndquadrant.com/) offers a certified installer for the open source version, and they also offer commercially supported versions with advanced features. The latest digitally signed installer for the open source version is available [**here**](https://www.2ndquadrant.com/en/resources/postgresql-installer-2ndquadrant/). This installer supports fully interactive, command-line, or unattended mode operations.
    
-   [**Crunchy Data**](https://www.crunchydata.com/) offers various enhanced commercial versions and support.
    
-   [**Cybertec**](https://www.cybertec-postgresql.com/) offers the typical support services (administration, consulting design, development, etc) as well as their own Enterprise Edition along with tools and add-ons for PostgreSQL.
    
-   [**Fujitsu Enterprise Postgres**](https://www.postgresql.fastware.com/) is another commercial distribution with advanced features supported by a large company. Note that as of August 2020, **Amit Kapila** (one of the major contributors to PostgreSQL) has joined Fujitsu, leading their PostgreSQL team as Senior Director. See this [**blog post**](https://www.postgresql.fastware.com/blog/meet-amit-kapila-fujitsu-postgresql-team-senior-director) for details.
    
-   [**HighGo**](https://www.highgo.ca/) offers their own enhanced PostgreSQL Server with Parallel Backup, Shard Management, and enhancements to Partitioning.
    
-   [**PostgresPro**](https://postgrespro.com/) is another commercial distribution to possibly consider with Standard, Enterprise and Cloud based options all containing advanced features. This is a smaller Russian company with some PostgreSQL contributors that have developed custom extensions and features. Also see their [**GitHub page**](https://github.com/postgrespro) for more info.
    

Many other extensions are available, including those from [**PostGIS**](http://www.postgis.net/) and [**Citus**](https://www.citusdata.com/).

## Open Source License

The open source version is licensed under a unique [**PostgreSQL License**](https://www.postgresql.org/about/licence/) which is similar to the commonly utilized MIT license. Simply put: do what you want and realize that there is no warranty.

## Commercial Support

If you are leveraging the open source version, you may still want to get support. Other than the various commercial options listed above, likely the most popular dedicated support option is from [**Command Prompt, Inc.**](https://www.commandprompt.com/) They have been commercially supporting PostgreSQL longer than anyone, dating back to 1997.

Another popular support option is [**PGExperts**](https://pgexperts.com/), a North American company dedicated to PostgreSQL support.

A company dedicated to PostgreSQL support is [**Data Egret**](https://dataegret.com/) who would like to be your _Remote PostgreSQL DBA Team_. They offer 24/7 support, consulting, and training services.

## PostgreSQL Connectivity Options For Delphi

There are a number of options to connect to PostgreSQL from Delphi.

-   [**FireDAC**](http://docwiki.embarcadero.com/RADStudio/en/Connect_to_PostgreSQL_(FireDAC)) comes with the Enterprise Edition of Delphi but relies on external DLLs
    
-   **Devart** offers a [**PostgreSQL DAC**](https://www.devart.com/pgdac/) with either direct mode support or can rely on external DLLs with a Standard license costing $200/year. They also offer [**UniDAC**](https://www.devart.com/dac.html) to connect to most popular databases with a Standard license at $300/year.
    
-   **Microolap** offers their [**PostgresDAC**](https://www.microolap.com/products/connectivity/postgresdac/) for connectivity. A personal license costs $135, Business license (internal use) cost $300 and a Commercial license (distribution rights) for $400 per developer. (Windows deployment includes: libpq.dll, libintl-8.dll, ssleay32.dll, libeay32.dll)
    
-   [**ZeosLib**](https://sourceforge.net/projects/zeoslib/) is a set of open source database components for Delphi to access various databases including PostgreSQL, MySQL, MariaDB, Interbase, Firebird, MS SQL Server and others. (Requires libpq.dll and libintl.dll)
    
-   The open source [**Synopse mORMot Framework**](https://synopse.info/fossil/wiki/Synopse+OpenSource) offers various types of connectivity. (OleDB, ODBC, and ZeosLib)
    
-   You can also utilize ODBC connectivity via dbExpress and ADO/dbGO. (DevArt also offers dbExpress components.) Refer to the official [**psqlODBC driver**](https://odbc.postgresql.org/).
    

## More Information

-   A highly used blog aggregator is available on [**planet.postgresql.org**](https://planet.postgresql.org/) with plenty of articles to read.
    
-   Fujitsu recently started an [**PostgreSQL Insider**](https://www.postgresql.fastware.com/postgresql-insider) section dedicated to enthusiasts.
    
-   A recent [**blog post from November 23, 2020**](https://www.postgresql.fastware.com/blog/the-future-of-postgresql) from **Amit Kapila** covers some future versions of PostgreSQL 14 and beyond.
    
-   There are currently 9,600 users on the [**Postgres**](https://postgres-slack.herokuapp.com/) slack channel.
    
-   There are currently 10,400 members of the [**PostgreSQL Server**](https://www.facebook.com/groups/postgres/) group on Facebook.
    
-   There are currently 13,200 members of the [**PostgreSQL Professionals**](https://www.linkedin.com/groups/51776/) group on LinkedIn.
    
-   The official Twitter account, [**postgresql**](https://twitter.com/postgresql) currently has 21,100 followers.
    
-   Blog entries from members of the PostgreSQL community are on the [**planetpostgres**](https://twitter.com/planetpostgres) Twitter account currently with 23,200 followers.
    

## Note on EDB and 2ndQuadrant

Two of the largest companies supporting PostgreSQL have recently merged with the purchase of 2ndQuadrant by EDB. See the [**acquisition announcement**](https://www.enterprisedb.com/news/edb-completes-acquisition-2ndquadrant-becomes-largest-dedicated-provider-postgresql-products) from September 30, 2020 on EDB's website. Their announcement includes the typical positive flare such as "_The acquisition will integrate two companies who are major contributors to PostgreSQL. The new unified roster of experts brings a long history of building and optimizing PostgreSQL for the most demanding global organizations_."

I may do follow-up posts on installing and utilizing PostgreSQL with Delphi on the Windows platform. If I missed something major in this post, please let me know!