---
layout: post
title: "PowerShell script to install PostgreSQL on Windows using the binary release zipfile"
date: 2024-02-03 12:00:00 +0000
last_modified_at: 2024-02-03 12:00:00 +0000
categories: [Tooling, Database]
tags: [PostgreSQL, Microsoft Windows, PowerShell, Build Server, Automation]
permalink: post/2024-02-03-setup-postgresql-on-windows-from-zip
published: true
image: /assets/blog/Database/PostgreSQL/PostgreSQL-Powershell-small.png
description: 'PowerShell script to install PostgreSQL on Windows from the latest binary release zipfile'
---
PostgreSQL is one of the [most popular database solutions](https://www.statista.com/statistics/809750/worldwide-popularity-ranking-database-management-systems/) in the world.  A few years back, I published a blog post full of [PostgreSQL links and general information](/post/postgresql-links-and-general-information) which includes links for the various distributions that are available and some connectivity options for [Delphi](https://www.embarcadero.com/products/delphi).  Shortly before that article was released, EDB and 2ndQuadrant merged and since that time EDB seems to be the only choice for pre-built community edition installers for Postgres on Windows (2ndQuadrant previously provided their own custom builds. EDB offers a download page for [installers](https://www.enterprisedb.com/downloads/postgres-postgresql-downloads) and also a page for [release zips of the binaries](https://www.enterprisedb.com/download-postgresql-binaries).  This post focuses on installing PostgreSQL from a release zip.

When given a choice, I prefer to utilize zip archives versus a full installer as you can typically customize the install a little more.  In addition, the release zips are typically smaller than the full installer.  In this particular case, the release zip only 10% smaller and that is certainly not as big of an issue as it used to be.  However, a much larger difference involves the speed of installation.  The Windows Installer is slow and can take around 3.5 to 4 minutes to complete while the PowerShell script can be finished in about 20 seconds on the same machine.  You can even cut that 20 second install time in half if you do not install pgAdmin.  The fancy progress bar provided by the Windows Installer is simply not worth that much of a slowdown to me.  There is an unattended/silent mode option available for the full installer, but that takes the same amount of time to complete.  (For reference, the unattended mode uses the switches: `--mode unattended --unattendedmodeui none`)

Excluding pgAdmin, a fresh installation of Postgres takes about 160MB of drive space. That gives you an incredibly powerful tool with very little drive space along with a 10 second installation that can be easily automated.

Years ago, I wrote Batch files to do the Postgres installation work, but recently converted to PowerShell for a few reasons.  One reason is that the [setx](https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/setx) command which is commonly used in batch files on Windows has a 1024 character limit and its usage can lead to silently truncating your system PATH. The length of my PATH on the server I was installing on was getting a little long and I started worrying about eventual truncation.  

Another reason is there is simply little excuse left not to use PowerShell instead of Batch files.  One big reason was that you had to first install PowerShell to run scripts as it was not pre-installed on Windows.  Well, I have heard that Windows XP died a very long time ago and PowerShell has been natively installed on pretty much every Windows version since.  So - you should really start using PowerShell instead of batch files.  

And even better, PowerShell is now cross platform and can be [installed on Linux](https://learn.microsoft.com/en-us/powershell/scripting/install/installing-powershell-on-linux?view=powershell-7.2). In fact, you can get PowerShell packages for Windows, Ubuntu, Debian, CentOS, Red Hat Enterprise Linux, openSUSE, Fedora, and even macOS from their [GitHub page](https://github.com/PowerShell/PowerShell).

One feature that I do not like about PowerShell is [Telemtry](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_telemetry?view=powershell-7.4) but, thankfully, you can easily opt-out.  

I have uploaded my [Install PostgreSQL on Windows](https://github.com/darianmiller/install-postgres-on-windows) PowerShell script to GitHub and will cover the basics here:


# Parameters

Note: you need to provide at least one parameter as you must either specify the `ArchiveFileName` or the `DownloadLatest` switch.

## ArchiveFileName
This string parameter is the release zip to install (as previously downloaded from EDB.)  An example of the current latest release zip is `postgresql-16.1-1-windows-x64-binaries.zip` which is 330MB in size.

## DestinationPath
This optional string parameter is the path to unzip the binaries into and it defaults to `C:\Postgres` if not provided. In a full installation, there will be three child folders created: Admin, Data, and Server

## ServiceName
This optional string parameter is the name of the new Windows Service created.  If not provided, the ServiceName defaults to `Postgres`


## ListenPort
If specified, this optional integer parameter will override the default TCP port the server listens on which is [5432 by default](https://www.postgresql.org/docs/current/runtime-config-connection.html) as set in `postgresql.conf`

## DownloadLatest
If specified, this optional switch parameter overrides the ArchiveFileName and the latest release zip will be downloaded from EDB's website and installed.  Unfortunately there does not seem to be an API available for this, so the script currently resorts to web scraping (and may be prone to breakage if they alter the format of their site.)  The script will parse the files found on the [download-postgresql-binaries](https://www.enterprisedb.com/download-postgresql-binaries) page and pick the first one (as it will be the latest Windows installer.)  The file will be saved as `Postgres-Windows-Binaries.zip` within the TEMP folder and deleted once the installation is completed.

## UpdatePath
If this optional switch is specified, the script will update the PATH with the Postgres\bin folder (useful if you are running command line tools like psql.exe.)

## InstallPGAdmin
If this optional switch is specified, the script will create an Admin child folder and unzip the [pgAdmin 4](https://www.pgadmin.org/download/pgadmin-4-windows/) files.  Including this switch adds about 760MB worth of space and increases the installation time.  This is useful if you do not have pgAdmin or another tool like [HeidiSQL](https://www.heidisql.com/) already installed.

# Custom Settings

I have added a few common parameters to configure the instance, and more may be added in the future.  But, for now, these following settings were used for [initdb](https://www.postgresql.org/docs/current/app-initdb.html) during the initial installation process:

- The default [encoding](https://www.postgresql.org/docs/current/multibyte.html#MULTIBYTE-CHARSET-SUPPORTED) is set to `UTF8` with a [locale](https://www.postgresql.org/docs/current/locale.html) of `en_US.UTF-8`
- The folder used for the [database cluster](https://www.postgresql.org/docs/current/glossary.html#GLOSSARY-DB-CLUSTER) is set to `DestinationPath`\Data
- The new Windows Service created is set to `Automatic (Delayed Startup)` and the windows service is started during the installation process.
- The [bootstrap superuser](https://www.postgresql.org/docs/current/glossary.html#GLOSSARY-BOOTSTRAP-SUPERUSER) is named `postgres` and has `postgres` as its password
- The default [time zone](https://www.postgresql.org/docs/current/datatype-datetime.html#DATATYPE-TIMEZONES) is set to `UTC`

All settings not set by parameters, or listed above in Custom Settings, will utilize the Postgres default values, such as:
- The default [authentication method](https://www.postgresql.org/docs/current/auth-methods.html) is `trust` which is meant for local workstations, not multi-user servers.
- The default [max_connections](https://www.postgresql.org/docs/current/runtime-config-connection.html#GUC-MAX-CONNECTIONS) is set automatically by initdb and is typically set to `100`.

# Summary
This is a single PowerShell script file with no dependencies.  You can simply run the script with the DownloadLatest switch (`Install-Postgres-On-Windows.ps1 -DownloadLatest`) and the latest release zip will be downloaded, then automatically installed to `C:\Postgres`, and you will end up with a new Windows Service named `Postgres` actively running and accepting all local connections on port 5432.  Alternately, manually download the latest release zip and provide it via the ArchiveFileName parameter. (`Install-Postgres-On-Windows.ps1 -ArchiveFileName postgresql-16.1-1-windows-x64-binaries.zip`)

If you have a repetitive task for getting a PostgreSQL service up and running, I would suggest manually downloading and customizing the release zip by extracting the `bin`, `lib` and `share` folders from within the full EDB provided binaries archive file and then simply zip them backup into a much smaller release zip of about 40MB (instead of the full 330MB) and use that instead.  (Or, [build your own binaries](https://www.postgresql.org/docs/current/install-windows.html) directly from the sources if you are adventurous which I might try one day if I have some free time.)

In short, you could easily have a release zip installed and running on Windows in under 10 seconds for one of the most powerful and widely used RDMS in the world...not bad!

A future blog post will introduce PowerShell scripts for easily backing up and restoring Postgres databases.

![PostgreSQL with PowerShell logo](/assets/blog/Database/PostgreSQL/PostgreSQL-Powershell.png)