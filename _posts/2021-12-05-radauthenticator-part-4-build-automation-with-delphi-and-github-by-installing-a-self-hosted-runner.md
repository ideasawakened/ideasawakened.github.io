---
layout: post
title: "RADAuthenticator Part 4 - Build Automation with Delphi and GitHub by Installing a Self-hosted Runner"
date: 2021-12-05 12:00:00 +0000
last_modified_at: 2021-12-05 12:00:00 +0000
categories: [Tooling, Automation]
tags: [rad-authenticator, GitHub, Continuous Integration, Microsoft Windows, Undocumented, Automation]
permalink: post/radauthenticator-part-4-build-automation-with-delphi-and-github-by-installing-a-self-hosted-runner
published: true
image: /assets/blog/Continuous-Integration/GitHub-Runners/Delphi-And-GitHub-Self-Hosted-Runner.png
description: "Install a GitHub self-hosted runner in order to trigger local Delphi builds based on GitHub action workflows."
---
It the first two parts of this [**blog series**](https://www.ideasawakened.com/tags/rad-authenticator), we covered some underlying technical code - first on [**base32 encoding**](https://www.ideasawakened.com/post/base32-encoding-in-delphi-for-google-authenticator-replacement-app-part-one-in-series) and then in part-two we covered [**TOTP for one-time use password generation**](https://www.ideasawakened.com/post/radauthenticator-part-2-generate-one-time-password-tokens-in-delphi-using-totp). In [**part 3**](https://www.ideasawakened.com/post/radauthenticator-part-3-upgrade-unit-testing-in-delphi-with-testinsight), we covered some enhanced tooling and introduced **TestInsight** to expand our unit test capabilities within RAD Studio.

In part 4, we will stay focused on improved tooling and get set up to allow GitHub actions to trigger Delphi builds. GitHub has many built-in capabilities available to other mainstream developers through the use of [**GitHub Actions**](https://docs.github.com/en/actions) with custom workflows performed on cloud-based virtual hosts running Linux, Windows and macOS operating systems in either Docker containers or directly on virtual machines hosted in Microsoft Azure. These virtual systems come with a lot of [**pre-installed software**](https://github.com/actions/virtual-environments/blob/main/images/win/Windows2019-Readme.md) to help you with the most common build actions. This includes Go, Node, PHP, Chocolatey, NuGet, Ant, Maven, Docker, Git, SVN, InnoSetup, OpenSSL, Java, PostgreSQL, Apache, Nginx, Visual Studio (with a bunch of components and extensions), along with a whole bunch of other well-known developer tools. Due to licensing and other requirements, one important item that you may not ever see on that list of pre-installed software is RAD Studio. And due to RAD Studio not being on GitHub's list of pre-installed software on their build machines, many developers (and managers) believe that Delphi is not able to play with the 'cool kids' on GitHub. That is simply not true - and this blog article will show you how to get started in five minutes or less.

![Use Delphi with GitHub's self-hosted runner on Windows](/assets/blog/Continuous-Integration/GitHub-Runners/Delphi-And-GitHub-Self-Hosted-Runner.png)

## GitHub Self-Hosted Runner Installation

Currently, the best option available for using GitHub actions with Delphi is to deploy a [**GitHub self-hosted runner**](https://docs.github.com/en/actions/hosting-your-own-runners/about-self-hosted-runners) on one (or more) of your own Windows machines that has RAD Studio installed and properly licensed. This is free, extremely easy to do and opens up many possibilities for automation of your Delphi projects via GitHub. (The same hosted-runner concept is available for Azure DevOps, GitLab and other services which might be covered in future articles based on feedback received.)

GitHub runners can be installed to interact with all repositories in your GitHub Organization, or on a per-repository basis. If you host any public repositories in your organization, I strongly recommend adding runners on a per-repository basis. For this example, we will install a runner for the [**rad-authenticator**](https://github.com/radprogrammer/rad-authenticator) repository, but the steps are nearly identical to setup a runner that can be shared at the organization level.

View your settings page for your repository and find the **Actions** menu and click on the **Runners** sub-menu item. This will list any runners that are currently defined for your repository and display their operational status. There currently are not any runners configured for the rad-authenticator repo, so we will click on the **New self-hosted runner** button as shown below.

![Adding a new self-hosted runner to a GitHub repository](/assets/blog/Continuous-Integration/GitHub-Runners//GitHub-Actions-New-Self-hosted-Runner.png)

Once you have clicked on the **New self-hosted runner** button, you will be presented with a screen to select the **Operating System** and **Architecture** type of the custom runner. We will obviously want to select **Windows** and the only architecture option is **x64**. Ensure those options are selected and you will be provided with the custom PowerShell script to download and install a runner on whatever machine you would like to use for servicing GitHub action build requests. An example script is displayed below. You should notice that a security token is automatically generated and displayed on this screen. This single-use token is only usable for registering self-hosted runners and is also only good for sixty minutes. If you do not complete your install in time, simply refresh the browser page and a new token will be generated.

![Example powershell script to install a GitHub self-hosted runner](/assets/blog/Continuous-Integration/GitHub-Runners/GitHub-Runner-powershell-script.png)

The convention is to install your runner in the root drive in a folder called **actions-runner**. You will also need specify a working directory for use in completing build requests which by convention is named **\_work**. Keep in mind that you can eventually install multiple GitHub self-hosted runners on the same machine and can include runners from other services as needed. I suggest creating a folder called **Automation** and, within that folder, have a subdirectory called **GitHubActions** with it having two subdirectories called **DelphiRunner** and **DelphiWork** but you can be as creative as desired, or simply use the defaults.

The GitHub provided PowerShell script assumes that you will make a directory off of the current folder named **actions-runner** and then switch to that directory to download the runner binary archive. You can execute the suggested PowerShell script, or simply download the file manually with your browser and extract it anywhere you want the runtime files to be installed.

In this example, we are downloading the current GitHub self-runner version 2.2.285.0 for Windows which is about 68MB in size: [https://github.com/actions/runner/releases/download/v2.285.0/actions-runner-win-x64-2.285.0.zip](https://github.com/actions/runner/releases/download/v2.285.0/actions-runner-win-x64-2.285.0.zip)

Once you have unzipped the archive file into your target runner folder, you will want to configure the runner by executing the included **config.cmd** PowerShell script. You will need to run this script with administrator access as it will be creating a new Windows Service.

You can right-click the **config.cmd** file within Windows Explorer and select the "Run as administrator" option to be prompted for your repository URL and temporary security token, or you can copy/paste the provided script line and execute it at a PowerShell command prompt. (**./config.cmd --url https://github.com/radprogrammer/rad-authenticator --token ....**)

As seen below, you will be prompted for the name of the runner group which is currently only available for GitHub Enterprise accounts so most of us will simply hit **Enter** to use a Default group. Next, you will be prompted to enter the name of the runner which defaults to the computer name. I suggest leaving this at the default value and once again simply press **Enter** to accept the default. Your runner will automatically be assigned the labels 'self-hosted', 'Windows', 'X64' and you can create additional labels as desired. As seen below, I added **D28.Alexandria.11.0** to establish which version of Delphi is installed on this machine and hit **Enter** to continue. At that point, the command script will test the connectivity to GitHub and configure your runner. You will then be asked for the work folder, which defaults to **\_work**. In the screen shot below, I entered in **F:\\Automation\\GitHubActions\\DelphiWork** and hit **Enter** to save my settings.

![Run config.cmd to setup GitHub self-hosted runner in Windows](/assets/blog/Continuous-Integration/GitHub-Runners/GitHub-Runner-Install-Powershell1.png)

Next, you will be prompted to configure the runner as a Windows Service. Type in **Y** and hit **Enter** to continue. You then need to specify the user credentials for running the service. This defaults to using the built-in NETWORK SERVICE account but I suggest you change this to use your local user account (so you the builds are performed with the same user account as when you do them manually.) As seen below, you will need to type in your username and password and the PowerShell script will automatically create a new Windows Service and grant the proper permissions. It will then automatically start up the Windows Service. You can type **Exit** or simply close the PowerShell command window.

![Install GitHub self-hosted runner as a Windows Service](/assets/blog/Continuous-Integration/GitHub-Runners/GitHub-Runner-Install-Powershell2.png)

At this point, if you revisit the Runners page on GitHub.com your runner should be listed and with the status of **Idle** as seen below.

![Delphi GitHub local runner](/assets/blog/Continuous-Integration/GitHub-Runners/Delphi-Github-Runner.png)

## Key Point
Your local GitHub runner service periodically polls GitHub looking for work to be performed. Based on the [**documentation page**](https://docs.github.com/en/actions/hosting-your-own-runners/about-self-hosted-runners), the runner utilizes a [**HTTPS long poll**](https://datatracker.ietf.org/doc/html/rfc6202) every 50 seconds. Only outbound connections are made, so no firewall changes are typically required, but if you also restrict outbound connections then you should check out the [**GitHub documentation page**](https://docs.github.com/en/actions/hosting-your-own-runners/about-self-hosted-runners) for which public hosts will are used.

You should now be able to load up the Windows Service Control Manager (**services.msc**) and see the new "GitHub Actions Runner" Windows Service listed and running. I have double-clicked the service to view the details in the screen shot below:

![Viewing the GitHub self-hosted runner Windows Service](/assets/blog/Continuous-Integration/GitHub-Runners/Windows-SCM.png)

You should now have a working communication channel between GitHub and your local Delphi workstation which has been setup and configured in less than five minutes! So...what do you do with this newly found power? Well, just about anything you want. If you are currently running Jenkins or some sort of CI/CD automation server locally, you can use this as a path for serverless operations. If you do not have any sort of automation running today, you can easily add new GitHub Actions introduce some automation to your current workflows (which is exactly what we will introduce in future blog articles.)

## Public Repository Warning

While there are large benefits for using self-hosted runners (they do not consume minutes on your GitHub plan and they allow you to run any software that is not supported by GitHub), these runners can open the door for nefarious activity to be executed on your local machine. The actions executed by this runner are defined in custom workflow configurations defined in your repository. Typically, this should not be an issue for private repositories with restricted access on who can access. For public repositories that welcome Pull Requests from any GitHub user, these requests could be easily configured to compromise your local machine. Consider an example of some random GitHub user submitting a Pull Request for their contributed code. If your defined GitHub workflow for Pull Requests is to update the source, rebuild on your local Delphi machine, and execute the newly built unit test executable on that same machine, then that untrusted user could introduce any sort of code into that process and potentially take over your local Delphi machine. There is some protection built-in for this, see the help page for [**Approving workflow runs from public forks**](https://docs.github.com/en/actions/managing-workflow-runs/approving-workflow-runs-from-public-forks).

_Simply put - be careful using self-hosted runners with public repositories!_ You should be intimately familiar with your Organization and Repository settings to protect yourself from malicious or accidental damage. GitHub [**recommends**](https://docs.github.com/en/actions/hosting-your-own-runners/about-self-hosted-runners) that you do NOT use self-hosted runners on public repositories. There are a few GitHub support [**threads**](https://github.community/t/self-hosted-runner-security-with-public-repositories/17860) that cover this [**issue**](https://github.com/actions/runner/issues/494) with GitHub staff [**explicitly**](https://web.archive.org/web/20210729113509/https://github.community/t/self-hosted-runner-with-public-repository/167356) stating they generally [**caution against it**](https://stackoverflow.com/questions/64553739/how-to-prevent-github-actions-workflow-being-triggered-by-a-forked-repository-ev).

At minimum, you should change the setting to "Require approval for all outside collaborators" found in your Actions Permissions configuration. This was [**introduced recently**](https://github.blog/changelog/2021-07-01-github-actions-new-settings-for-maintainers/) to prevent some security issues with self-hosted runners. See this [**GitHub blog post**](https://github.blog/2021-04-22-github-actions-update-helping-maintainers-combat-bad-actors/) for more info behind their change.

![Require approval for all outside collaborators](/assets/blog/Continuous-Integration/GitHub-Runners/Fork-Pull-request-workflows-from-outside-collaborators.png)

After prepping this article and re-reading the warnings again, I have removed the runner on my public repository and created a private repository with a self-hosted runner installed. I will use the private repository with all the Delphi automation as the master, and then automatically push the tested changes to the public repository. This will add a layer of protection to the runner on my machine without adding too much extra effort. The new option GitHub added to 'require approval for all outside collaborators' does lock down the runner from the most obvious attack vector, but it does leave the door open to possible mistakes being made. I will revisit this later if more options are added to secure self-hosted runners. (Or if I ever become more knowledgeable and assured of their safety.)

## Further Notes

There is a list of [**environment variables**](https://docs.github.com/en/actions/learn-github-actions/environment-variables) which are available for use within the GitHub runner build environments. One of the main environment variables utilized is called **GITHUB\_WORKSPACE** which corresponds to the name of the work folder you entered in the initial configuration steps above and this will be the folder utilized for local builds. The GitHub workspace directory path is initially empty. The [**actions/checkout**](https://github.com/actions/checkout) action will check out files to this directory.

For the curious types, this setting is stored in the runner's main folder within a hidden json configuration file named **.runner** as shown below:

![Runner config file contents](/assets/blog/Continuous-Integration/GitHub-Runners/GitHub-Runner-config.png)

There is another hidden config file named **.service** which contains the name of the associated Windows Service used by this particular runner instance. Also, within the main runner folder, there is a subdirectory named **\_diag** which contains log files which can be reviewed for errors.

## Next Steps

You will probably want to implement [**GitHub Flow**](https://docs.github.com/en/get-started/quickstart/github-flow) workflow to trigger actions on your local runner whenever a Pull Request is created. By leveraging GitHub Actions on Pull Request events, you can validate builds, run unit-tests, and perform other code quality checks before the code is committed. In an upcoming blog article in this series we will demonstrate how to get unit tests built and executed automatically on every Pull Request.

Are you are creating Pull Requests even if you are the only developer on the project? I know that I have not been doing this myself on my single-user repositories, but after reading [**articles**](https://www.tempertemper.net/blog/why-i-always-raise-a-pull-request-on-solo-projects) and giving it more thought, I plan to require this in the future on all of my repositories. This is even more important once you add GitHub Actions automatically triggering Delphi builds locally on a Pull Request. It becomes a great way to add a powerful, serverless, and free automation system to your process. That is a pretty cool result - even for old school Delphi developers.
