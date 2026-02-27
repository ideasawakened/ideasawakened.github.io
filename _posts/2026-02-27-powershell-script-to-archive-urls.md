---
layout: post
title: "Help Prevent Link Rot: Archive URLs from the Command Line with PowerShell"
date: 2026-02-27 00:30:00 +0000
last_modified_at: 2026-02-27 00:31:00 +0000
categories: [Tooling, Automation]
tags: [PowerShell, Wayback Machine, archive.ph, gist, GitHub]
permalink: post/powershell-script-to-archive-urls
published: true
image: /assets/blog/PowerShell/PowerShell_ArchiveUrls_WaybackMachine_small.png
description: "A self-contained PowerShell 7 script that archives URLs to the Wayback Machine and archive.ph simultaneously"
---

Anyone who has maintained technical documentation over time has run into the problem of 
link rot. After carefully crafting a `links` section pointing to a relevant blog post or reference
page, six months later it can return a 404. The missing links eventually reduces the long-term value
of your content.

The common solution is to snapshot URLs to a web archiving service at the time you write
the documentation. That way, even if the original disappears, you have a permanent copy to
fall back to. The problem with doing this manually (opening a browser, navigating to the
archiving service, pasting the URL, waiting for the result, copying the snapshot link) is 
mind numbing if you do it enough. (At least for the impatient types like me.)

I ran into this problem while building out documentation for an upcoming Delphi-related project. I wanted
to include the archive url along with every external reference I have in the documentation pages, and
I could only repeat this process a few times manually before a PowerShell utility was created to help
automate the boring task. If you regularly publish technical documentation, blog posts, or README files
that reference external resources, this small utility may also help save you time and prevent future link rot.

## A New Helper

**Invoke-WebArchiveSnapshot.ps1** is a self-contained PowerShell 7 script that:

- Fetches the page title from the target URL for use as link text
- Submits the URL to the **Wayback Machine** (web.archive.org) via their documented save API
- Polls the Wayback availability API to resolve the specific dated snapshot URL
- Submits the URL to **archive.ph** via their GET-based form submission
- Prints ready-to-paste markdown with both snapshot links

A companion `arcurl.bat` wrapper can make the PowerShell invocation short enough to actually use more than once.
I named my local batchfile `arcurl.bat` but will likely come up with a better name later. If you have a better one,
please let me know as I'm not too thrilled with 'arcurl' but hey, it is certainly better than typing the whole powershell line
every time.

```cmd
@echo off
if "%~1"=="" (
  echo.
  echo  arcurl - Archive a URL to Wayback Machine and archive.ph
  echo.
  echo  Usage:   arcurl ^<url^>
  echo  Example: arcurl https://ideasawakened.com/post/some-post
  echo.
  echo  Submits the URL to both archiving services and prints
  echo  ready-to-paste markdown with the resulting snapshot links.
  echo.
  exit /b 1
)

pwsh -ExecutionPolicy Bypass -File "%~dp0Invoke-WebArchiveSnapshot.ps1" -Url "%~1" -CopyToClipboard -NoHostMarkdown
```

The Gist for the full PowerShell script is available here: **[https://gist.github.com/darianmiller/254cc8851d9759e22ac0a886bf7bae60](https://gist.github.com/darianmiller/254cc8851d9759e22ac0a886bf7bae60)**

## Usage

Download the script and optionally create a simple batch file wrapper for ease of use.
The simplest invocation via the wrapper becomes something like:

```cmd
arcurl https://example.com/some-post
```

Or invoke the script directly:

```powershell
pwsh -ExecutionPolicy Bypass -File ./Invoke-WebArchiveSnapshot.ps1 -Url 'https://example.com/some-post'
```

To copy the few lines of markdown directly to your clipboard when its completed, add the optional switch: `-CopyToClipboard`:

```powershell
pwsh -ExecutionPolicy Bypass -File ./Invoke-WebArchiveSnapshot.ps1 -Url 'https://example.com/some-post' -CopyToClipboard
```

## Example Output

A typical run ends up looking something like this:

```powershell
pwsh ./Invoke-WebArchiveSnapshot.ps1 -Url 'https://ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt' -CopyToClipboard -NoHostMarkdown
```

````
IdeasAwakened: Archive Snapshot Tool - v1.1.0
------------------------------------------------
  URL: https://ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt
  Title: Creating unit tests for Delphi with ChatGPT | IdeasAwakened

Wayback Machine (web.archive.org)
---------------------------------
  Submitting...
  Snapshot queued. Resolving closest snapshot (up to 30s)...
  Closest:     https://web.archive.org/web/20250809030522/https://ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt
  Search:      https://web.archive.org/web/*/https://ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt

archive.ph
----------
  Submitting...
  Failed:      Response status code does not indicate success: 429 (Too Many Requests).
  Manual:      https://archive.ph/?url=https%3A%2F%2Fideasawakened.com%2Fpost%2Fcreating-unit-tests-for-delphi-with-chatgpt
  Rate limited. Wait a few minutes and try again, or use the manual URL above.

Markdown for documentation
--------------------------
  Markdown copied to clipboard.

- [Creating unit tests for Delphi with ChatGPT | IdeasAwakened](https://ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt)
  - [Wayback Machine snapshot](https://web.archive.org/web/20250809030522/https://ideasawakened.com/post/creating-unit-tests-for-delphi-with-chatgpt)
  - [archive.ph snapshot](https://archive.ph/?url=https%3A%2F%2Fideasawakened.com%2Fpost%2Fcreating-unit-tests-for-delphi-with-chatgpt)
````



The last three lines of output are ready to be pasted directly into a Markdown document.

### Optional Parameters

| Parameter | Purpose |
|---|---|
| `-CopyToClipboard` | Copies the generated markdown to the clipboard when finished |
| `-PassThru` | Emits a structured object to the pipeline instead of plain markdown, useful for scripting |
| `-NoHostMarkdown` | Suppresses the Write-Host markdown print; markdown is still emitted to the pipeline |
| `-CI` | Exits with a non-zero code when one or more submissions fail; also auto-detected from `GITHUB_ACTIONS` and `CI` environment variables |

The script emits the markdown to the PowerShell pipeline regardless of whether
`-PassThru` is specified, so you can capture it in a variable:

```powershell
$md = ./Invoke-WebArchiveSnapshot.ps1 -Url 'https://example.com/some-post' -NoHostMarkdown
```

`-NoHostMarkdown` prevents the markdown from printing twice — once to the host and once
from the pipeline — in scenarios where you are capturing the output but stdout is not
technically redirected (assignment capture happens at the PowerShell layer, which
`[Console]::IsOutputRedirected` cannot detect).


## Rate Limit Notes

Both services impose rate limits. archive.ph in particular will return a `429 Too Many Requests` response
if you submit too many URLs in a short window. The script detects this and prints a reminder to wait
a few minutes before retrying. The Wayback Machine is generally more tolerant but can time out under load,
especially during peak hours. In both cases the script falls back gracefully and prints a manual
submission URL so you are never left without a path forward.

The script runs unsigned. Use `-ExecutionPolicy Bypass` for the invocation, or sign it with
your own certificate if your environment requires signed scripts. 

## Implementation Notes

A few things worth explaining for anyone who wants to adapt the script:

**Wayback Machine** uses a documented save API -- a simple GET to
`https://web.archive.org/save/<url>`. The dated snapshot URL is returned in the
`Content-Location` response header when available. When it is not immediately returned,
the script polls `https://archive.org/wayback/available?url=<encoded-url>` every three
seconds for up to thirty seconds to resolve the specific snapshot URL. This avoids returning
a wildcard search URL when a precise link is achievable with a short wait.

**archive.ph** has no public API. The script submits via a GET request to
`https://archive.ph/submit/?url=<encoded-url>`, which matches the form on their homepage.
The response redirects to the snapshot URL on success. This approach is reliable under normal
conditions but could break silently if archive.ph changes their submission endpoint. The
script fails gracefully with a manual fallback URL in that case.

**Page title extraction** uses a multiline, case-insensitive regex against the `<title>`
tag of the target page. `System.Net.WebUtility.HtmlDecode` handles HTML entities — no
extra assembly load required. If the fetch fails for any reason, the raw URL is used as
link text instead.

**Exit codes** are opt-in via `-CI` or auto-detected from standard CI environment
variables (`GITHUB_ACTIONS`, `CI`). When active: 0 for full success, 1 if one service
failed, 2 if both failed. Exit codes are not emitted in interactive sessions unless
explicitly requested, so the script is safe to run from a shell without concern about
terminating your session.

**Pipeline output** — the markdown is emitted via `Write-Output` in addition to
`Write-Host`, so callers can capture it without any extra flags. Use `-NoHostMarkdown`
to suppress the host print when capturing via assignment.


The script is intentionally self-contained and only uses built-in .NET APIs
available in PowerShell 7, which should make it portable across Windows, macOS, and Linux
(currently only tested on Windows.)

## Gist

The script is available as a Gist on GitHub:

**[https://gist.github.com/darianmiller/254cc8851d9759e22ac0a886bf7bae60](https://gist.github.com/darianmiller/254cc8851d9759e22ac0a886bf7bae60)**

Please let me know if you have any comments, corrections, or improvements to suggest!
