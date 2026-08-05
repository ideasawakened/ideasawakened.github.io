---
layout: post
title: "Using Kai Inside RAD Studio to Generate Delphi Projects"
date: 2026-08-05 18:00:00 +0000
last_modified_at: 2026-08-05 18:00:00 +0000
categories: [Tooling, AI]
tags: [Continuous-Delphi, Kai, LLM, HTTP Client, Simon Cropp, replicant]
permalink: post/using-kai-inside-rad-studio-to-generate-delphi-projects
published: true
image: /assets/blog/continuous-delphi/delphi-replicant/delphi-replicant-250x250.png
description: "How Kai brings AI-assisted Delphi project creation, refactoring, and build feedback directly into RAD Studio."
---
I imagine most Delphi developers have looked at AI coding tools with the same basic question: does this actually help me build Delphi software, or does it mostly produce code-shaped text that I still have to drag back into the IDE and fix by hand?

That distinction matters. Delphi development is not just editing a `.pas` file. A real project includes a `.dpr`, a `.dproj`, source paths, platform settings, packages, resources, generated files, compiler output, and whatever unsaved state is currently sitting inside the RAD Studio editor. If an AI assistant cannot see enough of that workflow, it is useful in the same way a web search is useful. Sometimes helpful, but still outside the place where the work is happening.

Kai changes that equation because it lives inside RAD Studio.

I recently used Kai in RAD Studio 13.1 to build a small Win32 console project from scratch. The request was intentionally plain: create a console app that downloads `https://httpbin.org/json`, stores the response under a cache folder beside the executable, and prints how long the download takes. Then the experiment grew a little. Review an MIT-licensed .NET project ([replicant](https://github.com/SimonCropp/Replicant)), adapt the useful idea into Delphi, move the reusable code out of the demo program, update the project structure, write a README, compile it, and clean up the repository layout.

That is the kind of task where the value is not one clever code completion. The value is keeping the whole loop inside RAD Studio.

## More than generated text

The first useful thing Kai did was create the starting project. That is already handy, but it is not the most interesting part. The more useful part came after the first version existed.

I could ask Kai to change the shape of the code, and it understood the nearby project context. When the cache implementation outgrew the `.dpr`, I asked it to move the reusable code into `Replicant.Engine.pas`. Later, the repository needed another cleanup pass: reusable units moved into a root `source` folder, the console project references were updated, the README moved to the root, and the notice file moved with it.

That is normal software development. You create something, notice the first design is too cramped, move code into a better home, fix the project file, build again, and repeat. The difference is that Kai can participate in that loop without making me leave the IDE and manually shuttle edits between tools.

## The compiler stays in the conversation

The Embarcadero Kai page describes Kai as agentic AI for Delphi, C++Builder, and RAD Studio. It is not positioned as only autocomplete. It includes inline suggestions, Ghost Text, CodeInsight integration, Panel Completions, Agent Chat with project context, and workflows for compiling projects, reviewing compiler messages, and fixing errors and warnings.

That is the right direction for Delphi work.

Most Delphi projects have enough history that you cannot trust a pretty generated answer until the compiler has had its say. The code may look plausible and still miss a unit, use a symbol from the wrong RTL version, assume the wrong platform, or forget how the project search path is configured. When the build is part of the AI workflow, the assistant has a much tighter feedback loop. It can make a change, compile, read the actual failure, and adjust the code.

That does not remove the developer from the process. It removes some of the low-value shuffling around the process.

## Staying close to Delphi

In this little project, the HTTP work used Delphi's built-in HTTP client classes rather than pulling in another dependency. The reusable cache layer landed in normal Delphi units. The demo stayed a simple Win32 console app. The project remained buildable from RAD Studio.

That sounds ordinary because it should be ordinary. A useful AI assistant for Delphi should make it easier to use Delphi well, not quietly steer the codebase toward whatever library or language ecosystem the model happens to know best.

Kai also matters because it is aware of RAD Studio as an environment, not just Delphi as syntax. According to Embarcadero, Kai can help generate UI forms, configure controls, refactor and modernize existing Delphi and C++Builder code, and work with project context. It also supports flexible model choices, including cloud model providers and local model options depending on configuration and license setup.

That model flexibility is important. Some work benefits from the strongest hosted model you can use. Some work belongs closer to the machine or the network where the code already lives. Delphi shops vary a lot in age, size, security expectations, and tolerance for cloud dependencies. Giving teams options is the practical answer.

## The useful middle ground

I do not think Kai means every Delphi developer should stop understanding project files, unit references, compiler warnings, or the RTL. That would be a bad trade.

The better use is the middle ground: let Kai handle the mechanical parts that are easy to describe but tedious to execute, while the developer keeps judgment over architecture, licensing, code ownership, and final behavior.

In this session, that meant:

- creating a new console project
- adapting an MIT-licensed idea into Delphi
- building a small HTTP cache around Delphi RTL tooling
- separating reusable code from demo code
- updating project references after moving files
- writing project documentation
- compiling the result to catch mistakes

None of those steps are magic. Together, they are a useful chunk of project work completed without constantly context-switching away from RAD Studio.

## Available now

Kai is moving quickly. That is worth saying plainly. The product is young, the pace of improvement is fast, and the tight integration with RAD Studio is exactly where Delphi developers should want this category of tooling to improve.

Kai is available today as a trial or to purchase. For many users, it can be downloaded and installed through GetIt, and Embarcadero also lists trial and purchase paths from the Kai product page.

More information is available here:

[Kai for RAD Studio](https://www.embarcadero.com/products/rad-studio/kai)

If you are already spending most of your day inside RAD Studio, the interesting part of Kai is not that it can generate Delphi code. The interesting part is that it can help with Delphi project work from inside the place where Delphi project work already happens.

---

## All of the prompts used to create this project (and the blog post above)

### Prompt 1: create the project:

```
Create a new Win32 console project in `C:\code\delphi-replicant\projects\console-compare` that simply downloads `https://httpbin.org/json` to {exe}/cache folder and prints how long it takes.  Use the built-in HTTP Client tooling provided by Delphi RTL
```

### Prompt 2: review the reference implementation of Replicant:

```
KAI Prompts: Review the MIT-licensed Replicant repo found at: `C:\code\Replicant`  and plan for a Delphi HTTP client cache implementation based on it.
```

### Prompt 3: Implement a Delphi version of Replicant:

```
Implement as suggested, including your two suggested improvements.  Then alter the demo project to download the JSON file three times and print response times.
```

### Prompt 4: Slightly optimize the output:

```
Migrant the reusable code from the dpr to an Replicant.Engine.pas file 
```

### Prompt 5: Create a README:

```
Create a project README.md that explains the source of this project (created with the KAI pluging (version 1.0.2) within RAD Studio 13.1 with code generated by GPT-5.5 XHigh, based on MIT-licensed source repo: https://github.com/SimonCropp/Replicant)  Explain the functionality available and provide a few code snippets on how to use it.
```

### Prompt 6: Reorganize the files

```
Do a little cleanup to standardize the repo contents: Move the reusable .pas files to a /source folder in the repository root (and update the project and ensure it's buidable.)  Move the README.md content into the repository root's README.md and delete the one in the console-compare folder.  Move the NOTICE file into the repository root. 
```

### Prompt 7: Write the blog article above:

```
Finally, lets wrap up this Kai session with a short blog article.  Review one of my recent articles for style/tone: `https://ideasawakened.com/post/delphi-clean-for-rad-studio` and create a blog article explaining the value of using Kai inside RAD Studio to generate Delphi projects.  Review Kai's main webpage for product details to include in the blog post and use this link as a "more info available" `https://www.embarcadero.com/products/rad-studio/kai`   Explain that Kai is rapidly improving and is available today as a trial or to purchase.  Kai can be easily downloaded and installed via GetIt.
```

----

## delphi-replicant is available on GitHub

The Delphi version of [Replicant](https://github.com/SimonCropp/Replicant), created by Kai, is available on GitHub: [delphi-replicant](https://github.com/continuous-delphi/delphi-replicant)  Add the files from the /Source folder to your project and get easy client-side HTTP file caching by using `TReplicantHttpCache`

## Project Implementation Details

- RAD Studio 13.1
- KAI version 1.0.2
- OpenAI Codex via GPT-5.5 Xhigh

## Example output from the Win32 ConsoleCompare project created using delphi-replicant:

Note Run 1's time of 155ms is the actual file download time where run 2 and 3 utilize the local cached copy.

```
Executed:
consolecompare
Run 1: 155 ms (network, HTTP 200)
Run 2: 0 ms (cache, HTTP 200)
Run 3: 9 ms (cache, HTTP 200)
Downloaded https://httpbin.org/json
Saved to C:\code\delphi-replicant\projects\console-compare\Win32\Debug\cache\httpbin.json
```

Thanks to [*Simon Cropp*](https://github.com/SimonCropp) for his Replicant library which is now available to Delphi Developers after a quick half-hour session with [Kai](https://www.embarcadero.com/products/rad-studio/kai)!   Build your solution with Kai and enter the **Summer Developer Campaign 2026**: [https://learndelphi.org/kai/](https://learndelphi.org/kai/)
