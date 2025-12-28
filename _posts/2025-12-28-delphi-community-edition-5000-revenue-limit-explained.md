---
layout: post
title: "Delphi Community Edition $5,000 Revenue Limit Explained (What Actually Counts)"
date: 2025-12-28 19:00:00 +0000
last_modified_at: 2025-12-28 19:00:00 +0000
categories: [Delphi Programming, Community]
tags: [Community Edition, Licensing, Marco Cantu, Ian Barker, Roland Bengtsson]
permalink: post/delphi-community-edition-5000-revenue-limit-explained
published: true
image: /assets/blog/Delphi-Programming/Community_Edition/Community_Edition_QuestionLicense_Small.png
description: 'Clear explanation of the Delphi Community Edition $5,000 revenue limit, what income counts, employee exceptions, and how the official license defines eligibility.'
---

# Understanding the Delphi Community Edition $5,000 Revenue Rule

## **TL;DR**
- The [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter) eligibility is based on **total annual revenue of the licensee**, not just software income.
- _Employees_ may use Community Edition personally (on their own hardware, with no employer benefit).
- _Business owners or self-employed freelancers_ exceeding $5,000/year are not eligible, even for unrelated work.


## What the official license actually says - and what it means for you

> **Disclaimer**
> This article is an **unofficial, non-legal interpretation** of the Delphi Community Edition license.
> It does **not** represent an official position of Embarcadero.
> The license agreement itself is the controlling document.

Although this article uses “Delphi Community Edition” for brevity, the same Community Edition license terms apply equally to C++ Builder Community Edition. RAD Studio itself is not offered as a Community Edition.

---

## Why this post exists

Few _persistently contentious_ topics resurface more often than the **Community Edition $5,000 revenue limit**.  This was brought up again today on the [Delphi Developer group on Facebook](https://www.facebook.com/groups/137012246341854/permalink/25780915628191497) by **Roland Bengtsson**. I provided the comment referenced below from Marco Cantu which seemed to answer his question but decided to provide a more detailed response here.

![Delphi Community Edition License Questions](/assets/blog/Delphi-Programming/Community_Edition/Community_Edition_QuestionLicense.png)

Common questions include:

- What revenue actually counts?
- Does unrelated income matter?
- Are employees treated differently from business owners?

This post summarizes a practical reading of the official license agreement and FAQ, using direct excerpts from the legal text where relevant.

---

## Official source: License Agreement (v13)

The Community Edition terms are governed by the official **RAD Studio / Delphi / C++Builder Software License and Support Agreement** (_Version 13_ current at time of writing, December 2025.)  View the latest version on the [customer agreements page](https://www.ideracorp.com/legal/embarcadero#tabs-2) on Idera Corporation's legal document list.

---

## Extracted Community Edition license text (key sections)

> **“The Community Edition license applies solely if Licensee cumulative annual revenue
> (of the for-profit organization, the government entity or the individual developer)
> does not exceed USD $5,000.00.”**

> **“If Licensee is an individual developer, the revenue of all contract work
> performed by developer in one calendar year may not exceed the Threshold
> (whether or not the Community Edition is used for all projects).”**

> **“For example, a developer who receives payment of $5,000.00 for a single project
> (or more than $5,000.00 for multiple projects) even if such engagements do not
> anticipate the use of the Community Edition, is not allowed to use the Community Edition.”**

> **“If Licensee is a company that has a cumulative annual revenue which exceeds
> the Threshold, then Licensee is not allowed to use the Community Edition,
> regardless of whether the Community Edition is used solely to write applications
> for the business' internal use…”**

> **“…any such use is unauthorized, constitutes a violation of this Agreement and
> may constitute a misappropriation of Licensor's intellectual property rights.”**

(See the full license PDF for complete wording and context.)

---

## Plain-English interpretation

### 1. The $5,000 limit is **total revenue**
The license does **not** say:

- Delphi revenue only
- Software revenue only
- Consulting revenue only
- App sales only

It explicitly refers to **cumulative annual revenue** of the licensee.

---

### 2. Unrelated income still counts
_For individual developers performing contract/freelance work:_

- **All contract revenue counts**
- Even if Community Edition was **not used**
- Even if the work is **not software-related**

The license text explicitly anticipates and rejects the “unrelated income” argument.

---

### 3. Companies have no personal-use exception for the company itself
If a company exceeds $5,000 in annual revenue:

- Community Edition use is **not permitted**
- Even for internal tools
- Even for free or non-commercial software

What should a company do instead?
- Companies can use the [30-day Free Trial Version](https://www.embarcadero.com/products/delphi/start-for-free) before purchasing
- Companies can always [purchase](https://www.embarcadero.com/app-development-tools-store/delphi) unrestricted commercial  licenses with the choice of Professional, Enterprise, and Architect Editions
- Accredited educational institutions should look into the [Academic Edition](https://www.embarcadero.com/development-tools-for-education) and also check out the [Embarcadero Academy](https://www.embarcaderoacademy.com/)

---


## Employee versus Owner (critical distinction, typically missed)

### Employees
If you are an **employee** (not an owner):

- Your employer's revenue does **not** automatically count as your own revenue.
- You may use Community Edition personally **if**:
  - It is not installed on employer hardware
  - It is not used on employer networks or premises 
  - It does not benefit your employer directly or indirectly

This is clarified in the official [Community Edition FAQ](https://www.embarcadero.com/products/delphi/starter/faq). 

Note: ensure to prevent accidental connectivity to a corporate wifi from your personal laptop as the Community Edition can gather telemetry data about what network it is operating within.


### Owners / self-employed
If you are:

- A sole proprietor
- A contractor
- A freelancer
- A business owner

Then **all business or contract revenue counts**, regardless of source.

---

## Real-world examples

| Scenario | Community Edition Allowed? |
|--------|------------|
| Fast-food employee earning $30k | Yes |
| Salaried dev at large company learning at home | Yes |
| Unemployed dev living on government aid | Yes |
| Retired dev living on savings | Yes |
| Student with no income | Yes |
| Freelancer earning $5,000+ (any industry) | No |
| Fast-food restaurant owner | No |


The determining factor is **employee versus licensee**, not job type.

---

## Community Edition Renewals

In a previous post, [How To Easily Extend your Delphi Community Edition License](https://ideasawakened.com/post/How-To-Easily-Extend-your-Delphi-Community-Edition-License), I explained how the **annual Community Edition license term works**, including:

- The one-year Community Edition license duration
- How re-installation or re-registration renews the Community Edition term
- Staying technically compliant with annual license expiration

Renewing a Community Edition license **does not override** the $5,000 revenue eligibility requirement.
You must satisfy **both**:
1. A valid Community Edition license term, and
2. The annual revenue threshold

---

## Why this topic remains controversial

- Many tools use “non-commercial” or “personal use” licenses
- Visual Studio Community uses a different eligibility model
- “Revenue” is often assumed to mean “software revenue”

The Community Edition instead uses a [bright-line revenue rule](https://grokipedia.com/page/Bright-line_rule), which avoids ambiguity but surprises many users.

---

## Final takeaway

> **Community Edition eligibility is determined by the total annual revenue of the licensee, regardless of source - with a narrow exception for employees using Community Edition purely for personal, non-employer-related purposes.**

This interpretation follows directly from the license text itself.

---

## Historical Statements

**Marco Cantu** specifically lays out who the Community Edition is for in the [Embarcadero Blog Post](https://blogs.embarcadero.com/delphi-12-and-cbuilder-12-community-editions-released/?utm_source=chatgpt.com):

The Community Editions are perfect for:

- Freelance Developers: Create and sell apps until your revenue reaches $5,000 per year.
- Startups: Use the Community Edition if your annual revenue is less than $5,000 and if your team has up to 5 developers.
- Students: Learn and experiment with professional-level tools to kickstart your development career.

 **Marco Cantu** commented on his [Community Edition Blog Post](https://blog.marcocantu.com/blog/2018-july-delphi-ce.html) back on July 21, 2018:

>For the revenue limit, this is tied to the direct use of Delphi CE or software development and related consulting revenues, or revenues obtained based on the use of the software. Anyone with a regular employee job (where Delphi is not used) or a non-work income (retirement pension, unemployment funds, financial revenue, parent inheritance) should be covered. Ultimately, read the EULA, this is just my personal and non-binding interpretation.

---

## Invitation for clarification

If Embarcadero wishes to clarify or update any of these points, such clarification would be welcome and this document may be updated accordingly.

The goal of this post is to help provide clarity and not criticism.  As I do not work for Embarcadero and am not an attorney, this is my _personal and non-binding interpretation_.
