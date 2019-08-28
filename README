# nngmail

## Introduction

**nngmail** is a **Gnus** back end for reading email using the **Gmail**
REST API. If you read email with Gnus and use a Gmail-hosted account,
you can use **nngmail** instead of **nnimap**.  **nngmail** does not act
as a transport agent.  Sending and replying to email will continue to
use whatever method you have setup in Emacs previously.

nngmail does not access the Gmail servers directly.  Instead it relies
on a local server (or proxy) that stores message metadata and caches
some messages in a local sqlite database.  Because Gnus can access data
locally, reading mail is instantaneous.  Synchronous requests to Gmail
are required for:

 - Reading a message that is not in the local cache
 - Searching your email
 
Synchronization happens via polling (the polling interval is
configurable).  Cached messages are zipped before storing them in the
sqlite database to save space.
 
The local server can sync multiple Gmail accounts simultaneously.  You
access them by setting up multiple servers in Gnus.  The proxy does not
(currently) provide for real synchronization support in offline mode
(see [Destructive mode](#destructive-mode) below)

While the local proxy is meant for use with the nngmail back end, it
provides a generic REST interface for reading messages that may be of
use for other tools.  For example, it may be interesting to generate an
email dashboard.  In a pinch, you can read your mail by simple running
sqlite!

## Status 

I use this on a daily basis but you should consider it
["me-ware"](https://ericsink.com/articles/Yours_Mine_Ours.html).  I am
not adverse to it becoming "us-ware" but that's not what has driven the
effort.  I wrote it in a bit of a hurry as I was more interested in how
much work would be required to write a new Gnus back end.  Now that I
have convinced myself it is a possible and worthwhile effort, I will go
back and develop a test suite before adding more features.  Plus setup
is a bit involved--you need to be comfortable with *shell*, *python*,
and *Emacs*.

This software will **not** eat your mail no matter how hungry it is.  By
default the system requests read-only access to your Gmail account.  The
Gmail API service will thus deny requests to modify or delete messages
(if it doesn't, something is seriously wrong).

There is experimental^2 support for updating and deleting messages in
the *destructive* branch.  Use with caution, it may well eat your email.

Once there is a nice test suite, I will enable write-mode by default.
Until then, you will have to opt-in to life on the edge.

The local proxy stores the name and email of everyone you correspond
with (so you could use it as a local address book).  But it is rather
simplistic in how it handles multiple email addresses and names.  This
can cause the From field to appear a bit funny at times.  For example,
if a company changes how they send email notifications (I have seen this
with old LinkedIn messages).

### Emacs & OS Support

I use this code on Ubuntu (16.04 LTS and 18.04 LTS) and OSX (10.12 and
10.13) on Emacs 25.3 and 26.1.  I haven't tried running it on Windows
(and probably won't).  Patches for older/newer versions of Emacs are
welcome.

### Security

**The server responds to un-authenticated http requests and makes no
attempt at preventing unauthorized access to your email.** The only
(small) nod at security is to listen on the local interface only.

### Documentation

Yeah, as you guessed there is no documentation other than this
[README](README) file.

There are a number of configuration options which you can tweak.  See
`data/config.yaml` for some.  There is support for parallel requests
when importing an account.  However, it does not yet handle errors due
to too many simultaneous user requests so it's best to leave the setting
as is.


## Motivation

I wrote this because I was frustrated with how long it takes to read
email with Gnus.  My mail reading was state-of-the-art 30 years ago but
it feels like it hasn't improved with time (although it clearly has).
My main frustration was how long it takes to check for new messages.
And so I wanted a system that would allow me to read email quickly and
with low overhead.  In particular, I want:

 - Fast access to recent email 
 - Fast and accurate search for everything else
 - Use Emacs to compose email

I tried **offlineimap** and **notmuch** but neither was what I am
looking for.  I don't want to keep an complete copy of all my Gmail
accounts on my laptop (someday this may not be a problem but right now
it requires a significant fraction of available storage).  Plus I find
search works best in Gmail because it searches through attachments.

Which brings up the topic of why Gmail since we should be wary of
providing Google too much of our personal and private information.  This
is absolutely true.  However, when I looked at all the email I received,
a significant portion of it either originated from or was delivered to
Gmail anyway.  That is, even if I move my account elsewhere Google will
still have my information.  Furthermore, sometimes you are forced to use
Gmail, e.g. for a work account.

When I do move elsewhere, I think the system should be easy to extend to
other mail service providers.
 
## Installation

1. Clone this repo

2. Enable Gmail API access for your account (this may be a bit out of date).

    2.1 Visit the [Google Developer Console]
		(https://console.developers.google.com/).  From there, you need
		to create a project (or choose an existing one) and go to the
		APIs section.  Choose “Gmail API” under the Google Apps APIs
		section and click the “Enable API” button.
	
    2.2 Go to the [Credential page]
		(https://console.developers.google.com/apis/credentials) and
		click *Create credentials > API key* and select the
		appropriate key type.

    2.3 Store the credentials under `data/client-secret.json`.  If you
		choose a different location update the `config.yaml` file.

	Please see the instructions on the [Using API
	Keys](https://developers.google.com/api-client-library/python/auth/api-keys)
	section of the Gmail API documentation if you have questions.

3. Setup the local server

	3.1 Create a new virtual environment (virtualenv).  I recommend
	    *pyenv* to set this up, but *virtualenv* works well too.
	
	3.2 Install nngmail by running

		cd <repo>
		pip install -e .
	
	3.3 If everything was installed correctly you should be able to run

		FLASK_ENV=development FLASK_APP=src/nngmail:app flask routes

	    And see a list of routes for the proxy server.

4. Import your mail.

	4.1 Create the database with

		FLASK_ENV=development ./nngmail.sh init-db
	
	4.2 For each account you want to sync locally run

		FLASK_ENV=development ./nngmail.sh import user@example.com nickname --init-cache
	
	Where you clearly need to replace your user name and the
	nickname.  You will be directed to a web browser where you need
	to login to Google and authorize the application (nngmail or
	whatever you called it in step 1) to access your mail.  In some
	instances (I have only seen this on Ubuntu) you will be asked to
	copy-paste a URL into your browser.

	Pick a different nickname for each account as you will use it to
	identify the accounts to Gnus.

	If things went as expected the client will start synchronizing your
	email.  By default it will download all message metadata and cache
	messages received by the Gmail SMTP servers in the last 120 days
	(edit `config.yaml` to change cache retention timeout).  If
	cache_timeout is set to `0` it will not cache any messages.  If set
	to `-1` it will cache all messages.

	Initial synchronization time depends on how much email you have.
	For me it processes about 100 messages/sec.  YMMV.

	Repeat the import command once for each account you want to
	synchronize/read via nngmail.

5. Configure Gnus to read mail via nngmail.

	5.1 Load nngmail.el into your Emacs session.  For now `M-x find-file RET
	elisp/nngmail.el` and then `M-x eval-buffer`.

	5.2 Configure one virtual server per account.  Add something like this
	to `gnus-secondary-select-methods`.  You need one server per account

		(nngmail "personal"
			 (nngmail-email "personal@example.com")
			 (nngmail-address "localhost")
			 (nngmail-server-port 5544)
			 )
		(nngmail "work"
			 (nngmail-email "work@example.com")
			 (nngmail-address "localhost")
			 (nngmail-server-port 5544)
			 )

	The virtual server name (**personal**, **work** in the example
	above) and the email address must match what you used in the
	command to import the account.
	
	Don't forget to eval the assignment to
	`gnus-secondary-select-methods`.

6. Subscribe to new *groups* in Gnus.  The local proxy presents each
   Gmail label as a newsgroup you can read with Gnus (similar to how
   the IMAP interface works).  If you open *server mode* (with `^` in
   the group buffer) you should see one server per nickname (or
   account).  If they are not listed as *opened* something went wrong.

   Because by default nngmail request read-only access to your
   account(s) messages will not be marked as read permanently (they are
   marked as read until you check for new news).

7. Setup fast access via helm.  This package also provides a helm source
   you can use to read/deal with email (still a bit incomplete).  My
   goal is to provide a quick mechanism for reading and processing email
   without having to open Gnus.  To try this out load `helm-nngmail.el`
   and then run `M-x helm-nngmail`.  It will show you unread messages in
   all your accounts (beware if you keep a large number of unread
   messages in your mailbox).  You can select one or more messages to
   process and typing filters the list of messages.  Use prefix-arg to
   select a different label to process.
   
   The helm interface is still in it's infancy, but I have great hopes
   for what it can do.

## Details

One challenge with using Gnus and Gmail is Gnus requires nice
monotonically increasing article ID's--preferably without too many gaps.
The need for a local database arose from having to map from a Gmail
message ID to something that looks like an article number.

In the database each message is tracked via an `id`, an `article_id`,
and `account_id`.  The `id` is unique across accounts--it's the primary
key of the SQL table.  `article_id` is unique within an account and is
the article number passed to Gnus.  Each row in the table stores the
Gmail ID required to talk to the Gmail API.

The local server provides a very simple interface for accessing the
local sqlite database.  And it acts as a proxy for the Gmail API.  That
is, the nngmail lisp code never talks to the Gmail API directly.  The
proxy server runs a background thread per account to periodically poll
for changes.

Searching (via nnir in Gnus) allows passing arbitrary Gmail search
strings.  The query is further limited by the group (or label) that nnir
is trying to search (see the documentation for nnir for more details).
Because the query is passed as-is to Gmail, you can use the usual Gmail
search tricks.

The synchronization does not exports Gmail-assigned category labels,
like *CATEGORY_PERSONAL*, as groups.  Although there are hooks to export
them if someone really wanted to.

## Performance

I haven't done performance testing, but subjectively there is a world of
difference between using **nnimap** and **nngmail** due to the local
cache.  On my i7-5600U-based laptop (8G of DRAM) I can comfortably read
folders with 5000 emails (Gnus will show the Summary buffer almost
instantaneously).
	
Selecting "articles" (in Gnus parlance) which are in the local cache
should be almost instantaneous.  Selecting articles not in the cache
will synchronously request the message from Gmail using the REST API.

I believe Gnus has support for locally caching articles and such.  But I
have never been able to make it work.  Plus sqlite is a much better
store engine.

## Destructive mode

To enable destructive, a.k.a. read-write mode, edit the
`data/config.yaml` file to add the **modify** scope.  The scopes section
should look like this

    scopes:
      - https://www.googleapis.com/auth/gmail.readonly
      - https://www.googleapis.com/auth/gmail.modify

Note that the refresh token used to authenticate you remembers the
original scopes not the currently requested scopes.  If you initially
logged into Gmail using read-only mode, you will need to delete the
credentials file (`data/<account-email>-creds.json`) and re-authorize
the application to access your account.  Furthermore, you will need to
either re-initialize and re-import your email or hack the SQL to make
the accounts writable.

Please note the same applies when going in the opposite direction.  If
you want to try out *destructive mode* and you add **modify** scope in
`config.yaml`, please remember to delete
`data/<account-email>-creds.json` and re-authenticate.

If you enable *destructive* mode Gnus marks get propagated to the server
for posterity.

Note that the local proxy does not support offline mode.  It relies on
synchronous calls to Gmail to update state.  I may someday implement
support for offline mode so marks set while Gmail is not reachable will
be synchronised next time the API is available, but it is not a goal at
the moment.  It may be possible to get Gnus caching to work with
nngmail to enable offline access but I haven't tried.



