# Pivot



## About Myself

1.  About Myself     :B_column:BMCOL:

    -   James Aimonetti
        -   `Eye-Moe-Net-Tea`
        -   Commit to memory Patrick!
    -   Kazoo Architect and Community at 2600Hz
    -   mc\_ on freenode

2.  Presenter     :BMCOL:

    <div class="org-center">
    ![img](./images/presenter_h280.jpg)
    </div>


## High-level     :B_block:

-   Point a callflow at your server
-   Process the call against your business logic
-   Return Kazoo callflow JSON


## Flow     :B_block:

![img](./images/pivot_flow.png)


## Setting up Kazoo     :B_block:

-   create a callflow
-   set the pivot action as the child
-   set the URL to your webserver and script

```shell
curl -X PUT \
-H "x-auth-token: {AUTH_TOKEN}" \
-H "content-type: application/json" \
-d '{"data":{
  "numbers":["12345"]
  ,"flow":{
    "module":"pivot"
    ,"data":{
      "voice_url":"http://your.server/pivot.php"
    }
  }
 }
}' \
http://crossbar:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

Pivot will attempt to reach your server for calls to that callflow.


## The Request     :B_block:

-   GET - query string parameters
-   POST - x-www-form-urlencoded request body


## Common Request Fields     :B_block:

| Name             | Description       |
|---------------- |----------------- |
| Call-ID          | SIP Call-ID field |
| Request          | SIP Request user  |
| To               | SIP To user       |
| From             | SIP From user     |
| Account-ID       | Kazoo Account ID  |
| Caller-ID-Name   | CID Name          |
| Caller-ID-Number | CID Number        |  |  |


## Other Request Fields     :B_block:

| Name     | Description                           |
|-------- |------------------------------------- |
| Digits   | DTMFs (or collections) received       |
| User-ID  | Owner ID(s) of the calling device     |
| Language | Configured by the device/user/account |


## Create a handler     :B_block:

-   Create the script that Pivot will call
-   Process and build the Kazoo callflow JSON
-   Set **Content-Type** to **application/json**
-   Return the JSON as the body of the HTTP response


## Let's Build something!

<div class="org-center">
Example Time!
</div>


## Say something     :B_frame:

-   Uses [FreeSWITCH `mod_flite`](https://freeswitch.org/confluence/display/FREESWITCH/mod_flite) by default
-   Supports [iSpeech](http://www.ispeech.org/text.to.speech) and [VoiceFabric](https://voicefabric.ru/) as alternatives


## Say Hi to the caller     :B_frame:


## Play an MP3 to the caller     :B_frame:


## Bridging the caller to a user     :B_frame:

Bridge to:

-   [Device](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/callflows.device.json)s (SIP, CallFwd, WebRTC)
-   [Users](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/callflows.user.json)
-   [Ring Groups](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/callflows.ring_group.json)
-   [Conferences](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/callflows.conference.json) (Pre-defined or ad-hoc)
-   and more!


## Bridging the caller to a carrier     :B_frame:

-   Toggle between local and global resources
-   Statically define a number to dial out to
-   See the [schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/callflows.resources.json) for all the **data** options


## Collecting DTMF     :B_frame:

First, collect the DTMF


## Collecting DTMF     :B_frame:

A followup HTTP request will be sent to `http://pivot.your.company.com/collected.php`

-   Request includes `Digits[custom_name]=1234`
-   DTMF can either come as a basic string (if no custom name is used) or an array.
-   Let's see how to process them.


## Collecting DTMF     :B_frame:


## Sending Presence Updates     :B_frame:

Send custom presence updates

-   Some IoT devices/appliances can process SIP NOTIFY
-   `presence_id` can be **name** or **name@realm**
-   `status` can be **idle**, **ringing**, or **busy**


## Callflow actions     :B_frame:

1.  Bridging     :B_column:

    -   Device
    -   User
    -   Group
    -   Resource
    -   Conference
    -   Voicemail Box
    -   Fax Box

2.  Caller ID     :B_column:

    -   Statically set Caller ID
    -   Dynamically set Caller ID
    -   Prepend Caller ID
    -   Routing via Whitelist or Regex of Caller IDs


## Callflow actions (cont)     :B_frame:

1.  Features     :B_column:BMCOL:

    -   Eavesdrop
    -   Group Pickup
    -   Intercept
    -   Page Groups
    -   Hotdesk

2.  Miscellaneous     :B_column:BMCOL:

    -   Toggle call forwarding
    -   Toggle Do Not Disturb
    -   Directory services
    -   DISA
    -   Detect incoming fax
    -   Menus
    -   Custom Webhook


## Brainstorm     :B_frame:

1.  Possible Integrations     :B_block:

    -   Calendars (Office365, Google, etc)
    -   Put received media (vm, fax) into services (storage, chat)
    -   Ad-driven, free calling?
    -   Digital assistants
    -   IVR Analytics (track stats as callers progress)
    -   CRM/Salesforce access
    -   Combine with metaflows and webhooks: call queues
    -   **Your Ideas?**


## What's Next?     :B_frame:

1.  In Progress

    -   Building automated doc generation
    -   Continuing to add callflow actions

2.  How can you help?

    -   Build, build, build!
    -   Blog, blog, blog!
    -   Contribute to the docs effort
        -   Take ref docs and create/update the **real** docs


# Thank You