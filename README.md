# Campaigns
Generic site for mail signup campaigns, implemented using Haskell/Yesod for the server and Mailchimp for managing mail subscribers.

__Getting started:__

* `$ cp config/settings.yml.sample config/settings.yml` and fill out your details
* `$ stack build`
* `$ stack exec -- yesod devel`

## Deploying

To deploy the yesod app, take a look at the Yesod book chapter [Deploying your Webapp](http://www.yesodweb.com/book/deploying-your-webapp). I personally like to use Keter (with `stack exec -- yesod keter`), but that is entirely up to you.

### Background Jobs

Currently you need to force the background jobs to run, by visiting the endpoint `/force-run-job`. You can setup a cron job every 30 seconds by adding (or just ignore the second line to run it every minute),

```bash
* * * * * /usr/bin/curl --silent http://konkurrence.hvisk.com/force-run-jobs >/dev/null 2>&1
* * * * * ( sleep 30 ; /usr/bin/curl --silent http://konkurrence.hvisk.com/force-run-jobs >/dev/null 2>&1 )
```

to your crontab file (you can use `crontab -e`).

### Vagrant VM

Included is a `Vagrantfile` that will set up a Ubuntu 14.04 64-bit VM, install the libraries needed (`stack` etc) and build the project on `vagrant up`.

You can then use `vagrant push` to deploy your app with `stack exec -- yesod keter`. NOTE that it will also pull in your key for the server (assuming `~/.ssh/campaigns.pem`) and add your `~/.ssh/config` so that `yesod keter` can use the SCP command.

## Development

You can start the development server using

```bash
$ stack exec -- yesod devel
```

or alternatively the .ghci file is set up for development, providing a `:reload` command that loads `DevelMain` and calls the `update` function. This can be a little speedier than `yesod devel`, namely because of `:set -fobject-code` and `:set -O0` in the `.ghci` file. So you would do,

```bash
$ stack ghci
```

and after it has loaded call

```bash
:reload
```

everytime you want to reload the server.

### Running with docker

There is included a docker setup in the `stack-docker.yaml` file. It will setup the docker image and expose the docker ports `3000` so the website is accessible.

```bash
$ stack --stack-yaml=stack-docker.yaml exec -- yesod devel
```

## Setting up Mailchimp triggers

First you either need to add a new list, or adjust an existing one. What we need to do is add a couple of merge fields, and note down the merge field tags (e.g. MMERGE19):

* Activation Link (type text, not required, not visible)
* Referral Link (type text, not required, not visible)
* Dashboard Link (type text, not required, not visible)
* Signed Up (type text, not required, not visible)
* Confirmed (type text, not required, not visible)
* Reached Step (type text, not required, not visible)

Now we can set up the triggers for sending out the mails,

1. Add a new automation under the Automation tab on Mailchimp
2. Press "Add email" to add a new trigger (this will be the one that sends the activation mail)
3. Edit the trigger for the new mail
4. Set it to send when the "Signed Up" merge fields gets the value "Yes"
5. Press "Add email" to add a second trigger (this will be the one that sends the welcome mail)
6. Set it to send when the "Confirmed" merge fields gets the value "Yes"

Now all that's left is to design the mails and use the merge field for "Activation Link" in the first email, while "Referral Link" and "Dashboard Link" should go into the second email.

Finally, if you want to send mails out when a user gets to a step, you can add a trigger that sends a mail when "Reached Step" changes. This will be updated for each step the user reaches (i.e. 1, 2, 3 and 4).
