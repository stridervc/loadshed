* Command line utility to query Eskom loadshedding status
* Daemon to power down a Linux server / desktop cleanly before loadshedding happens

Linux build instructions
========================
* Install **stack**, either [directly](https://docs.haskellstack.org/en/stable/README/#how-to-install)
or via [ghcup](https://www.haskell.org/ghcup/).

* Clone repo
``` shell
git clone git@github.com:stridervc/loadshed.git
cd loadshed
```

* Build with stack
``` shell
stack build
```

* Install (to ~/.local/bin/)
``` shell
stack install
```

Using loadshedding cli
======================
Firstly, you need the ID number of the province you're interested in. List them with
``` shell
loadshedding provinces
```

Next, you need the ID of the municipality you're in.
``` shell
loadshedding municipalities -p <province ID>
```

Finally, you need the ID of your suburb.
``` shell
loadshedding suburbs -m <municipality ID>

Now you can check the current loadshedding stage.
``` shell
loadshedding
```

And you can get your schedule for a pariticular stage with
``` shell
loadshedding schedule -l <stage> -p <province ID> -s <suburb ID>
```

To avoid having to pass the province and suburb IDs every time, see the configuration section below.

Configuration
=============
Both the command line utility and the daemon will check the configuration files in order, with the latter overriding the earlier.
They are:
* /etc/loadshedding.conf
* ~/.config/loadshedding

Here is an example config file, with comments to indicate what each line does.
``` config
# example location, important to change this
# the default when not specified on command line
province  = 3
suburb    = 1021456

# how frequently, in minutes, to check the loadshedding schedule
# for the daemon only
frequency = 10

# how long before loadshedding, in minutes, to execute shutdown
# for the daemon only
early = 2

# command to execute for shutdown
# for the daemon only
#shutdown = "sudo shutdown -h now 'Shutting down for loadshedding'"
shutdown = 'touch /tmp/loadshedding.shutdown'

# where to log to
# for the daemon only
logfile = "/tmp/loadshedding.log"
```

Resources
=========
* https://tutorials.techrad.co.za/2020/09/13/eskoms-loadshedding-api/
