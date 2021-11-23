* Command line utility to query Eskom loadshedding status
* Daemon to power down a Linux server / desktop cleanly before loadshedding happens

Linux build instructions
========================
* Install **stack**, either [directly](https://docs.haskellstack.org/en/stable/README/#how-to-install) or via [ghcup](https://www.haskell.org/ghcup/).
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

Resources
=========
* https://tutorials.techrad.co.za/2020/09/13/eskoms-loadshedding-api/
