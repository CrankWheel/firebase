firebase
=====

An OTP application for handling REST queries to Firebase.

Requires configuration via application config via sys.config.

See `priv/sys.config.example`

Provides:
firebase:get(Path)
firebase:get(Path, Opts)

TODO
----
Caching, and supporting other stuff than GET.

Build
-----

    $ ./rebar3 compile
