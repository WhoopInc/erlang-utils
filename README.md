## Notice: This repository is no longer maintained as of 3/26/2024


WHOOP Erlang utils
=====

A collection of useful Erlang modules

Include `wutils` in your application list to use it.

Build
-----

    $ rebar3 compile

Contents
=========

Database pool
-------------
Episcina + pgsql for Postgres database pooling.

Include the following in your configuration (`size` and `timeout` may be changed):

```erlang
{episcina,
  [{pools,
    [{primary,
      [{size, 10}
      ,{timeout, 20000}
      ,{connect_provider, {wutils_db, ep_open, []}}
      ,{close_provider, {wutils_db, ep_close, []}}
    ]}
  ]}
]}
```

Connection info comes from the environment variables:
`PGUSER`, `PGPASSWORD`, `PGDATABASE`, `PGHOST`, `PGPORT`

Use the pool like this:
```erlang
PG = wutils_db:open(),
Result = PG:simple_query("SELECT hello FROM world"),
wutils_db:close(PG),
Result.
```

or the shorter form for one-off queryies:
```erlang
Result = wutils_db:run("SELECT hello FROM world WHERE id = $1", [Id]).
```

SQL query repository
---------------------
Read a directory full of SQL queries and store them in memory for later use

Include the following in your configuration:

```erlang
{wutils,
  [{sql_source_applications, [app1, app2]}
]}
```

where `app1` and `app2` are application names. For each item in the list, the application
is expected to have a `priv` directory which will be scanned for `.sql` files. Each file
is loaded as a query.

Listing more than one application will behave as if the directories have been merged. It's
an error to have files with the same name and different content.

Datetime utilities
-------------------
`wutils_time:datetime()` is an extended datetime format: `{{Year, Month, Day}, {Hour, Minute, Second, Millisecond}}`

`epoch_to_datetime/1` and `datetime_to_epoch` convert between the datetime tuple and milliseconds since Unix epoch.

`parse_iso/1` and `format_iso/1` convert between the datetime tuple and the ISO8601 date+time string format. The
parser is not terrifically robust, but will handle JavaScript's `Date.prototype.toISOString()`.
