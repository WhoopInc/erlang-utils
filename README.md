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
