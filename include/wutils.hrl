%% Lager helpers

-define(DEBUG(Message), ?DEBUG(Message, [])).
-define(DEBUG(Fmt, Args), lager:debug(Fmt, Args)).

-define(INFO(Message), ?INFO(Message, [])).
-define(INFO(Fmt, Args), lager:info(Fmt, Args)).

-define(WARN(Message), ?WARN(Message, [])).
-define(WARN(Fmt, Args), lager:warning(Fmt, Args)).

-define(ERROR(Message), ?ERROR(Message, [])).
-define(ERROR(Fmt, Args), lager:error(Fmt, Args)).
