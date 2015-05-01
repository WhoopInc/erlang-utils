%% Lager helpers

-define(DEBUG(Message), DEBUG(Message, [])).
-define(DEBUG(Fmt, Args), lager:debug(Fmt, Args)).

-define(INFO(Message), DEBUG(Message, [])).
-define(INFO(Fmt, Args), lager:info(Fmt, Args)).

-define(WARN(Message), WARN(Message, [])).
-define(WARN(Fmt, Args), lager:warn(Fmt, Args)).
