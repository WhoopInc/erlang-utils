-module(kcl_client).

-type bstring() :: binary().

-type stream_name() :: bstring().
-type shard_id() :: bstring().
-type iterator_type() :: bstring().
-type shard_iterator() :: bstring().
-type sequence_number() :: bstring().

-type records() :: [{Data :: bstring(), PartitionKey :: bstring()}].

-type client_state() :: term().

%% A checkpoint into a stream that gets stored for later use
-opaque stream_pointer() :: sequence_number().

-export_type([
              stream_pointer/0
             ,stream_name/0
             ,shard_id/0
             ]).
-export([
         register/5
        ]).

%% Return a previously-stored pointer out of storage. The pointer is a binary string.
%% If no pointer has been stored, `undefined` must be returned.
-callback read_pointer(StreamName :: stream_name(), ShardId :: shard_id()) ->
    {ok, StoredPointer :: stream_pointer()} | undefined.

%% Store a stream pointer. The pointer is a binary string and is specific to the StreamName x ShardId.
-callback store_pointer(StreamName :: stream_name(), ShardId :: shard_id(), StreamPointer :: stream_pointer()) ->
    ok.

%% Receive data from the stream. Data is a list of record/partition-key tuples. LastPointer
%% is the pointer to the last record in the passed list.
%% Return `ok` to leave checkpoint management up to kcl. Return `{ok, no_checkpoint}` if you
%% will manage calling store_pointer yourself.
%% This function should not return until you are ready to receieve more data.
-callback stream_data(Data :: records(), LastPointer :: stream_pointer()) ->
    ok | {ok, no_checkpoint}.

-spec register(Module :: atom(), Function :: atom(), State :: client_state(), 
               StreamName :: stream_name(), ShardId :: shard_id()) -> ok.
register(Module, Function, State, StreamName, ShardId) ->
    fixme.

