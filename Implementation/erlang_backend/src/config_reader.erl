%%--------------------------------------------------------------------
%% @doc
%% Config Reader module for Backend Node.
%%
%% This module reads configuration parameters from a JSON file (using jsx).
%% It provides utility functions to retrieve configuration values.
%%--------------------------------------------------------------------
-module(config_reader).

%% API exports
-export([load_config/0, 
        get_nodes/0]).

-define(CONFIG_FILE, "config.json").

%%--------------------------------------------------------------------
%% @doc
%% Reads the JSON configuration file and returns its contents as a map.
%%
%% If the file cannot be read or parsed, an empty map is returned.
%%--------------------------------------------------------------------
load_config() ->
    case file:read_file(?CONFIG_FILE) of
        {ok, Bin} ->
            %% Decode the JSON into a map.
            try
                jsx:decode(Bin, [return_maps])
            catch
                _:_ ->
                    io:format("Error decoding JSON configuration.~n"),
                    #{}
            end;
        {error, Reason} ->
            io:format("Error reading config file (~s): ~p~n", [?CONFIG_FILE, Reason]),
            #{}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a specific value identified by Key from the JSON configuration.
%%
%% Returns the value if present; otherwise, returns undefined.
%%--------------------------------------------------------------------
get_value(Key) ->
    Config = load_config(),
    maps:get(Key, Config, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of event server nodes from the configuration.
%%
%% The nodes are expected to be provided as a JSON array of strings.
%% This function converts them to atoms for further use in messaging.
%%
%% Returns the list of event server nodes as atoms if present; otherwise, returns an empty list.
get_nodes() ->
    Config = load_config(),
    %% The event server nodes are provided as a list of binaries under the key <<"event_server_nodes">>.
    case maps:get(<<"event_server_nodes">>, Config, undefined) of
        undefined -> [];
        List when is_list(List) ->
            [ binary_to_atom(NodeBin, utf8) || NodeBin <- List ]
    end.